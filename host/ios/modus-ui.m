// modus-ui.m — a framebuffer and touch events for a modus image on iOS
// (docs/macos-hosting.md, "Drawing on the phone").
//
// The image speaks Linux syscalls through the shim and has no way to reach
// UIKit, so the shim offers four pseudo-syscalls past every Linux number
// (the same trick as 1000, set-thread-delta), dispatched here:
//
//   1001 UI-INFO k          k=0 width, 1 height (pixels), 2 scale
//   1002 FILL  xy wh rgb    fill [x,x+w)×[y,y+h) with 0xRRGGBB, clipped;
//                           xy = x | y<<16, wh = w | h<<16
//   1003 PRESENT            show the buffer (coalesced on the main thread)
//   1004 NEXT-EVENT         the oldest event: type<<40 | y<<20 | x for a touch,
//                           type 1 down / 2 move / 3 up; type<<40 | keysym for
//                           a key, type 4 (an X11 keysym: Latin-1 as itself,
//                           other Unicode 0x01000000+cp, Return 0xff0d,
//                           BackSpace 0xff08, arrows 0xff51-54, Escape 0xff1b);
//                           0 when there is none
//   1006 KEYBOARD on        1 shows the on-screen keyboard, 0 hides it.  Keys
//                           typed on it -- or on a hardware keyboard while it
//                           is up -- arrive through NEXT-EVENT.
//   1005 BLIT src wh xy [stride]  copy a w×h block of Lisp pixels to (x,y), clipped:
//                           SRC is the address of element 0 of a SIMPLE-VECTOR
//                           of 0xRRGGBB fixnums, w per row -- glass's frame-
//                           buffer as modus stores it, one 8-byte word each
//                           (value<<1).  wh = w | h<<16, xy = x | y<<16.
//                           STRIDE, when given, is the source's row length in
//                           elements -- a block cut from a wider framebuffer
//                           (the part of the screen that changed); 0 means w.
//
// The buffer is the screen at SCALE (2) pixels per point, XRGB 32-bit, drawn
// with nearest-neighbour filtering.  Filling is native, so interpreted Lisp
// only decides WHAT to draw.  Every call waits until UIKit has put the view
// up, because the image thread starts before UIApplicationMain.

#import <UIKit/UIKit.h>
#import <AVFoundation/AVFoundation.h>
#include <os/lock.h>
#include <stdint.h>
#include <string.h>

// The screen's own pixel density (3 on a current iPhone), so one buffer pixel
// is one device pixel; making things large enough to read is the drawing
// side's job (glass's FB-BLIT-SCALED).
static long ui_scale = 2;
#define UI_SCALE ui_scale
#define NEV 512

static uint32_t *fb;
static long fb_w, fb_h;
static dispatch_semaphore_t ui_ready;
static dispatch_once_t ui_once;
static UIView *fb_view;
static os_unfair_lock ev_lock = OS_UNFAIR_LOCK_INIT;
static uint64_t events[NEV];
static unsigned ev_head, ev_tail;
static volatile int present_pending;

static void push_event(long type, CGPoint p) {
    long x = (long)(p.x * UI_SCALE), y = (long)(p.y * UI_SCALE);
    if (x < 0) x = 0; if (y < 0) y = 0;
    if (x >= fb_w) x = fb_w - 1; if (y >= fb_h) y = fb_h - 1;
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = ((uint64_t)type << 40) | ((uint64_t)y << 20) | (uint64_t)x;
    os_unfair_lock_unlock(&ev_lock);
}

static void push_key(uint32_t keysym) {
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = ((uint64_t)4 << 40) | (uint64_t)keysym;
    os_unfair_lock_unlock(&ev_lock);
}

static uint32_t keysym_for(uint32_t cp) {
    if (cp == '\n' || cp == '\r') return 0xff0d;
    if (cp == '\t') return 0xff09;
    if (cp >= 0x20 && cp <= 0xff) return cp;          // Latin-1: the keysym is the character
    return 0x01000000 | cp;                            // the X11 Unicode keysym range
}

// THE KEYBOARD.  The view is a UIKeyInput, so it can be first responder: the on-screen keyboard
// types into it, and so does a hardware one while the view holds focus.  Text is the image's to
// interpret -- no autocorrection, no auto-capitals, the keys as typed.
@interface ModusView : UIView <UIKeyInput>
@property(nonatomic) UITextAutocorrectionType autocorrectionType;
@property(nonatomic) UITextAutocapitalizationType autocapitalizationType;
@property(nonatomic) UITextSpellCheckingType spellCheckingType;
@property(nonatomic) UITextSmartQuotesType smartQuotesType;
@property(nonatomic) UITextSmartDashesType smartDashesType;
@end
@implementation ModusView
- (BOOL)canBecomeFirstResponder { return YES; }
- (BOOL)hasText { return YES; }                        // so Backspace is always offered
- (void)insertText:(NSString *)text {
    [text enumerateSubstringsInRange:NSMakeRange(0, text.length)
                             options:NSStringEnumerationByComposedCharacterSequences
                          usingBlock:^(NSString *ch, NSRange r, NSRange er, BOOL *stop) {
        uint32_t cp = 0;
        [ch getBytes:&cp maxLength:4 usedLength:NULL encoding:NSUTF32LittleEndianStringEncoding
             options:0 range:NSMakeRange(0, ch.length) remainingRange:NULL];
        push_key(keysym_for(cp));
    }];
}
- (void)deleteBackward { push_key(0xff08); }
// A hardware keyboard's arrows and Escape never reach insertText.
- (void)pressesBegan:(NSSet<UIPress *> *)presses withEvent:(UIPressesEvent *)e {
    BOOL rest = NO;
    for (UIPress *p in presses) {
        uint32_t ks = 0;
        switch (p.key.keyCode) {
        case UIKeyboardHIDUsageKeyboardLeftArrow:  ks = 0xff51; break;
        case UIKeyboardHIDUsageKeyboardUpArrow:    ks = 0xff52; break;
        case UIKeyboardHIDUsageKeyboardRightArrow: ks = 0xff53; break;
        case UIKeyboardHIDUsageKeyboardDownArrow:  ks = 0xff54; break;
        case UIKeyboardHIDUsageKeyboardEscape:     ks = 0xff1b; break;
        default: rest = YES;
        }
        if (ks) push_key(ks);
    }
    if (rest) [super pressesBegan:presses withEvent:e];
}
- (void)touches:(NSSet<UITouch *> *)ts type:(long)type {
    for (UITouch *t in ts) push_event(type, [t locationInView:self]);
}
- (void)touchesBegan:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:1]; }
- (void)touchesMoved:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:2]; }
- (void)touchesEnded:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:3]; }
- (void)touchesCancelled:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:3]; }
@end

static UIWindow *window;

// Called by the shim's app delegate, on the main thread, once launching is done.
void modus_ui_start(void) {
    CGRect b = [[UIScreen mainScreen] bounds];
    ui_scale = (long)[[UIScreen mainScreen] scale];
    if (ui_scale < 1) ui_scale = 1;
    fb_w = (long)b.size.width * UI_SCALE;
    fb_h = (long)b.size.height * UI_SCALE;
    fb = calloc((size_t)(fb_w * fb_h), 4);
    window = [[UIWindow alloc] initWithFrame:b];
    UIViewController *vc = [[UIViewController alloc] init];
    ModusView *v = [[ModusView alloc] initWithFrame:b];
    v.multipleTouchEnabled = YES;
    v.backgroundColor = UIColor.blackColor;
    v.layer.magnificationFilter = kCAFilterNearest;
    vc.view = v;
    window.rootViewController = vc;
    [window makeKeyAndVisible];
    fb_view = v;
    dispatch_semaphore_signal(ui_ready);
}

static void wait_ui(void) {
    dispatch_once(&ui_once, ^{
        // Signalled once by modus_ui_start; re-signal so later waiters pass.
        dispatch_semaphore_wait(ui_ready, DISPATCH_TIME_FOREVER);
        dispatch_semaphore_signal(ui_ready);
    });
}

static void present(void) {
    if (__atomic_exchange_n(&present_pending, 1, __ATOMIC_ACQ_REL)) return;
    dispatch_async(dispatch_get_main_queue(), ^{
        __atomic_store_n(&present_pending, 0, __ATOMIC_RELEASE);
        CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
        CGContextRef cx = CGBitmapContextCreate(fb, (size_t)fb_w, (size_t)fb_h, 8, (size_t)fb_w * 4, cs,
                                                kCGImageAlphaNoneSkipFirst | kCGBitmapByteOrder32Little);
        CGImageRef img = CGBitmapContextCreateImage(cx);   // copies the pixels
        fb_view.layer.contents = (__bridge id)img;
        CGImageRelease(img); CGContextRelease(cx); CGColorSpaceRelease(cs);
    });
}

static long fill(long xy, long wh, long rgb) {
    long x = xy & 0xFFFF, y = (xy >> 16) & 0xFFFF, w = wh & 0xFFFF, h = (wh >> 16) & 0xFFFF;
    if (x >= fb_w || y >= fb_h) return 0;
    if (x + w > fb_w) w = fb_w - x;
    if (y + h > fb_h) h = fb_h - y;
    uint32_t c = (uint32_t)rgb & 0xFFFFFF;
    for (long j = 0; j < h; j++) {
        uint32_t *row = fb + (y + j) * fb_w + x;
        for (long i = 0; i < w; i++) row[i] = c;
    }
    return 0;
}

static long blit(long src, long wh, long xy, long stride) {
    long w = wh & 0xFFFF, h = (wh >> 16) & 0xFFFF, x = xy & 0xFFFF, y = (xy >> 16) & 0xFFFF;
    if (x >= fb_w || y >= fb_h) return 0;
    long cw = x + w > fb_w ? fb_w - x : w, ch = y + h > fb_h ? fb_h - y : h;
    const uint64_t *s = (const uint64_t *)(uintptr_t)src;
    for (long j = 0; j < ch; j++) {
        uint32_t *row = fb + (y + j) * fb_w + x;
        const uint64_t *in = s + j * (stride > 0 ? stride : w);
        for (long i = 0; i < cw; i++) row[i] = (uint32_t)(in[i] >> 1) & 0xFFFFFF;
    }
    return 0;
}

long modus_ui_call(long nr, long a0, long a1, long a2, long a3) {
    if (!ui_ready) return -38;
    wait_ui();
    switch (nr) {
    case 1001: return a0 == 0 ? fb_w : a0 == 1 ? fb_h : UI_SCALE;
    case 1002: return fill(a0, a1, a2);
    case 1003: present(); return 0;
    case 1005: return blit(a0, a1, a2, a3);
    case 1006:
        dispatch_async(dispatch_get_main_queue(), ^{
            if (a0) [fb_view becomeFirstResponder]; else [fb_view resignFirstResponder];
        });
        return 0;
    case 1004: {
        uint64_t e = 0;
        os_unfair_lock_lock(&ev_lock);
        if (ev_head != ev_tail) e = events[ev_head++ % NEV];
        os_unfair_lock_unlock(&ev_lock);
        return (long)e;
    }
    }
    return -38;
}

// Before the image thread starts: from then on a UI call waits for the view.
void modus_ui_init(void) { ui_ready = dispatch_semaphore_create(0); }

// The speaker's half that needs UIKit's world (host/macos/modus-audio.c calls it
// when the image opens the output): PLAYBACK, so a media player is heard with
// the ring/silent switch off -- the default category treats an app's sound as
// incidental and mutes it.
void modus_audio_session(void) {
    AVAudioSession *s = [AVAudioSession sharedInstance];
    [s setCategory:AVAudioSessionCategoryPlayback error:nil];
    [s setActive:YES error:nil];
}
