// modus-ui.m — a framebuffer and touch events for a modus image on iOS
// (docs/macos-hosting.md, "Drawing on the phone").
//
// The image speaks Linux syscalls through the shim and has no way to reach
// UIKit, so the shim offers four pseudo-syscalls past every Linux number
// (the same trick as 1000, set-thread-delta), dispatched here:
//
//   1001 UI-INFO k          k=0 width, 1 height (pixels), 2 scale; 3..6 the safe area's
//                           insets in pixels: top, left, bottom, right (the status bar, the
//                           Dynamic Island's side in landscape, the home indicator)
//   1002 FILL  xy wh rgb    fill [x,x+w)×[y,y+h) with 0xRRGGBB, clipped;
//                           xy = x | y<<16, wh = w | h<<16
//   1003 PRESENT            show the buffer (coalesced on the main thread)
//   1008 WAIT-FRAME ms      block until the display's next refresh (a CADisplayLink
//                           tick), or MS milliseconds at most; 0 = ticked, 1 = timed out.
//                           A frame loop paced by this draws once per refresh, in step
//                           with the screen, instead of guessing with a sleep.
//   1004 NEXT-EVENT         the oldest event: type<<40 | y<<20 | x for a touch,
//                           type 1 down / 2 move / 3 up; type<<40 | keysym for
//                           a key, type 4 (an X11 keysym: Latin-1 as itself,
//                           other Unicode 0x01000000+cp, Return 0xff0d,
//                           BackSpace 0xff08, arrows 0xff51-54, Escape 0xff1b);
//                           0 when there is none.  Type 6 is a RESIZE: the phone
//                           turned, and the screen is now x by y pixels (1001 says so
//                           too from here on); the image lays itself out again.
//                           Type 7 is the KEYBOARD: x is how many pixels at the
//                           bottom of the screen the on-screen keyboard now covers
//                           (0: it went away, or a hardware keyboard is in use).
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
// with nearest-neighbour filtering.  IT IS AN IOSURFACE, the view's layer's contents:
// FILL and BLIT write into it and PRESENT only tells Core Animation it changed.  It used
// to be a malloc'd buffer copied into a fresh CGImage on every present -- the whole
// screen, 12.6 MB on an iPhone 15 Pro, on the main thread, for a change of a few rows.  Filling is native, so interpreted Lisp
// only decides WHAT to draw.  Every call waits until UIKit has put the view
// up, because the image thread starts before UIApplicationMain.

#import <UIKit/UIKit.h>
#import <AVFoundation/AVFoundation.h>
#import <IOSurface/IOSurfaceRef.h>
#import <QuartzCore/QuartzCore.h>
#include <sys/stat.h>
#include <os/lock.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// The screen's own pixel density (3 on a current iPhone), so one buffer pixel
// is one device pixel; making things large enough to read is the drawing
// side's job (glass's FB-BLIT-SCALED).
static long ui_scale = 2;
#define UI_SCALE ui_scale
#define NEV 512

static uint32_t *fb;
static long fb_w, fb_h;             // the screen as it is now: turning the phone swaps them
static long fb_cap;                // the surface's side: square, the screen's longer side, so a
                                   // turn changes only which part of it is shown -- never the
                                   // memory a blit on the image's thread is writing into
static long safe_top, safe_left, safe_bottom, safe_right;   // the safe area, in pixels
static long fb_stride;             // the surface's row length in pixels (rows are padded)
static IOSurfaceRef fb_surface;
static dispatch_semaphore_t frame_tick;   // signalled by the display link once a refresh
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

// A FINGER'S OWN TOUCH (type 14), beside the 1/2/3 the desk reads: which finger (a slot 0..9, held from the touch's
// start to its end) in bits 48-51, the phase (0 began, 1 moved, 2 ended or cancelled) in bits 52-53, x and y as a
// pointer's.  A game's two thumbs -- a stick each -- are told apart by it; the desk ignores it.
static __unsafe_unretained UITouch *fingers[10];

static void push_finger(UITouch *t, long type, CGPoint p) {
    long slot = -1;
    for (long i = 0; i < 10; i++) if (fingers[i] == t) { slot = i; break; }
    if (slot < 0 && type == 1)
        for (long i = 0; i < 10; i++) if (!fingers[i]) { fingers[i] = t; slot = i; break; }
    if (slot < 0) return;                          // an eleventh finger, or one never seen begin
    long phase = type == 1 ? 0 : type == 2 ? 1 : 2;
    if (phase == 2) fingers[slot] = nil;
    long x = (long)(p.x * UI_SCALE), y = (long)(p.y * UI_SCALE);
    if (x < 0) x = 0; if (y < 0) y = 0;
    if (x >= fb_w) x = fb_w - 1; if (y >= fb_h) y = fb_h - 1;
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = ((uint64_t)phase << 52) | ((uint64_t)slot << 48) | ((uint64_t)14 << 40)
                                | ((uint64_t)y << 20) | (uint64_t)x;
    os_unfair_lock_unlock(&ev_lock);
}

static void push_resize(long w, long h) {
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = ((uint64_t)6 << 40) | ((uint64_t)h << 20) | (uint64_t)w;
    os_unfair_lock_unlock(&ev_lock);
}

static __attribute__((unused)) void push_keyboard(long px) {
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = ((uint64_t)7 << 40) | (uint64_t)(px < 0 ? 0 : px);
    os_unfair_lock_unlock(&ev_lock);
}

static void push_key_as(long type, uint32_t keysym) {   // 4 a press and release, 8 a press, 9 a release
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = ((uint64_t)type << 40) | (uint64_t)keysym;
    os_unfair_lock_unlock(&ev_lock);
}
static void push_key(uint32_t keysym) { push_key_as(4, keysym); }

static uint32_t keysym_for(uint32_t cp) {
    if (cp == '\n' || cp == '\r') return 0xff0d;
    if (cp == '\t') return 0xff09;
    if (cp >= 0x20 && cp <= 0xff) return cp;          // Latin-1: the keysym is the character
    return 0x01000000 | cp;                            // the X11 Unicode keysym range
}

// THE KEYBOARD.  The view is a UIKeyInput, so it can be first responder: the on-screen keyboard
// types into it, and so does a hardware one while the view holds focus.  Text is the image's to
// interpret -- no autocorrection, no auto-capitals, the keys as typed.
#if TARGET_OS_TV
@interface ModusView : UIView <UIKeyInput>          // a TV has no clipboard and no edit menu
#else
@interface ModusView : UIView <UIKeyInput, UIEditMenuInteractionDelegate>
@property(nonatomic, strong) UIEditMenuInteraction *editMenu;   // the paste menu (two-finger press)
#endif
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

#if !TARGET_OS_TV
// PASTE.  The clipboard's text goes in as keys, as typing would: keysym_for makes a newline Return
// and a tab Tab.  Cmd-V on a hardware keyboard, a Paste in the menu that a two-finger press
// brings up (one finger stays the pointer), and the system's own paste: all come here.
- (instancetype)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        UILongPressGestureRecognizer *press = [[UILongPressGestureRecognizer alloc]
            initWithTarget:self action:@selector(twoFingerPress:)];
        press.numberOfTouchesRequired = 2;
        [self addGestureRecognizer:press];
        self.editMenu = [[UIEditMenuInteraction alloc] initWithDelegate:self];
        [self addInteraction:self.editMenu];
    }
    return self;
}
- (void)paste:(id)sender {
    NSString *s = [UIPasteboard generalPasteboard].string;
    if (s.length) [self insertText:s];
}
- (BOOL)canPerformAction:(SEL)action withSender:(id)sender {
    if (action == @selector(paste:)) return [UIPasteboard generalPasteboard].hasStrings;
    return [super canPerformAction:action withSender:sender];
}
- (NSArray<UIKeyCommand *> *)keyCommands {
    return @[ [UIKeyCommand keyCommandWithInput:@"v" modifierFlags:UIKeyModifierCommand
                                         action:@selector(paste:)] ];
}
- (void)twoFingerPress:(UILongPressGestureRecognizer *)g {
    if (g.state != UIGestureRecognizerStateBegan) return;
    UIEditMenuConfiguration *c = [UIEditMenuConfiguration configurationWithIdentifier:nil
                                                                          sourcePoint:[g locationInView:self]];
    [self.editMenu presentEditMenuWithConfiguration:c];
}
- (UIMenu *)editMenuInteraction:(UIEditMenuInteraction *)interaction
          menuForConfiguration:(UIEditMenuConfiguration *)configuration
              suggestedActions:(NSArray<UIMenuElement *> *)suggestedActions {
    if (![UIPasteboard generalPasteboard].hasStrings) return nil;
    UIAction *paste = [UIAction actionWithTitle:@"Paste" image:nil identifier:nil
                                        handler:^(__kindof UIAction *a) { [self paste:nil]; }];
    return [UIMenu menuWithChildren:@[ paste ]];
}
#endif
#if TARGET_OS_TV
// THE REMOTE.  A TV's input is presses, not touches: its arrows (the clickpad's edges) are the arrow keys, Back
// (Menu) is Escape and Play/Pause is Space -- what the desk does with a keyboard.  The view is focusable so the
// presses come to it.  Back is the desk's from here on: Home (the TV button) leaves the app.
//
// THE POINTER.  A thumb on the touch surface moves a cursor drawn over the screen, as a laptop's trackpad does: by
// how far it slides, not where it lands, so a short stroke repeated crosses the screen.  Select is its button -- a
// press where it points (type 1), a drag while held (2), a release (3) -- and with nothing held its moves are a
// hover (5), as a mouse's are.  It fades after a few still seconds and comes back where it was.
- (BOOL)canBecomeFocused { return YES; }
static uint32_t remote_keysym(UIPress *p) {
    switch (p.type) {
    case UIPressTypeLeftArrow:  return 0xff51;
    case UIPressTypeUpArrow:    return 0xff52;
    case UIPressTypeRightArrow: return 0xff53;
    case UIPressTypeDownArrow:  return 0xff54;
    case UIPressTypeMenu:       return 0xff1b;
    case UIPressTypePlayPause:  return ' ';
    default:                    return 0;
    }
}
#define POINTER_GAIN 1.5                              // screen points a point of the surface's slide moves
static CGPoint cursor_at;
static CAShapeLayer *cursor_arrow;
static BOOL select_down;
static NSUInteger cursor_moves;                       // which move a fade belongs to: a later one cancels it

static CAShapeLayer *make_arrow(void) {
    UIBezierPath *a = [UIBezierPath bezierPath];      // a pointer's arrow, its tip at (0,0), 32 points tall
    [a moveToPoint:CGPointMake(0, 0)];   [a addLineToPoint:CGPointMake(0, 28)];
    [a addLineToPoint:CGPointMake(7, 21)]; [a addLineToPoint:CGPointMake(12, 32)];
    [a addLineToPoint:CGPointMake(17, 30)]; [a addLineToPoint:CGPointMake(12, 19)];
    [a addLineToPoint:CGPointMake(21, 19)]; [a closePath];
    CAShapeLayer *l = [CAShapeLayer layer];
    l.path = a.CGPath;
    l.fillColor = UIColor.whiteColor.CGColor;
    l.strokeColor = UIColor.blackColor.CGColor;
    l.lineWidth = 2;
    l.lineJoin = kCALineJoinRound;
    l.shadowOpacity = 0.4; l.shadowRadius = 3; l.shadowOffset = CGSizeMake(1, 2);
    l.opacity = 0;
    return l;
}
- (void)showCursor {
    if (!cursor_arrow) {
        cursor_arrow = make_arrow();
        cursor_at = CGPointMake(CGRectGetMidX(self.bounds), CGRectGetMidY(self.bounds));
        [self.layer addSublayer:cursor_arrow];
    }
    [CATransaction begin]; [CATransaction setDisableActions:YES];
    cursor_arrow.position = cursor_at;
    [CATransaction commit];
    cursor_arrow.opacity = 1;                         // implicit: the fade back in is animated
    NSUInteger n = ++cursor_moves;
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 4 * NSEC_PER_SEC), dispatch_get_main_queue(), ^{
        if (n == cursor_moves && !select_down) cursor_arrow.opacity = 0;
    });
}
- (void)slide:(UITouch *)t {
    CGPoint p = [t locationInView:self], q = [t previousLocationInView:self];
    CGRect b = self.bounds;
    cursor_at.x = fmin(fmax(cursor_at.x + (p.x - q.x) * POINTER_GAIN, 0), CGRectGetMaxX(b) - 1);
    cursor_at.y = fmin(fmax(cursor_at.y + (p.y - q.y) * POINTER_GAIN, 0), CGRectGetMaxY(b) - 1);
    [self showCursor];
    push_event(select_down ? 2 : 5, cursor_at);
}
- (void)remoteSelect:(BOOL)down {
    if (down == select_down) return;
    select_down = down;
    [self showCursor];
    push_event(down ? 1 : 3, cursor_at);
}
#endif
// HELD KEYS.  A hardware keyboard's arrows and Escape (which never reach insertText) and the remote's buttons come
// as a press (type 8) and its release (type 9), so what is held is known -- a game turns while an arrow is down.
// Typed text stays type 4, a press and release at once.
static uint32_t press_keysym(UIPress *p) {
#if TARGET_OS_TV
    if (!p.key) return remote_keysym(p);
#endif
    switch (p.key.keyCode) {
    case UIKeyboardHIDUsageKeyboardLeftArrow:  return 0xff51;
    case UIKeyboardHIDUsageKeyboardUpArrow:    return 0xff52;
    case UIKeyboardHIDUsageKeyboardRightArrow: return 0xff53;
    case UIKeyboardHIDUsageKeyboardDownArrow:  return 0xff54;
    case UIKeyboardHIDUsageKeyboardEscape:     return 0xff1b;
    default:                                   return 0;
    }
}
- (void)pressesBegan:(NSSet<UIPress *> *)presses withEvent:(UIPressesEvent *)e {
    NSMutableSet<UIPress *> *rest = [NSMutableSet set];
    for (UIPress *p in presses) {
        uint32_t ks;
#if TARGET_OS_TV
        if (!p.key && p.type == UIPressTypeSelect) { [self remoteSelect:YES]; continue; }
#endif
        if ((ks = press_keysym(p))) push_key_as(8, ks); else [rest addObject:p];
    }
    if (rest.count) [super pressesBegan:rest withEvent:e];
}
// The presses the view took are not passed on: a TV's Back that reaches the application leaves the app.
- (NSSet<UIPress *> *)pressesLetGo:(NSSet<UIPress *> *)presses {
    NSMutableSet<UIPress *> *rest = [NSMutableSet set];
    for (UIPress *p in presses) {
        uint32_t ks;
#if TARGET_OS_TV
        if (!p.key && p.type == UIPressTypeSelect) { [self remoteSelect:NO]; continue; }
#endif
        if ((ks = press_keysym(p))) push_key_as(9, ks); else [rest addObject:p];
    }
    return rest;
}
- (void)pressesEnded:(NSSet<UIPress *> *)presses withEvent:(UIPressesEvent *)e {
    NSSet *rest = [self pressesLetGo:presses];
    if (rest.count) [super pressesEnded:rest withEvent:e];
}
- (void)pressesCancelled:(NSSet<UIPress *> *)presses withEvent:(UIPressesEvent *)e {
    NSSet *rest = [self pressesLetGo:presses];
    if (rest.count) [super pressesCancelled:rest withEvent:e];
}
- (void)touches:(NSSet<UITouch *> *)ts type:(long)type {
    for (UITouch *t in ts) {
#if TARGET_OS_TV
        if (t.type == UITouchTypeIndirect) {            // the remote's touch surface: the pointer, by how far it slides
            if (type == 2) [self slide:t];
            continue;
        }
#endif
        CGPoint p = [t locationInView:self];
        push_event(type, p);
        push_finger(t, type, p);
    }
}
- (void)touchesBegan:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:1]; }
- (void)touchesMoved:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:2]; }
- (void)touchesEnded:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:3]; }
- (void)touchesCancelled:(NSSet<UITouch *> *)t withEvent:(UIEvent *)e { [self touches:t type:3]; }
@end

static UIWindow *window;

// THE SHOWN PART of the square surface: its top-left FB_W x FB_H, through the layer's contentsRect
// over the whole view.  (Pinning it with a contents gravity at the screen's scale was tried: with an
// IOSurface as the contents, the Simulator showed a different part of the surface than any gravity
// predicts -- measured with the surface's pixels logged against a screenshot.)
static CGRect shown_part(void) {
    return CGRectMake(0, 0, (CGFloat)fb_w / fb_cap, (CGFloat)fb_h / fb_cap);
}
static void show_part(void) {
    if (!fb_surface || fb_cap <= 0) return;
    [CATransaction begin]; [CATransaction setDisableActions:YES];
    fb_view.layer.contentsRect = shown_part();
    [CATransaction commit];
}

static void note_safe_area(UIView *v) {
    UIEdgeInsets in = v.safeAreaInsets;
    safe_top = (long)(in.top * UI_SCALE); safe_left = (long)(in.left * UI_SCALE);
    safe_bottom = (long)(in.bottom * UI_SCALE); safe_right = (long)(in.right * UI_SCALE);
}

// TURNING THE PHONE.  Portrait and both landscapes (the Info.plist allows them); upside down is
// left out, as iPhone apps do.  The new size is the screen's from then on, and the image hears it
// as a RESIZE event; until it repaints, the old picture is shown in the new shape.
@interface ModusViewController : UIViewController
@end
@implementation ModusViewController
- (UIInterfaceOrientationMask)supportedInterfaceOrientations { return UIInterfaceOrientationMaskAllButUpsideDown; }
- (void)viewSafeAreaInsetsDidChange { [super viewSafeAreaInsetsDidChange]; note_safe_area(self.view); }
- (void)viewWillTransitionToSize:(CGSize)size withTransitionCoordinator:(id<UIViewControllerTransitionCoordinator>)co {
    [super viewWillTransitionToSize:size withTransitionCoordinator:co];
    // THE NEW SIZE NOW, at the start of the turn, so the image lays the new screen out while the
    // turn animates rather than after it; the safe area is the new shape's once it has settled.
    long w = (long)(size.width * UI_SCALE), h = (long)(size.height * UI_SCALE);
    if (w > fb_cap) w = fb_cap;
    if (h > fb_cap) h = fb_cap;
    // THE SHOWN PART TURNS WITH THE VIEW: animated from the old part to the new one over the turn's
    // own duration, so the picture's proportions change as the view's bounds do and it is not
    // stretched from one shape into the other (the first cut swapped it at the end: a smear).
    CGRect from = fb_view.layer.contentsRect;
    fb_w = w; fb_h = h;
    if (fb_surface && fb_cap > 0) {
        CABasicAnimation *an = [CABasicAnimation animationWithKeyPath:@"contentsRect"];
        an.fromValue = [NSValue valueWithCGRect:from];
        an.toValue = [NSValue valueWithCGRect:shown_part()];
        an.duration = co.transitionDuration > 0 ? co.transitionDuration : 0.3;
        an.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];
        [CATransaction begin]; [CATransaction setDisableActions:YES];
        fb_view.layer.contentsRect = shown_part();
        [CATransaction commit];
        [fb_view.layer addAnimation:an forKey:@"turn"];
    }
    push_resize(w, h);
    // the view follows its window through the turn (the root view's frame is its window's bounds)
    [co animateAlongsideTransition:^(id<UIViewControllerTransitionCoordinatorContext> ctx) {
        (void)ctx;
        if (self.view.window) self.view.frame = self.view.window.bounds;
    } completion:^(id<UIViewControllerTransitionCoordinatorContext> ctx) {
        (void)ctx;
        if (self.view.window) self.view.frame = self.view.window.bounds;
        long t = safe_top, l = safe_left, b = safe_bottom, r = safe_right;
        note_safe_area(self.view);
        // the insets of the new shape: lay out again only if they are not the ones it was laid out with
        if (t != safe_top || l != safe_left || b != safe_bottom || r != safe_right) push_resize(fb_w, fb_h);
    }];
}
@end

static CADisplayLink *frame_link;
static volatile int frame_banked;
@interface ModusFrameTicker : NSObject
- (void)tick:(CADisplayLink *)link;
@end
@implementation ModusFrameTicker
- (void)tick:(CADisplayLink *)link {
    (void)link;
    if (!__atomic_exchange_n(&frame_banked, 1, __ATOMIC_ACQ_REL)) dispatch_semaphore_signal(frame_tick);
}
@end

static long wait_frame(long ms) {
    if (!frame_tick) return -38;
    long r = dispatch_semaphore_wait(frame_tick, dispatch_time(DISPATCH_TIME_NOW, (int64_t)(ms > 0 ? ms : 1) * 1000000));
    __atomic_store_n(&frame_banked, 0, __ATOMIC_RELEASE);
    return r ? 1 : 0;
}

#if TARGET_OS_TV
// WHAT A TV KEEPS.  A TV app's files are all in Library/Caches (the shim moves HOME there: it may not write
// Documents), and the system empties Caches when it wants the room -- between launches, as a rule.  What stays is
// the app's defaults, 500 KB of them while it is installed.  So what the image keeps as state -- the dot-entries
// at the top of HOME (.glass-seats, .glass/, .operandi/ ...) and the small files at the top of Documents (Media's
// places) -- is copied into the defaults whenever the app goes to the background and every few minutes besides,
// and put back at launch where Caches lost them.  Media (Documents' folders), logs and the system's own caches
// (the rest of Caches) are not kept: they are big, and a lost one is fetched or found again.
#define KEEP_KEY @"modus.home"
#define KEEP_FILE_MAX (64 * 1024)
#define KEEP_TOTAL_MAX (400 * 1024)
static dispatch_queue_t keep_queue;
static dispatch_source_t keep_timer;   // held for the app's life

static NSString *keep_home(void) {
    const char *h = getenv("HOME");
    return h ? [NSString stringWithUTF8String:h] : nil;
}
// Before the image starts (the shim's main, once HOME is Caches): each kept file that is not there now.
void modus_keep_restore(void) {
    @autoreleasepool {
        NSString *home = keep_home();
        NSDictionary *kept = [[NSUserDefaults standardUserDefaults] dictionaryForKey:KEEP_KEY];
        if (!home || !kept) return;
        NSFileManager *fm = NSFileManager.defaultManager;
        long back = 0;
        for (NSString *rel in kept) {
            NSData *d = kept[rel];
            NSString *p = [home stringByAppendingPathComponent:rel];
            if (![d isKindOfClass:NSData.class] || [rel containsString:@".."] || [fm fileExistsAtPath:p]) continue;
            [fm createDirectoryAtPath:p.stringByDeletingLastPathComponent withIntermediateDirectories:YES
                           attributes:nil error:NULL];
            if ([d writeToFile:p atomically:YES]) {
                chmod(p.fileSystemRepresentation, 0600);      // some are secrets: kept as the strictest of them
                back++;
            }
        }
        if (back) NSLog(@"modus keep: %ld file(s) put back in %@", back, home);
    }
}
static void keep_save(void) {
    @autoreleasepool {
        NSString *home = keep_home();
        if (!home) return;
        NSDirectoryEnumerator *en = [NSFileManager.defaultManager enumeratorAtPath:home];
        NSMutableDictionary *keep = [NSMutableDictionary dictionary];
        NSUInteger total = 0, left = 0;
        for (NSString *rel in en) {
            NSDictionary *a = en.fileAttributes;
            BOOL dot = [rel hasPrefix:@"."], docs = [rel hasPrefix:@"Documents/"];
            if ([a.fileType isEqual:NSFileTypeDirectory]) {
                if (!dot && !(en.level == 1 && [rel isEqual:@"Documents"])) [en skipDescendants];
                continue;
            }
            if (![a.fileType isEqual:NSFileTypeRegular] || !(dot || (docs && en.level == 2))
                || [rel hasSuffix:@".log"]) continue;
            unsigned long long n = a.fileSize;
            if (n > KEEP_FILE_MAX || total + n > KEEP_TOTAL_MAX) { left++; continue; }
            NSData *d = [NSData dataWithContentsOfFile:[home stringByAppendingPathComponent:rel]];
            if (!d) continue;
            keep[rel] = d;
            total += d.length;
        }
        NSUserDefaults *u = [NSUserDefaults standardUserDefaults];
        if (![[u dictionaryForKey:KEEP_KEY] isEqual:keep]) [u setObject:keep forKey:KEEP_KEY];
        static NSUInteger said;
        if (left && left != said) NSLog(@"modus keep: %lu file(s) too big to keep", (unsigned long)left);
        said = left;
    }
}
static void keep_start(void) {
    keep_queue = dispatch_queue_create("modus.keep", DISPATCH_QUEUE_SERIAL);
    for (NSNotificationName n in @[ UISceneDidEnterBackgroundNotification, UIApplicationWillTerminateNotification ])
        [[NSNotificationCenter defaultCenter] addObserverForName:n object:nil queue:nil
                                                      usingBlock:^(NSNotification *note) {
            (void)note; dispatch_sync(keep_queue, ^{ keep_save(); });   // before the app is suspended
        }];
    dispatch_source_t t = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, keep_queue);
    dispatch_source_set_timer(t, dispatch_time(DISPATCH_TIME_NOW, 60 * NSEC_PER_SEC), 180 * NSEC_PER_SEC,
                              10 * NSEC_PER_SEC);
    dispatch_source_set_event_handler(t, ^{ keep_save(); });
    dispatch_resume(t);
    keep_timer = t;
}
#endif

// Called by the shim's scene delegate, on the main thread, when UIKit connects the
// scene.  iOS 27 traps an app that runs on the legacy app-delegate lifecycle, so the
// window belongs to a UIWindowScene and not to the application.
void modus_ui_start(void *scene) {
    CGRect b = [[UIScreen mainScreen] bounds];
    ui_scale = (long)[[UIScreen mainScreen] scale];
    if (ui_scale < 1) ui_scale = 1;
    fb_w = (long)b.size.width * UI_SCALE;
    fb_h = (long)b.size.height * UI_SCALE;
    fb_cap = fb_w > fb_h ? fb_w : fb_h;
    {
        NSDictionary *props = @{ (id)kIOSurfaceWidth: @(fb_cap), (id)kIOSurfaceHeight: @(fb_cap),
                                 (id)kIOSurfaceBytesPerElement: @4,
                                 (id)kIOSurfacePixelFormat: @((uint32_t)'BGRA') };
        fb_surface = IOSurfaceCreate((__bridge CFDictionaryRef)props);
    }
    if (fb_surface) {
        fb = (uint32_t *)IOSurfaceGetBaseAddress(fb_surface);
        fb_stride = (long)IOSurfaceGetBytesPerRow(fb_surface) / 4;
        memset(fb, 0, IOSurfaceGetAllocSize(fb_surface));
    } else {
        fb = calloc((size_t)(fb_cap * fb_cap), 4);
        fb_stride = fb_cap;
    }
    window = [[UIWindow alloc] initWithWindowScene:(__bridge UIWindowScene *)scene];
    // NO window.frame: a scene's window is sized by the scene, and set by hand it kept the portrait
    // frame when the phone turned (measured: a landscape screen showing a 402-point-wide view).
    UIViewController *vc = [[ModusViewController alloc] init];
    ModusView *v = [[ModusView alloc] initWithFrame:b];
#if !TARGET_OS_TV
    v.multipleTouchEnabled = YES;
#endif
    v.backgroundColor = UIColor.blackColor;
    v.layer.magnificationFilter = kCAFilterNearest;
    if (fb_surface) v.layer.contents = (__bridge id)fb_surface;
    v.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
    vc.view = v;
    window.rootViewController = vc;
    [window makeKeyAndVisible];
    fb_view = v;
    show_part();
    note_safe_area(v);
#if TARGET_OS_TV
    keep_start();
#endif
#if !TARGET_OS_TV   // a TV's keyboard is a screen of its own, and a TV does not turn
    // THE KEYBOARD'S FRAME, as it comes and goes (and when it changes size: emoji, a different
    // language): how much of the view it covers, in pixels, as a type-7 event.  The image lays
    // its windows out in what is left.
    [[NSNotificationCenter defaultCenter]
        addObserverForName:UIKeyboardWillChangeFrameNotification object:nil queue:[NSOperationQueue mainQueue]
                usingBlock:^(NSNotification *n) {
        CGRect kb = [n.userInfo[UIKeyboardFrameEndUserInfoKey] CGRectValue];
        CGRect in = [fb_view convertRect:kb fromCoordinateSpace:UIScreen.mainScreen.coordinateSpace];
        CGFloat cover = CGRectGetMaxY(fb_view.bounds) - CGRectGetMinY(in);
        if (CGRectIsEmpty(CGRectIntersection(in, fb_view.bounds)) || cover < 0) cover = 0;
        push_keyboard((long)(cover * UI_SCALE));
    }];
    [[NSNotificationCenter defaultCenter]
        addObserverForName:UIKeyboardWillHideNotification object:nil queue:[NSOperationQueue mainQueue]
                usingBlock:^(NSNotification *n) { (void)n; push_keyboard(0); }];
    // MODUS_UI_TURN=SECONDS: turn to landscape that long after start (and back as long again) --
    // what turning the phone does, for a Simulator nobody can turn (simctl has no rotate).
    {
        const char *turn = getenv("MODUS_UI_TURN");
        double secs = turn ? atof(turn) : 0;
        if (secs > 0) {
            UIWindowScene *ws = (__bridge UIWindowScene *)scene;
            dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(secs * 1e9)), dispatch_get_main_queue(), ^{
                [ws requestGeometryUpdateWithPreferences:
                        [[UIWindowSceneGeometryPreferencesIOS alloc] initWithInterfaceOrientations:UIInterfaceOrientationMaskLandscapeRight]
                                            errorHandler:nil];
                dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(secs * 1e9)), dispatch_get_main_queue(), ^{
                    [ws requestGeometryUpdateWithPreferences:
                            [[UIWindowSceneGeometryPreferencesIOS alloc] initWithInterfaceOrientations:UIInterfaceOrientationMaskPortrait]
                                                errorHandler:nil];
                });
            });
        }
    }
#endif
    // THE FRAME CLOCK: a display link signals FRAME_TICK once a refresh (60 or 120 Hz), and
    // WAIT-FRAME waits on it.  At most one tick is banked, so a slow frame does not let the
    // next ones run back to back.
    frame_tick = dispatch_semaphore_create(0);
    frame_link = [CADisplayLink displayLinkWithTarget:[ModusFrameTicker new] selector:@selector(tick:)];
    [frame_link addToRunLoop:[NSRunLoop mainRunLoop] forMode:NSRunLoopCommonModes];
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
        if (fb_surface) {
            // CALayer's -setContentsChanged: re-read the surface it already shows.  Not in the iOS SDK's
            // headers (it is in macOS's), so asked for by name; a layer without it is given the surface
            // again, which Core Animation also takes as new contents.
            CALayer *l = fb_view.layer;
            SEL sel = NSSelectorFromString(@"setContentsChanged");
            if ([l respondsToSelector:sel]) {
                ((void (*)(id, SEL))[l methodForSelector:sel])(l, sel);
            } else {
                [CATransaction begin]; [CATransaction setDisableActions:YES];
                l.contents = nil; l.contents = (__bridge id)fb_surface;
                [CATransaction commit];
            }
            return;
        }
        CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
        CGContextRef cx = CGBitmapContextCreate(fb, (size_t)fb_w, (size_t)fb_h, 8, (size_t)fb_stride * 4, cs,
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
        uint32_t *row = fb + (y + j) * fb_stride + x;
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
        uint32_t *row = fb + (y + j) * fb_stride + x;
        const uint64_t *in = s + j * (stride > 0 ? stride : w);
        for (long i = 0; i < cw; i++) row[i] = (uint32_t)(in[i] >> 1) & 0xFFFFFF;
    }
    return 0;
}

int64_t modus_ui_call(int64_t nr, int64_t a0, int64_t a1, int64_t a2, int64_t a3) {   // the shim's words
    if (!ui_ready) return -38;
    wait_ui();
    switch (nr) {
    case 1001: switch (a0) { case 0: return fb_w; case 1: return fb_h; case 2: return UI_SCALE;
                             case 3: return safe_top; case 4: return safe_left;
                             case 5: return safe_bottom; case 6: return safe_right; default: return -22; }
    case 1002: { if (fb_surface) IOSurfaceLock(fb_surface, 0, NULL);
                 long r = fill(a0, a1, a2);
                 if (fb_surface) IOSurfaceUnlock(fb_surface, 0, NULL);
                 return r; }
    case 1003: present(); return 0;
    case 1005: { if (fb_surface) IOSurfaceLock(fb_surface, 0, NULL);
                 long r = blit(a0, a1, a2, a3);
                 if (fb_surface) IOSurfaceUnlock(fb_surface, 0, NULL);
                 return r; }
    case 1008: return wait_frame(a0);
    case 1007: {   // copy: the UTF-8 bytes at a0, a1 long, onto the pasteboard
        NSString *s = [[NSString alloc] initWithBytes:(const void *)a0 length:(NSUInteger)a1
                                             encoding:NSUTF8StringEncoding];
        if (!s) return -22;
#if TARGET_OS_TV
        return -38;                                    // a TV has no pasteboard: ENOSYS
#else
        dispatch_async(dispatch_get_main_queue(), ^{ [UIPasteboard generalPasteboard].string = s; });
        return 0;
#endif
    }
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
// incidental and mutes it.  MIXING WITH OTHERS: the output opens when the desk
// starts, and a session that does not mix takes the phone's audio from whatever
// was playing (music in the AirPods stopped, or moved) just for the app opening.
void modus_audio_session(void) {
    AVAudioSession *s = [AVAudioSession sharedInstance];
    [s setCategory:AVAudioSessionCategoryPlayback
       withOptions:AVAudioSessionCategoryOptionMixWithOthers error:nil];
    [s setActive:YES error:nil];
}
