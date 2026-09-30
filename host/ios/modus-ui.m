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
//   1004 NEXT-EVENT         the oldest touch: type<<40 | y<<20 | x, type
//                           1 down / 2 move / 3 up; 0 when there is none
//
// The buffer is the screen at SCALE (2) pixels per point, XRGB 32-bit, drawn
// with nearest-neighbour filtering.  Filling is native, so interpreted Lisp
// only decides WHAT to draw.  Every call waits until UIKit has put the view
// up, because the image thread starts before UIApplicationMain.

#import <UIKit/UIKit.h>
#include <os/lock.h>
#include <stdint.h>
#include <string.h>

#define UI_SCALE 2
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

@interface ModusView : UIView
@end
@implementation ModusView
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

long modus_ui_call(long nr, long a0, long a1, long a2, long a3) {
    (void)a3;
    if (!ui_ready) return -38;
    wait_ui();
    switch (nr) {
    case 1001: return a0 == 0 ? fb_w : a0 == 1 ? fb_h : UI_SCALE;
    case 1002: return fill(a0, a1, a2);
    case 1003: present(); return 0;
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
