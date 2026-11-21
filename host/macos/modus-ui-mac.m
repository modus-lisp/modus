// modus-ui-mac.m — a native macOS window, framebuffer, keyboard and mouse for a modus image.
// The AppKit counterpart of host/ios/modus-ui.m: the SAME pseudo-syscalls (1001-1008, documented
// there), so an image that draws on the phone draws in a Mac window with no change.  Differences:
//
//   - AppKit owns the main thread (modus_mac_main runs the run loop); the image runs on a thread of
//     its own, as on iOS, and every UI call from it waits until the window is up.
//   - The mouse, not touches: button 1 is a press (type 1), a drag (2) and a release (3).  Hover is
//     not sent -- the image has no hover, and a move stream would flood the event queue.
//   - The keyboard is the hardware one.  Command-modified keys go to the menu (Quit, Hide, ...) and
//     are NOT sent to the image; everything else is, as a keysym (see keysym_for).
//   - There is no on-screen keyboard, so KEYBOARD (1006) only moves focus, and no type-7 events come.
//   - The frame clock is a CVDisplayLink, not a CADisplayLink (macOS has no UIKit).
//
// Only built with MODUS_MAC_GUI (build-macos.sh --gui).  The CLI runner does not link AppKit.

#import <AppKit/AppKit.h>
#import <IOSurface/IOSurfaceRef.h>
#import <QuartzCore/QuartzCore.h>
#include <os/lock.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NEV 512
#define WANT_W 1280          // the window's starting size, in points (kiln's default screen)
#define WANT_H 800

static long ui_scale = 2;            // pixels per point: the backing scale of the window's screen
static uint32_t *fb;                 // the surface's base address (BGRA, one word a pixel)
static long fb_w, fb_h;              // the window's content, in pixels, as it is now
static long fb_cap;                  // the surface's side: square, large enough for every screen
static long fb_stride;               // the surface's row length in pixels (rows are padded)
static IOSurfaceRef fb_surface;
static dispatch_semaphore_t frame_tick;   // signalled by the display link once a refresh
static dispatch_semaphore_t ui_ready;
static dispatch_once_t ui_once;
static NSWindow *window;
static NSView *fb_view;
static os_unfair_lock ev_lock = OS_UNFAIR_LOCK_INIT;
static uint64_t events[NEV];
static unsigned ev_head, ev_tail;
static volatile int present_pending;
static volatile int frame_banked;
static CADisplayLink *display_link;

// --- the event queue -------------------------------------------------------------------------
// Same encoding as the iOS host: type<<40 | y<<20 | x for a pointer, type<<40 | keysym for a key,
// type<<40 | h<<20 | w for a resize (type 6).  Coordinates are pixels, origin top-left.

static void queue_event(uint64_t e) {
    os_unfair_lock_lock(&ev_lock);
    if (ev_tail - ev_head < NEV) events[ev_tail++ % NEV] = e;
    os_unfair_lock_unlock(&ev_lock);
}

static void push_pointer(long type, long x, long y) {
    if (x < 0) x = 0;
    if (y < 0) y = 0;
    if (x >= fb_w) x = fb_w - 1;
    if (y >= fb_h) y = fb_h - 1;
    queue_event(((uint64_t)type << 40) | ((uint64_t)y << 20) | (uint64_t)x);
}

static void push_resize(long w, long h) {
    queue_event(((uint64_t)6 << 40) | ((uint64_t)h << 20) | (uint64_t)w);
}

static void push_key(uint32_t keysym) {
    queue_event(((uint64_t)4 << 40) | (uint64_t)keysym);
}

// A Unicode code point as the X11 keysym the image expects: Latin-1 as itself, the rest in
// the 0x01000000 range.  Control characters are not text, so they are not sent.
static uint32_t keysym_for(uint32_t cp) {
    if (cp >= 0x20 && cp <= 0xff) return cp;
    if (cp > 0xff && cp < 0x110000) return 0x01000000 | cp;
    return 0;
}

// The keys that have no character, by their hardware key code (the same on every Mac keyboard
// layout).  Returns 0 for the rest, which are then typed as text.
static uint32_t special_keysym(unsigned short code) {
    switch (code) {
    case 36: case 76: return 0xff0d;   // Return, keypad Enter
    case 48:          return 0xff09;   // Tab
    case 51:          return 0xff08;   // Delete (backspace)
    case 117:         return 0xffff;   // Forward delete
    case 53:          return 0xff1b;   // Escape
    case 123:         return 0xff51;   // Left
    case 124:         return 0xff53;   // Right
    case 125:         return 0xff54;   // Down
    case 126:         return 0xff52;   // Up
    }
    return 0;
}

// --- the window -----------------------------------------------------------------------------

@interface ModusView : NSView
@end
@implementation ModusView
- (BOOL)isFlipped { return YES; }                  // y grows down, as the framebuffer does
- (BOOL)acceptsFirstResponder { return YES; }
- (BOOL)wantsUpdateLayer { return NO; }

- (NSPoint)pixelPointOf:(NSEvent *)e {
    NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
    return NSMakePoint(p.x * ui_scale, p.y * ui_scale);
}
- (void)mouseDown:(NSEvent *)e {
    NSPoint p = [self pixelPointOf:e];
    push_pointer(1, (long)p.x, (long)p.y);
}
- (void)mouseDragged:(NSEvent *)e {
    NSPoint p = [self pixelPointOf:e];
    push_pointer(2, (long)p.x, (long)p.y);
}
- (void)mouseUp:(NSEvent *)e {
    NSPoint p = [self pixelPointOf:e];
    push_pointer(3, (long)p.x, (long)p.y);
}

- (void)keyDown:(NSEvent *)e {
    // Command is for the menu: Cmd-Q quits, Cmd-H hides.  The image does not see those.
    if (e.modifierFlags & NSEventModifierFlagCommand) { [super keyDown:e]; return; }
    uint32_t ks = special_keysym(e.keyCode);
    if (ks) { push_key(ks); return; }
    NSString *s = e.characters;
    if (!s) return;
    [s enumerateSubstringsInRange:NSMakeRange(0, s.length)
                          options:NSStringEnumerationByComposedCharacterSequences
                       usingBlock:^(NSString *ch, NSRange r, NSRange er, BOOL *stop) {
        (void)r; (void)er; (void)stop;
        uint32_t cp = 0;
        [ch getBytes:&cp maxLength:4 usedLength:NULL encoding:NSUTF32LittleEndianStringEncoding
             options:0 range:NSMakeRange(0, ch.length) remainingRange:NULL];
        uint32_t k = keysym_for(cp);
        if (k) push_key(k);
    }];
}
@end

// The menu bar: an application menu with Quit, which is what Cmd-Q and the user expect.
@interface ModusAppDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>
@end

static void show_part(void) {
    // The surface is bigger than the window (it has to hold the largest screen), so only the
    // part the window shows is drawn: contentsRect is that part, in the surface's own units.
    if (!fb_view) return;
    fb_view.layer.contentsRect = CGRectMake(0, 0, (CGFloat)fb_w / (CGFloat)fb_cap,
                                            (CGFloat)fb_h / (CGFloat)fb_cap);
}

static void set_pixel_size(long w, long h) {
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    if (w > fb_cap) w = fb_cap;
    if (h > fb_cap) h = fb_cap;
    fb_w = w;
    fb_h = h;
    show_part();
}

@implementation ModusAppDelegate
// The display link calls this on the main thread once per refresh; it only signals the image.
- (void)tick:(CADisplayLink *)link {
    (void)link;
    if (!__atomic_exchange_n(&frame_banked, 1, __ATOMIC_ACQ_REL)) dispatch_semaphore_signal(frame_tick);
}
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app {
    (void)app;
    return YES;
}
- (void)applicationWillTerminate:(NSNotification *)n {
    (void)n;
    _exit(0);                      // the image thread is still running; do not run atexit under it
}
- (void)windowDidResize:(NSNotification *)n {
    (void)n;
    NSSize pts = fb_view.bounds.size;
    long w = (long)(pts.width * ui_scale), h = (long)(pts.height * ui_scale);
    if (w == fb_w && h == fb_h) return;
    set_pixel_size(w, h);
    push_resize(fb_w, fb_h);
}
- (void)applicationDidFinishLaunching:(NSNotification *)n {
    (void)n;
    NSRect want = NSMakeRect(0, 0, WANT_W, WANT_H);
    window = [[NSWindow alloc] initWithContentRect:want
                                         styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable |
                                                   NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable
                                           backing:NSBackingStoreBuffered
                                             defer:NO];
    window.title = @"modus";
    window.delegate = self;
    window.releasedWhenClosed = NO;

    ModusView *v = [[ModusView alloc] initWithFrame:want];
    v.wantsLayer = YES;
    v.layer.magnificationFilter = kCAFilterNearest;
    v.layer.contentsScale = ui_scale;
    v.layer.opaque = YES;
    v.layer.geometryFlipped = YES;     // row 0 of the surface is the top of the view
    if (fb_surface) v.layer.contents = (__bridge id)fb_surface;
    fb_view = v;
    window.contentView = v;
    if (fb_surface) v.layer.contents = (__bridge id)fb_surface;   // again: the view joined a window
    [window makeFirstResponder:v];
    [window center];
    [window makeKeyAndOrderFront:nil];
    [window orderFrontRegardless];
    set_pixel_size((long)(want.size.width * ui_scale), (long)(want.size.height * ui_scale));
    fprintf(stderr, "modus-ui-mac: window visible=%d frame=%.0fx%.0f at %.0f,%.0f windows=%lu policy=%ld\n",
            (int)window.isVisible, window.frame.size.width, window.frame.size.height,
            window.frame.origin.x, window.frame.origin.y, (unsigned long)NSApp.windows.count,
            (long)NSApp.activationPolicy);

    // THE FRAME CLOCK: the view's display link (macOS 14+) signals FRAME_TICK once a refresh, and
    // WAIT-FRAME waits on it.  At most one tick is banked, so a slow frame does not let the next
    // ones run back to back.  The link is per view, so it follows the window to another screen.
    display_link = [v displayLinkWithTarget:self selector:@selector(tick:)];
    [display_link addToRunLoop:[NSRunLoop mainRunLoop] forMode:NSRunLoopCommonModes];

    dispatch_semaphore_signal(ui_ready);
}
@end

static void build_menu(void) {
    NSMenu *bar = [[NSMenu alloc] init];
    NSMenuItem *appItem = [[NSMenuItem alloc] init];
    [bar addItem:appItem];
    NSMenu *appMenu = [[NSMenu alloc] initWithTitle:@"modus"];
    [appMenu addItemWithTitle:@"Quit modus" action:@selector(terminate:) keyEquivalent:@"q"];
    appItem.submenu = appMenu;
    NSApp.mainMenu = bar;
}

// Before the image thread starts: from then on a UI call waits for the window.
void modus_mac_init(void) { ui_ready = dispatch_semaphore_create(0); frame_tick = dispatch_semaphore_create(0); }

// The window's surface and the app's run loop.  Called on the main thread by the shim; it does
// not return -- the app ends with the window (or the image's exit()).
void modus_mac_main(void) __attribute__((noreturn));
void modus_mac_main(void) {
    @autoreleasepool {
        NSApplication *app = [NSApplication sharedApplication];
        [app setActivationPolicy:NSApplicationActivationPolicyRegular];
        ui_scale = (long)[NSScreen mainScreen].backingScaleFactor;
        if (ui_scale < 1) ui_scale = 1;
        CGFloat m = 0;
        for (NSScreen *s in NSScreen.screens)
            m = fmax(m, fmax(s.frame.size.width, s.frame.size.height));
        fb_cap = (long)(m * ui_scale);
        if (fb_cap < WANT_W * ui_scale) fb_cap = WANT_W * ui_scale;
        if (fb_cap < WANT_H * ui_scale) fb_cap = WANT_H * ui_scale;
        NSDictionary *props = @{ (id)kIOSurfaceWidth: @(fb_cap), (id)kIOSurfaceHeight: @(fb_cap),
                                 (id)kIOSurfaceBytesPerElement: @4,
                                 (id)kIOSurfacePixelFormat: @((uint32_t)'BGRA') };
        fb_surface = IOSurfaceCreate((__bridge CFDictionaryRef)props);
        if (fb_surface) {
            fb = (uint32_t *)IOSurfaceGetBaseAddress(fb_surface);
            fb_stride = (long)IOSurfaceGetBytesPerRow(fb_surface) / 4;
            memset(fb, 0, IOSurfaceGetAllocSize(fb_surface));
        } else {
            fb = calloc((size_t)(fb_cap * fb_cap), 4);
            fb_stride = fb_cap;
        }
        ModusAppDelegate *delegate = [[ModusAppDelegate alloc] init];
        app.delegate = delegate;
        build_menu();
        [app activateIgnoringOtherApps:YES];
        [app run];
    }
    _exit(0);
}

// --- the pseudo-syscalls: the same contract as host/ios/modus-ui.m --------------------------

static void wait_ui(void) {
    dispatch_once(&ui_once, ^{
        dispatch_semaphore_wait(ui_ready, DISPATCH_TIME_FOREVER);
        dispatch_semaphore_signal(ui_ready);
    });
}

static void present(void) {
    if (__atomic_exchange_n(&present_pending, 1, __ATOMIC_ACQ_REL)) return;
    dispatch_async(dispatch_get_main_queue(), ^{
        __atomic_store_n(&present_pending, 0, __ATOMIC_RELEASE);
        if (!fb_view) return;
        // Assign the surface afresh: the layer shows what it holds, and assigning the same object
        // again is not a change to Core Animation.  The window can replace the layer when the view
        // joins it, so this is done on the layer that is there now, every time.
        CALayer *l = fb_view.layer;
        [CATransaction begin]; [CATransaction setDisableActions:YES];
        l.contents = nil;
        l.contents = (__bridge id)fb_surface;
        l.contentsRect = CGRectMake(0, 0, (CGFloat)fb_w / (CGFloat)fb_cap, (CGFloat)fb_h / (CGFloat)fb_cap);
        [CATransaction commit];
    });
}

static long wait_frame(long ms) {
    if (!frame_tick) return -38;
    long r = dispatch_semaphore_wait(frame_tick, dispatch_time(DISPATCH_TIME_NOW, (int64_t)(ms > 0 ? ms : 1) * 1000000));
    __atomic_store_n(&frame_banked, 0, __ATOMIC_RELEASE);
    return r ? 1 : 0;
}

static long fill(long xy, long wh, long rgb) {
    long x = xy & 0xFFFF, y = (xy >> 16) & 0xFFFF, w = wh & 0xFFFF, h = (wh >> 16) & 0xFFFF;
    if (x >= fb_w || y >= fb_h) return 0;
    if (x + w > fb_w) w = fb_w - x;
    if (y + h > fb_h) h = fb_h - y;
    uint32_t c = ((uint32_t)rgb & 0xFFFFFF) | 0xFF000000;   // alpha 255: a 0 alpha is see-through
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
        for (long i = 0; i < cw; i++) row[i] = ((uint32_t)(in[i] >> 1) & 0xFFFFFF) | 0xFF000000;
    }
    return 0;
}

int64_t modus_mac_call(int64_t nr, int64_t a0, int64_t a1, int64_t a2, int64_t a3) {
    if (!ui_ready) return -38;
    wait_ui();
    switch (nr) {
    case 1001: switch (a0) { case 0: return fb_w; case 1: return fb_h; case 2: return ui_scale;
                             case 3: case 4: case 5: case 6: return 0;   // no safe area on a Mac
                             default: return -22; }
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
    case 1007: {   // copy: the UTF-8 bytes at a0, a1 long, onto the general pasteboard
        NSString *s = [[NSString alloc] initWithBytes:(const void *)(uintptr_t)a0 length:(NSUInteger)a1
                                             encoding:NSUTF8StringEncoding];
        if (!s) return -22;
        dispatch_async(dispatch_get_main_queue(), ^{
            NSPasteboard *pb = [NSPasteboard generalPasteboard];
            [pb clearContents];
            [pb setString:s forType:NSPasteboardTypeString];
        });
        return 0;
    }
    case 1006:     // keyboard: a Mac has no on-screen one, so this only gives or takes focus
        dispatch_async(dispatch_get_main_queue(), ^{
            if (a0) [window makeFirstResponder:fb_view]; else [window makeFirstResponder:nil];
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
