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
// TWO SURFACES: the image draws into fb_surface; the window shows show_surface, which PRESENT brings up to date by
// copying what changed.  So the window only ever shows finished frames -- a resize's clear, and the repaint after
// it, drawn in steps, were seen in flashes when it showed the surface being drawn.
static IOSurfaceRef show_surface;
static uint32_t *show_fb;
static long show_stride;
// What fill and blit have changed since the last present, in pixels: [x0, x1) x [y0, y1); empty when x1 <= x0.
static long dirty_x0, dirty_y0, dirty_x1, dirty_y1;
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
    // A RESIZE REPLACES ONE STILL WAITING: dragging a window's edge sends dozens, and the image would lay itself out
    // for every size it passed through, behind the drag; the latest is the only one that matters.
    if ((e >> 40) == 6 && ev_tail != ev_head && (events[(ev_tail - 1) % NEV] >> 40) == 6)
        events[(ev_tail - 1) % NEV] = e;
    else if (ev_tail - ev_head < NEV)
        events[ev_tail++ % NEV] = e;
    os_unfair_lock_unlock(&ev_lock);
}

static void push_pointer(long type, long x, long y) {
    static int debug = -1;
    if (debug < 0) debug = getenv("MODUS_UI_DEBUG") != NULL;
    if (debug) fprintf(stderr, "modus-ui-mac: pointer type %ld at %ld,%ld\n", type, x, y);
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

// The window: one that can be key and main with no title bar too, so a FULL-BLEED (borderless) window still takes
// the keyboard -- AppKit refuses a borderless window both, by default.  Close and Minimize are its own, so the Window
// menu's work when the buttons are hidden or gone.
@interface ModusWindow : NSWindow
@end
@implementation ModusWindow
- (BOOL)canBecomeKeyWindow { return YES; }
- (BOOL)canBecomeMainWindow { return YES; }
- (void)modusClose:(id)sender { (void)sender; [self close]; }
- (void)modusMinimize:(id)sender { [self miniaturize:sender]; }
@end

// A Command-drag moves the window (a full-bleed window has no title bar to take hold of); its events are the
// window's, not the image's, until the button comes up.
static BOOL moving_window;

@interface ModusView : NSView
@end
@implementation ModusView
- (BOOL)isFlipped { return YES; }                  // y grows down, as the framebuffer does
- (BOOL)acceptsFirstResponder { return YES; }
// A click on a window that is not key would otherwise only activate it: NSView drops the first
// mouse-down unless it accepts first mouse.  The desk should react to that first click too.
- (BOOL)acceptsFirstMouse:(NSEvent *)event { (void)event; return YES; }
- (BOOL)wantsUpdateLayer { return NO; }

- (NSPoint)pixelPointOf:(NSEvent *)e {
    NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
    return NSMakePoint(p.x * ui_scale, p.y * ui_scale);
}
- (void)mouseDown:(NSEvent *)e {
    if (e.modifierFlags & NSEventModifierFlagCommand) {
        moving_window = YES;
        [self.window performWindowDragWithEvent:e];
        return;
    }
    NSPoint p = [self pixelPointOf:e];
    push_pointer(1, (long)p.x, (long)p.y);
}
- (void)mouseDragged:(NSEvent *)e {
    if (moving_window) return;
    NSPoint p = [self pixelPointOf:e];
    push_pointer(2, (long)p.x, (long)p.y);
}
- (void)mouseUp:(NSEvent *)e {
    if (moving_window) { moving_window = NO; return; }
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

// The surface is bigger than the window (it has to hold the largest screen).  The layer shows it ONE TO ONE from its
// top left corner (contentsGravity top-left, at the screen's scale) and the window cuts off the rest: so resizing the
// window shows more or less of the last frame, and never stretches it while the image draws the next one.
static void set_pixel_size(long w, long h) {
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    if (w > fb_cap) w = fb_cap;
    if (h > fb_cap) h = fb_cap;
    fb_w = w;
    fb_h = h;
}

// How much of the content's top the title bar covers, in pixels: none, unless the content runs under a transparent
// title bar (host/macos/swift/MacWindow.swift's window_chrome).  The image reads it as the top of the safe area
// (1001 3), as the phone's status bar is, and lays itself out below it.
static long safe_top_px;

// The window's content in pixels, and its covered top, as they are now; a change to either is a resize event, which
// the image lays itself out again for.  The window resizing calls it, and so does a change of its chrome.
static void refit(void) {
    if (!fb_view) return;
    NSSize pts = fb_view.bounds.size;
    long w = (long)(pts.width * ui_scale), h = (long)(pts.height * ui_scale);
    long top = 0;
    if (window && (window.styleMask & NSWindowStyleMaskFullSizeContentView))
        top = (long)((pts.height - window.contentLayoutRect.size.height) * ui_scale);
    if (top < 0) top = 0;
    if (w == fb_w && h == fb_h && top == safe_top_px) return;
    safe_top_px = top;
    set_pixel_size(w, h);
    push_resize(fb_w, fb_h);
}

// For the Swift side, after it changes the window's chrome (the content's size and covered top change with it, and
// AppKit sends no resize for that).  Main thread.
void modus_mac_relayout(void) { refit(); }

// The app's name, as its bundle gives it (build-macos.sh writes it from MODUS_MAC_NAME): the window's first title,
// and the menu's.
static NSString *app_name(void) {
    NSString *name = [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleDisplayName"];
    return name.length ? name : @"modus";
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
    refit();
}
- (void)applicationDidFinishLaunching:(NSNotification *)n {
    (void)n;
    NSRect want = NSMakeRect(0, 0, WANT_W, WANT_H);
    window = [[ModusWindow alloc] initWithContentRect:want
                                         styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable |
                                                   NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable
                                           backing:NSBackingStoreBuffered
                                             defer:NO];
    window.title = app_name();
    window.delegate = self;
    window.releasedWhenClosed = NO;
    // full screen in any chrome: a borderless (full-bleed) window is not offered it unless it asks
    window.collectionBehavior |= NSWindowCollectionBehaviorFullScreenPrimary;

    ModusView *v = [[ModusView alloc] initWithFrame:want];
    v.wantsLayer = YES;
    v.layer.magnificationFilter = kCAFilterNearest;
    v.layer.contentsScale = ui_scale;
    v.layer.opaque = YES;
    v.layer.geometryFlipped = YES;     // row 0 of the surface is the top of the view
    // one to one from the top left, never stretched (set_pixel_size).  BOTTOM-left, as Core Animation names it: the
    // geometry is flipped, and in a flipped layer its "top" is the bottom of the screen
    v.layer.contentsGravity = kCAGravityBottomLeft;
    v.layer.masksToBounds = YES;                   // the window cuts off the rest of the (larger) surface
    if (show_surface) v.layer.contents = (__bridge id)show_surface;
    fb_view = v;
    window.contentView = v;
    if (show_surface) v.layer.contents = (__bridge id)show_surface;   // again: the view joined a window
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
    NSMenu *appMenu = [[NSMenu alloc] initWithTitle:app_name()];
    [appMenu addItemWithTitle:[NSString stringWithFormat:@"Quit %@", app_name()]
                       action:@selector(terminate:) keyEquivalent:@"q"];
    appItem.submenu = appMenu;
    // THE WINDOW MENU: Minimize, Close and Full Screen by their usual keys, whichever chrome the window has (they
    // reach ModusWindow through the responder chain, so a full-bleed window, with no buttons, has them too)
    NSMenuItem *windowItem = [[NSMenuItem alloc] init];
    [bar addItem:windowItem];
    NSMenu *windowMenu = [[NSMenu alloc] initWithTitle:@"Window"];
    [windowMenu addItemWithTitle:@"Minimize" action:@selector(modusMinimize:) keyEquivalent:@"m"];
    [windowMenu addItemWithTitle:@"Close" action:@selector(modusClose:) keyEquivalent:@"w"];
    NSMenuItem *full = [windowMenu addItemWithTitle:@"Enter Full Screen" action:@selector(toggleFullScreen:)
                                      keyEquivalent:@"f"];
    full.keyEquivalentModifierMask = NSEventModifierFlagCommand | NSEventModifierFlagControl;
    windowItem.submenu = windowMenu;
    NSApp.windowsMenu = windowMenu;
    NSApp.mainMenu = bar;
}

// The image's calls through the bridges (Objective-C 1040, Swift 1060) wait here, on its own thread, until the window
// is up, as its window calls do: an app's first call (its window's chrome, say) can come before AppKit has finished
// launching, and found no window.
static void wait_ui(void);
void modus_mac_wait_ready(void) { if (ui_ready) wait_ui(); }

// The window's content view, for the native components (host/macos/swift/): they are its subviews, over the
// framebuffer.  Main thread only, as AppKit is; NULL before the window is up.
void *modus_mac_root_view(void) { return (__bridge void *)fb_view; }

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
        show_surface = IOSurfaceCreate((__bridge CFDictionaryRef)props);
        if (show_surface) {
            show_fb = (uint32_t *)IOSurfaceGetBaseAddress(show_surface);
            show_stride = (long)IOSurfaceGetBytesPerRow(show_surface) / 4;
            // opaque black where nothing is drawn yet (a window grown past the last frame): zero is see-through
            size_t words = IOSurfaceGetAllocSize(show_surface) / 4;
            for (size_t i = 0; i < words; i++) show_fb[i] = 0xFF000000;
        }
        dirty_x0 = dirty_y0 = fb_cap;      // nothing changed yet
        dirty_x1 = dirty_y1 = 0;
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

// What fill and blit changed, added to the region the next present copies.
static void mark_dirty(long x, long y, long w, long h) {
    if (w <= 0 || h <= 0) return;
    if (x < dirty_x0) dirty_x0 = x;
    if (y < dirty_y0) dirty_y0 = y;
    if (x + w > dirty_x1) dirty_x1 = x + w;
    if (y + h > dirty_y1) dirty_y1 = y + h;
}

static void present(void) {
    // THE FRAME IS DONE: what changed since the last one goes into the surface the window shows.  Here, on the
    // image's thread, which is the one that draws -- so the copy is of a finished frame, never of the next being drawn.
    if (show_fb && dirty_x1 > dirty_x0 && dirty_y1 > dirty_y0) {
        IOSurfaceLock(show_surface, 0, NULL);
        size_t bytes = (size_t)(dirty_x1 - dirty_x0) * 4;
        for (long y = dirty_y0; y < dirty_y1; y++)
            memcpy(show_fb + y * show_stride + dirty_x0, fb + y * fb_stride + dirty_x0, bytes);
        IOSurfaceUnlock(show_surface, 0, NULL);
    }
    dirty_x0 = dirty_y0 = fb_cap;
    dirty_x1 = dirty_y1 = 0;
    if (__atomic_exchange_n(&present_pending, 1, __ATOMIC_ACQ_REL)) return;
    dispatch_async(dispatch_get_main_queue(), ^{
        __atomic_store_n(&present_pending, 0, __ATOMIC_RELEASE);
        if (!fb_view || !show_surface) return;
        // Assign the surface afresh: the layer shows what it holds, and assigning the same object
        // again is not a change to Core Animation.  The window can replace the layer when the view
        // joins it, so this is done on the layer that is there now, every time.
        CALayer *l = fb_view.layer;
        [CATransaction begin]; [CATransaction setDisableActions:YES];
        l.contents = nil;
        l.contents = (__bridge id)show_surface;
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
    mark_dirty(x, y, w, h);
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
    mark_dirty(x, y, cw, ch);
    return 0;
}

int64_t modus_mac_call(int64_t nr, int64_t a0, int64_t a1, int64_t a2, int64_t a3) {
    if (!ui_ready) return -38;
    wait_ui();
    switch (nr) {
    case 1001: switch (a0) { case 0: return fb_w; case 1: return fb_h; case 2: return ui_scale;
                             case 3: return safe_top_px;   // the title bar, when the content runs under it
                             case 4: case 5: case 6: return 0;
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
