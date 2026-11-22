// bridge.h — the C functions the Mac's Swift side calls: host/ios/modus-swift.m's registry (the same as the
// phone's), and the window's content view (host/macos/modus-ui-mac.m), where the native views go.

#ifndef MODUS_MAC_SWIFT_BRIDGE_H
#define MODUS_MAC_SWIFT_BRIDGE_H

// Name a Swift function, so Lisp can call it by NAME.  FN is a @convention(c) function taking six
// UInt words and returning a UInt.
void modus_swift_register(const char *name, void *fn);

// The modus window's content view (an NSView, flipped: y grows down), or NULL before the window is up.
void *modus_mac_root_view(void);

// After a change to the window's chrome: the content's size, and how much of its top the title bar covers, measured
// again, and a resize event for the image if either changed.
void modus_mac_relayout(void);

// The app's own events (modus-ui-mac.m): the hooks the Swift side sets, and the opens kept until it did.
extern void (*modus_mac_opened_hook)(int kind, const char *url);
extern int (*modus_mac_quit_hook)(void);
void modus_mac_opened_flush(void);

// The pointer's shape over the window: 0 arrow, 1 I-beam, 2 pointing hand, 3 open hand, 4 closed hand,
// 5 crosshair, 6 resize left-right, 7 resize up-down, 8 not allowed.
void modus_mac_pointer_shape(int kind);

// Lock the pointer (1: hidden, held still, its moves sent as motion events, type 13) or let it go (0).
void modus_mac_pointer_lock(int on);

#endif
