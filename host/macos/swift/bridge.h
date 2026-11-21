// bridge.h — the C functions the Mac's Swift side calls: host/ios/modus-swift.m's registry (the same as the
// phone's), and the window's content view (host/macos/modus-ui-mac.m), where the native views go.

#ifndef MODUS_MAC_SWIFT_BRIDGE_H
#define MODUS_MAC_SWIFT_BRIDGE_H

// Name a Swift function, so Lisp can call it by NAME.  FN is a @convention(c) function taking six
// UInt words and returning a UInt.
void modus_swift_register(const char *name, void *fn);

// The modus window's content view (an NSView, flipped: y grows down), or NULL before the window is up.
void *modus_mac_root_view(void);

#endif
