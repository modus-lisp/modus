// bridge.h — the C functions the Swift side calls (host/ios/modus-swift.m).

#ifndef MODUS_SWIFT_BRIDGE_H
#define MODUS_SWIFT_BRIDGE_H

// Name a Swift function, so Lisp can call it by NAME.  FN is a @convention(c) function taking six
// UInt words and returning a UInt.
void modus_swift_register(const char *name, void *fn);

#endif
