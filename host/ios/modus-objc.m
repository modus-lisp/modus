// modus-objc.m — the Objective-C runtime for a modus image on iOS: pseudo-syscall 1040.
//
// The image reaches UIKit, and any Swift class that inherits NSObject or is @objc (by its
// runtime name, "Module.Class"), through a FRAME in its own memory, read and written in place
// (the way the keychain and the framebuffer calls already work).  A frame is 384 bytes of
// little-endian words and bytes:
//
//    0  op       1 objc_getClass(name)   2 sel_registerName(name)
//                3 objc_msgSend(recv, sel, args...)
//    8  recv     the receiver (op 3)
//   16  sel      the SEL (op 3)
//   24  nargs    0..4 arguments
//   32  args     four words: integers and pointers (not floats, not structs)
//   64  result   the returned word: a pointer, or an integer
//  128  name     NUL-terminated: the class or selector name (ops 1 and 2)
//
// The call's return is 0, or -errno: -22 for a bad frame, -5 for an Objective-C exception (logged).
// It runs on the main thread, where UIKit lives: dispatch_sync from the image's thread, or
// directly when the caller already is there.  The arguments go through typed objc_msgSend
// prototypes, not a variadic call: on arm64 Apple's variadic calls pass arguments on the stack.
// One call at a time: the frame is the image's, and the image makes its calls one after another.

#import <Foundation/Foundation.h>
#import <objc/runtime.h>
#import <objc/message.h>
#include <stdint.h>
#include <string.h>

enum { F_OP = 0, F_RECV = 8, F_SEL = 16, F_NARGS = 24, F_ARGS = 32, F_RESULT = 64, F_NAME = 128 };

static uint64_t fget(const uint8_t *f, int off) { uint64_t w; memcpy(&w, f + off, 8); return w; }
static void fput(uint8_t *f, int off, uint64_t w) { memcpy(f + off, &w, 8); }

// Does SEL, sent to RECV, return an object?  Its return type says so: '@' (an id) or '#' (a class).
// A count, a BOOL or a C string comes back as a word, and retaining that word would crash.
static int returns_object(id recv, SEL sel) {
    if (!recv) return 0;
    Method m = class_getInstanceMethod(object_getClass(recv), sel);
    if (!m) return 0;
    char type[64];
    method_getReturnType(m, type, sizeof type);
    const char *t = type;
    while (*t && strchr("rnNoORV", *t)) t++;   // type qualifiers (const, in, oneway ...)
    return *t == '@' || *t == '#';
}

static long run(uint8_t *f) {
    switch (fget(f, F_OP)) {
    case 1:
        fput(f, F_RESULT, (uint64_t)(uintptr_t)objc_getClass((const char *)(f + F_NAME)));
        return 0;
    case 2:
        fput(f, F_RESULT, (uint64_t)(uintptr_t)sel_registerName((const char *)(f + F_NAME)));
        return 0;
    case 3: {
        id recv = (__bridge id)(void *)(uintptr_t)fget(f, F_RECV);
        SEL sel = (SEL)(uintptr_t)fget(f, F_SEL);
        uint64_t a0 = fget(f, F_ARGS), a1 = fget(f, F_ARGS + 8);
        uint64_t a2 = fget(f, F_ARGS + 16), a3 = fget(f, F_ARGS + 24);
        uint64_t r = 0;
        @try {
            switch (fget(f, F_NARGS)) {
            case 0: r = (uint64_t)(uintptr_t)((void *(*)(id, SEL))objc_msgSend)(recv, sel); break;
            case 1: r = (uint64_t)(uintptr_t)((void *(*)(id, SEL, uint64_t))objc_msgSend)(recv, sel, a0); break;
            case 2: r = (uint64_t)(uintptr_t)((void *(*)(id, SEL, uint64_t, uint64_t))objc_msgSend)(recv, sel, a0, a1); break;
            case 3: r = (uint64_t)(uintptr_t)((void *(*)(id, SEL, uint64_t, uint64_t, uint64_t))objc_msgSend)(recv, sel, a0, a1, a2); break;
            case 4: r = (uint64_t)(uintptr_t)((void *(*)(id, SEL, uint64_t, uint64_t, uint64_t, uint64_t))objc_msgSend)(recv, sel, a0, a1, a2, a3); break;
            default: return -22;
            }
        } @catch (NSException *e) {
            NSLog(@"modus objc: an exception from a message: %@", e.name);
            return -5;
        }
        // A returned object belongs to the image now: UIKit drains its autorelease pool between
        // two calls, which would free an alert or an action the image is still holding.  So it is
        // retained, and the image's own references are never released (a leak, for now).  Only an
        // object return is retained (returns_object); a tagged pointer (top bit set on arm64) is no
        // object in memory, and is left alone.
        if (r != 0 && !(r >> 63) && returns_object(recv, sel)) CFRetain((CFTypeRef)(uintptr_t)r);
        fput(f, F_RESULT, r);
        return 0;
    }
    default:
        return -22;
    }
}

int64_t modus_objc_call(int64_t frame) {   // int64_t: the shim's word (a long is 32 bits on a watch)
    uint8_t *f = (uint8_t *)(uintptr_t)frame;
    if ([NSThread isMainThread]) return run(f);
    __block long rc = 0;
    dispatch_sync(dispatch_get_main_queue(), ^{ rc = run(f); });
    return rc;
}
