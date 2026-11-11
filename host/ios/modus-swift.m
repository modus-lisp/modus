// modus-swift.m — Lisp calls Swift: a registry of named Swift functions, and the call itself
// (pseudo-syscall 1060).  The Swift side (host/ios/swift/) names each function with
// modus_swift_register, from modus_swift_install, which runs once before the first call.
//
// A call's frame is 256 bytes in the image's memory, read and written in place:
//    0  nargs     0..6
//    8  args      six words (pointers and integers)
//   64  result    the function's word
//  128  name      NUL-terminated: the function's name
// The return is 0, or -2 (ENOENT) for a name no Swift function was given.  The function runs on the
// main thread, where UIKit lives: dispatch_sync from the image's thread, or directly when the caller
// already is there.  Calls are made one at a time: the frame is the image's.

#import <Foundation/Foundation.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t (*swift_fn)(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);

enum { F_NARGS = 0, F_ARGS = 8, F_RESULT = 64, F_NAME = 128 };

#define MAX_FNS 256
static struct { const char *name; void *fn; } table[MAX_FNS];
static int nfns;
static int installed;

extern void modus_swift_install(void) __attribute__((weak));

void modus_swift_register(const char *name, void *fn) {
    if (nfns < MAX_FNS) {
        table[nfns].name = strdup(name);
        table[nfns].fn = fn;
        nfns++;
    }
}

static uintptr_t fword(const uint8_t *f, int off) { uintptr_t w; memcpy(&w, f + off, 8); return w; }

static long run(uint8_t *f) {
    if (!installed) {
        installed = 1;
        if (modus_swift_install) modus_swift_install();
    }
    const char *name = (const char *)(f + F_NAME);
    for (int i = 0; i < nfns; i++) {
        if (strcmp(table[i].name, name) == 0) {
            swift_fn fn = (swift_fn)table[i].fn;
            // a Swift function takes six words; one that takes fewer ignores the rest (arm64's
            // calling convention passes the first eight in registers)
            uintptr_t r = fn(fword(f, F_ARGS), fword(f, F_ARGS + 8), fword(f, F_ARGS + 16),
                             fword(f, F_ARGS + 24), fword(f, F_ARGS + 32), fword(f, F_ARGS + 40));
            memcpy(f + F_RESULT, &r, 8);
            return 0;
        }
    }
    return -2;
}

long modus_swift_call(long frame) {
    uint8_t *f = (uint8_t *)(uintptr_t)frame;
    if ([NSThread isMainThread]) return run(f);
    __block long rc = 0;
    dispatch_sync(dispatch_get_main_queue(), ^{ rc = run(f); });
    return rc;
}
