// modus-keychain.m -- the system keychain and random bytes for the image, on macOS and iOS: the
// SSH identity and the known hosts (vessel's ios-ssh.lisp).  The shim serves pseudo-syscalls
// 1030-1032 here:
//   1030 get   name, buf, cap   -> the item's length, copied into buf; 0 if there is none (or it
//                                  is bigger than cap, or the keychain refused)
//   1031 put   name, buf, len   -> 1 stored, 0 refused
//   1032 rand  buf, len         -> 1 filled, 0 refused
// They return no negative numbers: a negative result reads in the image as -errno.

#import <Foundation/Foundation.h>
#import <Security/Security.h>
#include <stdint.h>
#include <string.h>

static NSDictionary *kc_query(const char *name) {
    return @{ (__bridge id)kSecClass: (__bridge id)kSecClassGenericPassword,
              (__bridge id)kSecAttrService: @"org.modus-lisp.modus",
              (__bridge id)kSecAttrAccount: [NSString stringWithUTF8String:name] };
}

static long kc_get(const char *name, uint8_t *buf, long cap) {
    NSMutableDictionary *q = [kc_query(name) mutableCopy];
    q[(__bridge id)kSecReturnData] = @YES;
    q[(__bridge id)kSecMatchLimit] = (__bridge id)kSecMatchLimitOne;
    CFTypeRef out = NULL;
    OSStatus st = SecItemCopyMatching((__bridge CFDictionaryRef)q, &out);
    if (st != errSecSuccess || out == NULL) return 0;
    NSData *d = (__bridge_transfer NSData *)out;
    if ((long)d.length > cap) return 0;
    memcpy(buf, d.bytes, d.length);
    return (long)d.length;
}

static long kc_put(const char *name, const uint8_t *buf, long len) {
    NSDictionary *q = kc_query(name);
    SecItemDelete((__bridge CFDictionaryRef)q);
    NSMutableDictionary *a = [q mutableCopy];
    a[(__bridge id)kSecValueData] = [NSData dataWithBytes:buf length:(NSUInteger)len];
    a[(__bridge id)kSecAttrAccessible] = (__bridge id)kSecAttrAccessibleAfterFirstUnlock;
    return SecItemAdd((__bridge CFDictionaryRef)a, NULL) == errSecSuccess ? 1 : 0;
}

long modus_keychain_call(long nr, long a0, long a1, long a2) {
    switch (nr) {
    case 1030: return kc_get((const char *)a0, (uint8_t *)a1, a2);
    case 1031: return kc_put((const char *)a0, (const uint8_t *)a1, a2);
    case 1032: return SecRandomCopyBytes(kSecRandomDefault, (size_t)a1, (uint8_t *)a0) == errSecSuccess ? 1 : 0;
    }
    return 0;
}
