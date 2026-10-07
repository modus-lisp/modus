// modus-audio.c — a speaker for a modus image on macOS and iOS.
//
// The image speaks Linux syscalls and has no way to reach Core Audio, so the
// shim offers three pseudo-syscalls past every Linux number (as 1000-1005 are):
//
//   1010 AUDIO-OPEN rate     open the output: mono, signed 16-bit, RATE Hz.
//                            0, or a negative error.  Idempotent.
//   1011 AUDIO-WRITE src n   queue N samples from SRC, the address of element 0
//                            of a SIMPLE-VECTOR of fixnums -- a (SIGNED-BYTE 16)
//                            array as modus stores it, one 8-byte word each
//                            (value<<1), exactly as BLIT reads pixels.  Returns
//                            how many were taken: all of them unless the queue
//                            is full, when the rest are dropped.
//   1012 AUDIO-QUEUED        samples queued and not yet handed to the device.
//
// THE IMAGE KEEPS THE QUEUE FED, THE DEVICE KEEPS THE TIME.  An AudioQueue on
// its own thread drains a ring every 10 ms; the image tops the ring up to a
// cushion whenever its loop comes round (kiln's iOS main, after each paint)
// and reads AUDIO-QUEUED to know how far ahead it is.  A ring that runs dry
// plays silence -- a gap, never a stall -- and nothing here ever waits on the
// image.

#include <AudioToolbox/AudioToolbox.h>
#include <TargetConditionals.h>
#include <os/lock.h>
#include <stdint.h>
#include <string.h>

#define RING (48000 * 4)          // 4 s at 48 kHz: far more than any cushion
#define NBUF 3
static int16_t ring[RING];
static uint64_t rd, wr;           // samples ever read / written; wr - rd queued
static os_unfair_lock lk = OS_UNFAIR_LOCK_INIT;
static AudioQueueRef q;

#if TARGET_OS_IPHONE && !TARGET_OS_WATCH
extern void modus_audio_session(void);   // host/ios/modus-ui.m: play through the silent switch
#endif

static void refill(void *u, AudioQueueRef aq, AudioQueueBufferRef b) {
    (void)u;
    int16_t *out = (int16_t *)b->mAudioData;
    uint32_t n = b->mAudioDataBytesCapacity / 2, take;
    os_unfair_lock_lock(&lk);
    uint64_t avail = wr - rd;
    take = avail < n ? (uint32_t)avail : n;
    for (uint32_t i = 0; i < take; i++) out[i] = ring[(rd + i) % RING];
    rd += take;
    os_unfair_lock_unlock(&lk);
    memset(out + take, 0, (size_t)(n - take) * 2);   // dry: silence for the rest
    b->mAudioDataByteSize = n * 2;
    AudioQueueEnqueueBuffer(aq, b, 0, NULL);
}

static long audio_open(long rate) {
    if (q) return 0;
    if (rate < 8000 || rate > 192000) return -22;
#if TARGET_OS_IPHONE && !TARGET_OS_WATCH
    modus_audio_session();
#endif
    AudioStreamBasicDescription f;
    memset(&f, 0, sizeof f);
    f.mSampleRate = (Float64)rate;
    f.mFormatID = kAudioFormatLinearPCM;
    f.mFormatFlags = kLinearPCMFormatFlagIsSignedInteger | kLinearPCMFormatFlagIsPacked;
    f.mBytesPerPacket = 2; f.mFramesPerPacket = 1; f.mBytesPerFrame = 2;
    f.mChannelsPerFrame = 1; f.mBitsPerChannel = 16;
    // No run loop: the queue calls back on a thread of its own.
    if (AudioQueueNewOutput(&f, refill, NULL, NULL, NULL, 0, &q) != noErr) { q = NULL; return -5; }
    uint32_t bytes = (uint32_t)(rate / 100) * 2;      // 10 ms a buffer
    for (int i = 0; i < NBUF; i++) {
        AudioQueueBufferRef b;
        if (AudioQueueAllocateBuffer(q, bytes, &b) != noErr) return -12;
        refill(NULL, q, b);
    }
    if (AudioQueueStart(q, NULL) != noErr) return -5;
    return 0;
}

static long audio_write(long src, long n) {
    if (!q || n <= 0) return 0;
    const int64_t *s = (const int64_t *)(uintptr_t)src;
    os_unfair_lock_lock(&lk);
    uint64_t room = RING - (wr - rd);
    long take = (uint64_t)n < room ? n : (long)room;
    for (long i = 0; i < take; i++) {
        int64_t v = s[i] >> 1;                         // untag
        ring[(wr + i) % RING] = (int16_t)(v > 32767 ? 32767 : v < -32768 ? -32768 : v);
    }
    wr += take;
    os_unfair_lock_unlock(&lk);
    return take;
}

static long audio_queued(void) {
    os_unfair_lock_lock(&lk);
    long n = (long)(wr - rd);
    os_unfair_lock_unlock(&lk);
    return n;
}

long modus_audio_call(long nr, long a0, long a1) {
    switch (nr) {
    case 1010: return audio_open(a0);
    case 1011: return audio_write(a0, a1);
    case 1012: return audio_queued();
    }
    return -38;
}
