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
//   1013 AUDIO-UNDERRUNS     how many times the device asked for a buffer and the ring could
//                            not fill it (each such buffer partly silence): the gaps a listener
//                            hears, counted where they happen.
//
// THE IMAGE KEEPS THE QUEUE FED, THE DEVICE KEEPS THE TIME.  An AudioQueue on
// its own thread drains a ring every 40 ms; the image tops the ring up to a
// cushion whenever its loop comes round (kiln's iOS main, after each paint)
// and reads AUDIO-QUEUED to know how far ahead it is.  A ring that runs dry
// plays silence -- a gap, never a stall -- and nothing here ever waits on the
// image.
//
// THE OUTPUT RUNS ONLY WHILE THERE IS SOMETHING TO HEAR.  It starts at the first
// sound written, not when it is opened, and pauses after IDLE_NS of nothing but
// silence.  An output playing silence is an app playing: on the phone it took the
// system's Now Playing at launch, and kept it while Media was paused (a headphone
// click then came as a pause, not a play).  While it is paused the queue drains by
// the wall clock instead, so the image -- which paces its mixer by AUDIO-QUEUED --
// keeps its time, and the next sound starts the output again.

#include <AudioToolbox/AudioToolbox.h>
#include <TargetConditionals.h>
#include <os/lock.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#define RING (48000 * 4)          // 4 s at 48 kHz: far more than any cushion
#define NBUF 3
static int16_t ring[RING];
static uint64_t rd, wr;           // samples ever read / written; wr - rd queued
static uint64_t underruns;        // refills that found the ring short (after the first sound)
static os_unfair_lock lk = OS_UNFAIR_LOCK_INIT;
static AudioQueueRef q;
static int running;               // the queue is started (not paused)
static uint64_t last_sound_ns;    // when a sample that was not silence was last written
static uint64_t drained_ns;       // paused: when the wall-clock drain last ran
static long q_rate;
#define IDLE_NS (2ULL * 1000000000ULL)

static uint64_t now_ns(void) { return clock_gettime_nsec_np(CLOCK_UPTIME_RAW); }

// Paused: what the device would have played since the last look, taken from the ring (lk held).
static void drain_by_clock(void) {
    uint64_t t = now_ns();
    uint64_t n = (t - drained_ns) * (uint64_t)q_rate / 1000000000ULL;
    if (n) {
        uint64_t avail = wr - rd;
        rd += n < avail ? n : avail;
        drained_ns = t;
    }
}

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
    if (take < n && wr > 0) underruns++;
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
    // 40 ms a buffer, not 10: an iPhone's hardware takes ~1024 frames a cycle, and three 10 ms
    // buffers (30 ms in all) let the queue run dry whenever its callback was a little late --
    // short regular gaps in the radio (Groove Salad on an iPhone 15 Pro) while the ring held
    // 150 ms and every sample the image wrote was whole (a tap of them showed no gap at all).
    uint32_t bytes = (uint32_t)(rate / 25) * 2;
    for (int i = 0; i < NBUF; i++) {
        AudioQueueBufferRef b;
        if (AudioQueueAllocateBuffer(q, bytes, &b) != noErr) return -12;
        refill(NULL, q, b);
    }
    // not started: the first sound starts it (audio_write)
    q_rate = rate;
    running = 0;
    drained_ns = now_ns();
    return 0;
}

static long audio_write(long src, long n) {
    if (!q || n <= 0) return 0;
    const int64_t *s = (const int64_t *)(uintptr_t)src;
    int sound = 0;
    for (long i = 0; i < n && !sound; i++) sound = (s[i] >> 1) != 0;
    uint64_t t = now_ns();
    if (sound) last_sound_ns = t;
    if (sound && !running) {
        // the silence banked while paused is dropped: the sound plays now, not behind it
        os_unfair_lock_lock(&lk);
        rd = wr;
        os_unfair_lock_unlock(&lk);
        if (AudioQueueStart(q, NULL) == noErr) running = 1;
    } else if (!sound && running && t - last_sound_ns > IDLE_NS) {
        AudioQueuePause(q);
        running = 0;
        drained_ns = t;
    }
    os_unfair_lock_lock(&lk);
    if (!running) drain_by_clock();
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
    if (q && !running) drain_by_clock();
    long n = (long)(wr - rd);
    os_unfair_lock_unlock(&lk);
    return n;
}

int64_t modus_audio_call(int64_t nr, int64_t a0, int64_t a1) {   // int64_t: a long is 32 bits on a watch
    switch (nr) {
    case 1010: return audio_open((long)a0);
    case 1011: return audio_write((long)a0, (long)a1);
    case 1012: return audio_queued();
    case 1013: return (int64_t)underruns;
    }
    return -38;
}
