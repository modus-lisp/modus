// modus-launcher.c — the Android app around a modus image (design A).
//
// The app is a NativeActivity (no Java).  This library is its native half: it
// starts the modus image as a CHILD PROCESS and is that process's screen and
// touch input.  The image ships as lib/arm64-v8a/libmodus.so — a static ELF,
// not a library; that name is how an Android app gets an executable it may
// exec (extractNativeLibs puts it in the native-library dir, which an app may
// execute from; its own data dir it may not).
//
// The child gets one end of a socketpair as fd 3 and speaks the iOS shim's
// four calls (host/ios/modus-ui.m) as 16-byte records <op a b c>, u32 each:
//   1001 UI-INFO k          reply u64: k=0 width, 1 height (pixels), 2 scale
//   1002 FILL  xy wh rgb    fill [x,x+w)×[y,y+h) with 0xRRGGBB, clipped;
//                           xy = x | y<<16, wh = w | h<<16.  No reply.
//   1003 PRESENT            show the buffer.  No reply.
//   1004 NEXT-EVENT         reply u64: the oldest touch, type<<40 | y<<20 | x,
//                           type 1 down / 2 move / 3 up; 0 when there is none
// A socket rather than a shared-memory ring: every record is a syscall, which
// orders memory on a weakly-ordered CPU without a barrier primitive in Lisp.
// Filling is native, so the (interpreted) Lisp only decides WHAT to draw.
//
// The child's stdout and stderr go to logcat (tag "modus").  The script it runs
// is named by the build (MODUS_SCRIPT) and written into the app's data dir on
// first start, from the bytes embedded below.

#define _GNU_SOURCE
#include <android/log.h>
#include <android/looper.h>
#include <android/native_activity.h>
#include <android/native_window.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

#include "modus-script.h"   // generated: modus_script[], modus_script_len

#define TAG "modus"
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, TAG, __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, TAG, __VA_ARGS__)

static pthread_mutex_t lk = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  ready_cv = PTHREAD_COND_INITIALIZER;
static ANativeWindow *win;          // the current window, or NULL
static uint32_t *fb;                // w*h pixels in the window's byte order
static int fb_w, fb_h;
static int dirty;                   // a PRESENT is pending

#define NEV 1024
static uint64_t events[NEV];
static unsigned ev_head, ev_tail;   // ev_tail - ev_head = queued

static pid_t child = -1;
static int sock = -1;               // our end of the socketpair

// 0xRRGGBB -> RGBX_8888 as stored: bytes R,G,B,X = little-endian 0xXXBBGGRR.
static uint32_t to_px(uint32_t rgb) {
    return 0xFF000000u | ((rgb & 0xFF) << 16) | (rgb & 0xFF00) | ((rgb >> 16) & 0xFF);
}

static void fill(uint32_t xy, uint32_t wh, uint32_t rgb) {
    int x = xy & 0xFFFF, y = xy >> 16, w = wh & 0xFFFF, h = wh >> 16;
    uint32_t px = to_px(rgb);
    pthread_mutex_lock(&lk);
    if (fb) {
        int x1 = x + w > fb_w ? fb_w : x + w, y1 = y + h > fb_h ? fb_h : y + h;
        for (int r = y; r < y1; r++)
            for (int c = x; c < x1; c++) fb[r * fb_w + c] = px;
    }
    pthread_mutex_unlock(&lk);
}

static void present_locked(void) {
    ANativeWindow_Buffer b;
    if (!win || !fb || ANativeWindow_lock(win, &b, NULL) != 0) return;
    int h = b.height < fb_h ? b.height : fb_h, w = b.width < fb_w ? b.width : fb_w;
    for (int r = 0; r < h; r++)
        memcpy((uint32_t *)b.bits + (size_t)r * b.stride, fb + (size_t)r * fb_w, (size_t)w * 4);
    ANativeWindow_unlockAndPost(win);
}

static int read_full(int fd, void *p, size_t n) {
    for (size_t got = 0; got < n;) {
        ssize_t k = read(fd, (char *)p + got, n - got);
        if (k == 0) return -1;
        if (k < 0) { if (errno == EINTR) continue; return -1; }
        got += k;
    }
    return 0;
}

// The child's requests, in order, until it closes its end.
static void *serve(void *arg) {
    (void)arg;
    uint32_t rec[4];
    while (read_full(sock, rec, sizeof rec) == 0) {
        uint64_t reply = 0;
        switch (rec[0]) {
        case 1001:
            pthread_mutex_lock(&lk);
            while (!fb) pthread_cond_wait(&ready_cv, &lk);   // like iOS: wait for the view
            reply = rec[1] == 0 ? (uint64_t)fb_w : rec[1] == 1 ? (uint64_t)fb_h : 1;
            pthread_mutex_unlock(&lk);
            write(sock, &reply, 8);
            break;
        case 1002: fill(rec[1], rec[2], rec[3]); break;
        case 1003:
            pthread_mutex_lock(&lk); present_locked(); dirty = 0; pthread_mutex_unlock(&lk);
            break;
        case 1004:
            pthread_mutex_lock(&lk);
            if (ev_head != ev_tail) reply = events[ev_head++ % NEV];
            pthread_mutex_unlock(&lk);
            write(sock, &reply, 8);
            break;
        default:
            LOGE("unknown request %u", rec[0]);
            reply = (uint64_t)-38;
            write(sock, &reply, 8);
        }
    }
    LOGI("modus closed the UI socket");
    return NULL;
}

// Child stdout/stderr -> logcat, a line at a time.
static void *pump(void *arg) {
    int fd = (int)(intptr_t)arg;
    char buf[1024]; size_t n = 0;
    for (;;) {
        ssize_t k = read(fd, buf + n, sizeof buf - 1 - n);
        if (k <= 0) break;
        n += k;
        char *s = buf, *nl;
        while ((nl = memchr(s, '\n', buf + n - s))) { *nl = 0; LOGI("%s", s); s = nl + 1; }
        n -= s - buf; memmove(buf, s, n);
        if (n == sizeof buf - 1) { buf[n] = 0; LOGI("%s", buf); n = 0; }
    }
    if (n) { buf[n] = 0; LOGI("%s", buf); }
    return NULL;
}

static void start_modus(ANativeActivity *act) {
    // The native-library directory is wherever this library was loaded from.
    Dl_info di;
    if (!dladdr((void *)&start_modus, &di) || !di.dli_fname) { LOGE("dladdr failed"); return; }
    char libdir[512];
    snprintf(libdir, sizeof libdir, "%s", di.dli_fname);
    char *slash = strrchr(libdir, '/'); if (slash) *slash = 0;
    char image[600], script[600];
    snprintf(image, sizeof image, "%s/libmodus.so", libdir);
    snprintf(script, sizeof script, "%s/" MODUS_SCRIPT_NAME, act->internalDataPath);

    FILE *f = fopen(script, "wb");
    if (!f) { LOGE("cannot write %s: %s", script, strerror(errno)); return; }
    fwrite(modus_script, 1, modus_script_len, f); fclose(f);

    int sp[2], out[2];
    if (socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sp) || pipe2(out, O_CLOEXEC)) {
        LOGE("socketpair/pipe: %s", strerror(errno)); return;
    }
    child = fork();
    if (child == 0) {
        dup2(sp[1], 3); dup2(out[1], 1); dup2(out[1], 2);
        setenv("HOME", act->internalDataPath, 1);
        chdir(act->internalDataPath);
        execl(image, "modus", "--script", script, (char *)NULL);
        dprintf(2, "exec %s: %s\n", image, strerror(errno));
        _exit(127);
    }
    close(sp[1]); close(out[1]);
    if (child < 0) { LOGE("fork: %s", strerror(errno)); close(sp[0]); close(out[0]); return; }
    sock = sp[0];
    LOGI("started modus pid %d: %s --script %s", child, image, script);
    pthread_t t;
    pthread_create(&t, NULL, serve, NULL); pthread_detach(t);
    pthread_create(&t, NULL, pump, (void *)(intptr_t)out[0]); pthread_detach(t);
}

// ---- touch input -------------------------------------------------------------

static void push_event(int type, float x, float y) {
    uint64_t xi = x < 0 ? 0 : (uint64_t)x, yi = y < 0 ? 0 : (uint64_t)y;
    uint64_t e = ((uint64_t)type << 40) | ((yi & 0xFFFFF) << 20) | (xi & 0xFFFFF);
    pthread_mutex_lock(&lk);
    if (ev_tail - ev_head < NEV) events[ev_tail++ % NEV] = e;
    pthread_mutex_unlock(&lk);
}

static int on_input(int fd, int evs, void *data) {
    (void)fd; (void)evs;
    AInputQueue *q = data;
    AInputEvent *ev;
    while (AInputQueue_getEvent(q, &ev) >= 0) {
        if (AInputQueue_preDispatchEvent(q, ev)) continue;
        int handled = 0;
        if (AInputEvent_getType(ev) == AINPUT_EVENT_TYPE_MOTION) {
            int a = AMotionEvent_getAction(ev) & AMOTION_EVENT_ACTION_MASK;
            int type = a == AMOTION_EVENT_ACTION_DOWN ? 1 : a == AMOTION_EVENT_ACTION_MOVE ? 2
                     : a == AMOTION_EVENT_ACTION_UP ? 3 : 0;
            if (type) { push_event(type, AMotionEvent_getX(ev, 0), AMotionEvent_getY(ev, 0)); handled = 1; }
        }
        AInputQueue_finishEvent(q, ev, handled);
    }
    return 1;
}

static void onInputQueueCreated(ANativeActivity *a, AInputQueue *q) {
    (void)a;
    AInputQueue_attachLooper(q, ALooper_forThread(), 1, on_input, q);
}
static void onInputQueueDestroyed(ANativeActivity *a, AInputQueue *q) { (void)a; AInputQueue_detachLooper(q); }

// ---- window ------------------------------------------------------------------

static void onNativeWindowCreated(ANativeActivity *a, ANativeWindow *w) {
    (void)a;
    ANativeWindow_setBuffersGeometry(w, 0, 0, WINDOW_FORMAT_RGBX_8888);
    pthread_mutex_lock(&lk);
    win = w;
    int nw = ANativeWindow_getWidth(w), nh = ANativeWindow_getHeight(w);
    if (!fb) {                      // first window: the size modus is told
        fb_w = nw; fb_h = nh;
        fb = calloc((size_t)fb_w * fb_h, 4);
        LOGI("window %dx%d", fb_w, fb_h);
        pthread_cond_broadcast(&ready_cv);
    }
    present_locked();               // a recreated window shows the last frame
    pthread_mutex_unlock(&lk);
}
static void onNativeWindowRedrawNeeded(ANativeActivity *a, ANativeWindow *w) {
    (void)a; (void)w;
    pthread_mutex_lock(&lk); present_locked(); pthread_mutex_unlock(&lk);
}
static void onNativeWindowDestroyed(ANativeActivity *a, ANativeWindow *w) {
    (void)a; (void)w;
    pthread_mutex_lock(&lk); win = NULL; pthread_mutex_unlock(&lk);
}

static void onDestroy(ANativeActivity *a) {
    (void)a;
    if (child > 0) { kill(child, SIGTERM); waitpid(child, NULL, 0); child = -1; }
    if (sock >= 0) { close(sock); sock = -1; }
}

JNIEXPORT void ANativeActivity_onCreate(ANativeActivity *act, void *saved, size_t saved_size) {
    (void)saved; (void)saved_size;
    act->callbacks->onNativeWindowCreated = onNativeWindowCreated;
    act->callbacks->onNativeWindowRedrawNeeded = onNativeWindowRedrawNeeded;
    act->callbacks->onNativeWindowDestroyed = onNativeWindowDestroyed;
    act->callbacks->onInputQueueCreated = onInputQueueCreated;
    act->callbacks->onInputQueueDestroyed = onInputQueueDestroyed;
    act->callbacks->onDestroy = onDestroy;
    signal(SIGPIPE, SIG_IGN);
    if (child < 0) start_modus(act);
}
