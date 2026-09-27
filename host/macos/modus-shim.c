// modus-shim.c — run a hosted-aarch64 modus image natively on macOS
// (docs/macos-hosting.md, M0/M2).
//
// The image is the ordinary Linux/aarch64 ELF, built with the Darwin layout
// (MODUS_DARWIN=1 MODUS_CODE_BASE=... MODUS_CONV_DELTA=... ...), embedded in
// this executable's own signed __TEXT,__modus section.  At start-up the shim:
//
//   1. mach_vm_remaps those signed pages to the image's link address (the
//      hosted equivalent of the RPi head.S MMU remap): the image is not PIC,
//      and macOS will not let anything create executable pages at a fixed
//      address any other way;
//   2. maps the page one 16 KB below the code base and stores the address of
//      the syscall stub in its first word — the slot the image's syscall
//      sites load (translate-aarch64 A64-SVC);
//   3. builds the Linux initial stack (argc, argv, envp, auxv) on a fresh
//      stack and jumps to the ELF entry.
//
// Everything else — the runtime-data region, the heap, the (absent) JIT
// arena — the image's own boot stub maps, through the syscall translator
// below, exactly as it does on Linux.

#include <errno.h>
#include <fcntl.h>
#include <dirent.h>
#include <mach-o/getsect.h>
#include <mach-o/ldsyms.h>
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <os/os_sync_wait_on_address.h>
#include <os/clock.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <sched.h>
#include <sys/socket.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>

extern void modus_syscall_stub(void);
extern void modus_enter(uint64_t sp, uint64_t entry) __attribute__((noreturn));

#define PAGE16K 0x4000ULL
#define ROUND_UP(x, a) (((x) + (a) - 1) & ~((a) - 1))

static void die(const char *what, long code) {
    fprintf(stderr, "modus-shim: %s (%ld)\n", what, code);
    _exit(111);
}

// ---------------------------------------------------------------- errno ---
// Darwin errno -> Linux errno, for the values that differ.  The image checks
// results as -errno the Linux way.
static long lx_errno(int e) {
    switch (e) {
    case EAGAIN:       return 11;
    case EINPROGRESS:  return 115;
    case EALREADY:     return 114;
    case ENOTSOCK:     return 88;
    case EDESTADDRREQ: return 89;
    case EMSGSIZE:     return 90;
    case EPROTOTYPE:   return 91;
    case ENOPROTOOPT:  return 92;
    case EPROTONOSUPPORT: return 93;
    case ENOTSUP:      return 95;
    case EAFNOSUPPORT: return 97;
    case EADDRINUSE:   return 98;
    case EADDRNOTAVAIL: return 99;
    case ENETDOWN:     return 100;
    case ENETUNREACH:  return 101;
    case ECONNABORTED: return 103;
    case ECONNRESET:   return 104;
    case ENOBUFS:      return 105;
    case EISCONN:      return 106;
    case ENOTCONN:     return 107;
    case ETIMEDOUT:    return 110;
    case ECONNREFUSED: return 111;
    case ELOOP:        return 40;
    case ENAMETOOLONG: return 36;
    case EHOSTUNREACH: return 113;
    case ENOTEMPTY:    return 39;
    case ENOLCK:       return 37;
    case ENOSYS:       return 38;
    default:           return e;   // 1..34 agree
    }
}
#define RET(expr) do { long _r = (long)(expr); return _r < 0 ? -lx_errno(errno) : _r; } while (0)

// ------------------------------------------------------- flag / constant ---
static int dx_dirfd(long fd) { return (int)fd == -100 ? AT_FDCWD : (int)fd; }   // Linux AT_FDCWD = -100

static int dx_open_flags(long f) {
    int o = (int)(f & 3);                          // O_RDONLY/O_WRONLY/O_RDWR agree
    if (f & 0x40)     o |= O_CREAT;
    if (f & 0x80)     o |= O_EXCL;
    if (f & 0x100)    o |= O_NOCTTY;
    if (f & 0x200)    o |= O_TRUNC;
    if (f & 0x400)    o |= O_APPEND;
    if (f & 0x800)    o |= O_NONBLOCK;
    if (f & 0x14000)  o |= O_DIRECTORY;            // aarch64 0x4000; the runtime may pass x86-64's 0x10000
    if (f & 0x8000)   o |= O_NOFOLLOW;
    if (f & 0x80000)  o |= O_CLOEXEC;
    return o;
}

static int dx_at_flags(long f) {
    int o = 0;
    if (f & 0x100) o |= AT_SYMLINK_NOFOLLOW;
    if (f & 0x200) o |= AT_REMOVEDIR;              // same bit Linux uses for unlinkat
    return o;
}

static clockid_t dx_clock(long c) {
    switch (c) {
    case 0: return CLOCK_REALTIME;
    case 1: return CLOCK_MONOTONIC;
    case 2: return CLOCK_PROCESS_CPUTIME_ID;
    case 3: return CLOCK_THREAD_CPUTIME_ID;
    case 4: return CLOCK_MONOTONIC_RAW;
    case 7: return CLOCK_MONOTONIC;                // BOOTTIME
    default: return CLOCK_REALTIME;
    }
}

// Linux aarch64 struct stat (128 bytes).
struct lx_stat {
    uint64_t dev, ino; uint32_t mode, nlink, uid, gid; uint64_t rdev, pad1;
    int64_t size; int32_t blksize, pad2; int64_t blocks;
    int64_t atime, atime_ns, mtime, mtime_ns, ctime, ctime_ns; uint32_t unused[2];
};
static void to_lx_stat(const struct stat *s, struct lx_stat *l) {
    memset(l, 0, sizeof *l);
    l->dev = s->st_dev; l->ino = s->st_ino; l->mode = s->st_mode; l->nlink = s->st_nlink;
    l->uid = s->st_uid; l->gid = s->st_gid; l->rdev = s->st_rdev; l->size = s->st_size;
    l->blksize = s->st_blksize; l->blocks = s->st_blocks;
    l->atime = s->st_atimespec.tv_sec; l->atime_ns = s->st_atimespec.tv_nsec;
    l->mtime = s->st_mtimespec.tv_sec; l->mtime_ns = s->st_mtimespec.tv_nsec;
    l->ctime = s->st_ctimespec.tv_sec; l->ctime_ns = s->st_ctimespec.tv_nsec;
}

// JIT regions: MAP_JIT memory is writable OR executable per thread, never
// both (pthread_jit_write_protect_np).  The image writes code and then runs
// it exactly as on Linux; the fault handler below flips the mode.
#define MAXJIT 64
static struct { uint64_t lo, hi; } jit_regions[MAXJIT];
static int n_jit;
static int in_jit(uint64_t a) {
    for (int i = 0; i < n_jit; i++) if (a >= jit_regions[i].lo && a < jit_regions[i].hi) return 1;
    return 0;
}

static long dx_mmap(long addr, long len, long prot, long flags, long fd, long off) {
    int f = (int)(flags & 3);                      // SHARED/PRIVATE agree
    if (flags & 0x20) f |= MAP_ANON;
    int noreplace = (flags & 0x100000) != 0;       // MAP_FIXED_NOREPLACE
    int jit = (prot & PROT_EXEC) && (flags & 0x20);
    // macOS refuses RWX anonymous memory without MAP_JIT, and MAP_JIT with
    // MAP_FIXED; it does honour MAP_JIT's address HINT (probed), which keeps
    // the JIT arena at its fixed address for save-and-die.
    if (jit) f |= MAP_JIT;
    else if ((flags & 0x10) && !noreplace) f |= MAP_FIXED;
    void *p = mmap((void *)addr, (size_t)len, (int)prot, f, (int)fd, (off_t)off);
    if (p != MAP_FAILED && jit) {
        if (noreplace && (long)p != addr) { munmap(p, (size_t)len); return -17; }
        if (n_jit < MAXJIT) { jit_regions[n_jit].lo = (uint64_t)p; jit_regions[n_jit].hi = (uint64_t)p + (uint64_t)len; n_jit++; }
        pthread_jit_write_protect_np(1);           // running mode: executable
        return (long)p;
    }
    if (p == MAP_FAILED && (prot & PROT_EXEC) && (prot & PROT_WRITE) && addr == 0) {
        // An unfixed RWX request is the runtime's exec-page primitive with no
        // JIT arena: on an M0 (JIT-off) image it only ever holds DATA (the GC
        // bitmaps).  Give it RW.  Real JIT pages need MAP_JIT and
        // pthread_jit_write_protect_np — M3.
        static int said;
        if (!said) { said = 1; fprintf(stderr, "modus-shim: RWX mmap refused by macOS; mapping RW (no JIT)\n"); }
        p = mmap(NULL, (size_t)len, (int)(prot & ~PROT_EXEC), f, (int)fd, (off_t)off);
    }
    if (p == MAP_FAILED) return -lx_errno(errno);
    if (noreplace && (long)p != addr) { munmap(p, (size_t)len); return -17; }   // EEXIST
    return (long)p;
}

// ------------------------------------------------------------- signals ---
// Linux and Darwin agree on most signal numbers but not all.
static int dx_sig(long l) {
    switch (l) {
    case 7:  return SIGBUS;   case 10: return SIGUSR1;  case 12: return SIGUSR2;
    case 17: return SIGCHLD;  case 18: return SIGCONT;  case 19: return SIGSTOP;
    case 20: return SIGTSTP;  case 23: return SIGURG;   case 29: return SIGIO;
    case 31: return SIGSYS;   default: return (int)l;   // 1-6, 8, 9, 11, 13-16, 21-22, 24-28
    }
}
static sigset_t dx_sigset(uint64_t lx) {
    sigset_t s; sigemptyset(&s);
    for (long l = 1; l < 32; l++) if (lx & (1ULL << (l - 1))) sigaddset(&s, dx_sig(l));
    return s;
}
static uint64_t lx_sigset(sigset_t s) {
    uint64_t m = 0;
    for (long l = 1; l < 32; l++) if (sigismember(&s, dx_sig(l))) m |= 1ULL << (l - 1);
    return m;
}
// Linux aarch64 struct sigaction as rt_sigaction takes it.
struct lx_sigaction { uint64_t handler, flags, restorer, mask; };

// SIGSEGV and SIGBUS stay the shim's own (the JIT mode flip must see them
// first); what the image registers for them is kept here and chained to.
static struct lx_sigaction image_fault_handler[2];      // [0] SEGV, [1] BUS
static int fault_slot(int sig) { return sig == SIGSEGV ? 0 : sig == SIGBUS ? 1 : -1; }

static long dx_rt_sigaction(long lsig, struct lx_sigaction *nw, struct lx_sigaction *old) {
    int sig = dx_sig(lsig);
    int slot = fault_slot(sig);
    if (slot >= 0) {
        if (old) *old = image_fault_handler[slot];
        if (nw) image_fault_handler[slot] = *nw;
        return 0;
    }
    struct sigaction dn, dold; memset(&dn, 0, sizeof dn);
    if (nw) {
        // The image's handler (translate-aarch64 trap #x0520) is a stub that
        // ignores its arguments and branches into the armed handler-case
        // frame without returning, so Darwin can call it directly.
        dn.sa_sigaction = (void (*)(int, siginfo_t *, void *))(uintptr_t)nw->handler;
        int f = 0;
        if (nw->flags & 0x4)        f |= SA_SIGINFO;
        if (nw->flags & 0x08000000) f |= SA_ONSTACK;
        if (nw->flags & 0x10000000) f |= SA_RESTART;
        if (nw->flags & 0x40000000) f |= SA_NODEFER;
        if (nw->flags & 0x80000000) f |= SA_RESETHAND;
        dn.sa_flags = f;
        dn.sa_mask = dx_sigset(nw->mask);
    }
    if (sigaction(sig, nw ? &dn : NULL, &dold) < 0) return -lx_errno(errno);
    if (old) {
        old->handler = (uint64_t)(uintptr_t)dold.sa_sigaction;
        old->flags = ((dold.sa_flags & SA_SIGINFO) ? 0x4 : 0) | ((dold.sa_flags & SA_NODEFER) ? 0x40000000 : 0)
                   | ((dold.sa_flags & SA_RESTART) ? 0x10000000 : 0) | ((dold.sa_flags & SA_ONSTACK) ? 0x08000000 : 0);
        old->restorer = 0; old->mask = lx_sigset(dold.sa_mask);
    }
    return 0;
}

static long dx_rt_sigprocmask(long how, const uint64_t *set, uint64_t *old) {
    int h = how == 0 ? SIG_BLOCK : how == 1 ? SIG_UNBLOCK : SIG_SETMASK;
    sigset_t ns, os; if (set) ns = dx_sigset(*set);
    if (sigprocmask(h, set ? &ns : NULL, &os) < 0) return -lx_errno(errno);
    if (old) *old = lx_sigset(os);
    return 0;
}

// ----------------------------------------------------------- directories ---
// getdents64 reads packed linux_dirent64 records from an fd.  Darwin has no
// public equivalent, so keep a DIR* per fd (fdopendir on a dup, so closedir
// cannot close the image's fd) and refill from readdir; one record that did
// not fit waits for the next call.
struct lx_dirent64 { uint64_t ino; int64_t off; uint16_t reclen; uint8_t type; char name[]; };
#define MAXDIRFD 1024
static DIR *dirs[MAXDIRFD];
static struct dirent *pending[MAXDIRFD];

static long dx_getdents64(long fd, uint8_t *buf, long len) {
    if (fd < 0 || fd >= MAXDIRFD) return -9;       // EBADF
    if (!dirs[fd]) {
        int d = dup((int)fd);
        if (d < 0) return -lx_errno(errno);
        if (!(dirs[fd] = fdopendir(d))) { close(d); return -lx_errno(errno); }
    }
    long used = 0;
    for (;;) {
        struct dirent *e = pending[fd] ? pending[fd] : readdir(dirs[fd]);
        pending[fd] = NULL;
        if (!e) break;
        size_t nlen = strlen(e->d_name);
        size_t rec = ROUND_UP(sizeof(struct lx_dirent64) + nlen + 1, 8);
        if (used + (long)rec > len) {
            if (used == 0) return -22;             // EINVAL: buffer too small
            pending[fd] = e; break;
        }
        struct lx_dirent64 *d = (struct lx_dirent64 *)(buf + used);
        d->ino = e->d_ino; d->off = used + (long)rec; d->reclen = (uint16_t)rec;
        d->type = e->d_type;                       // DT_* values agree
        memcpy(d->name, e->d_name, nlen + 1);
        used += (long)rec;
    }
    return used;
}
static void forget_dir(long fd) {
    if (fd >= 0 && fd < MAXDIRFD && dirs[fd]) { closedir(dirs[fd]); dirs[fd] = NULL; pending[fd] = NULL; }
}

// -------------------------------------------------------------- sockets ---
// Linux sockaddr: u16 family then data; Darwin: u8 length, u8 family, data.
// AF_INET is 2 on both; Linux AF_INET6 = 10, Darwin 30.
static socklen_t dx_sockaddr(const uint8_t *lx, socklen_t len, struct sockaddr_storage *out) {
    if (!lx || len < 2) return 0;
    if (len > sizeof *out) len = sizeof *out;
    memcpy(out, lx, len);
    uint16_t fam = (uint16_t)(lx[0] | (lx[1] << 8));
    ((uint8_t *)out)[0] = (uint8_t)len;
    ((uint8_t *)out)[1] = (uint8_t)(fam == 10 ? AF_INET6 : fam);
    return len;
}
static void lx_sockaddr(const struct sockaddr_storage *d, uint8_t *lx, uint32_t *lxlen, socklen_t dlen) {
    if (!lx || !lxlen) return;
    socklen_t n = dlen < *lxlen ? dlen : *lxlen;
    memcpy(lx, d, n);
    uint16_t fam = ((const uint8_t *)d)[1] == AF_INET6 ? 10 : ((const uint8_t *)d)[1];
    if (n >= 2) { lx[0] = (uint8_t)fam; lx[1] = (uint8_t)(fam >> 8); }
    *lxlen = dlen;
}
static int dx_sockopt(long level, long opt, int *dlevel) {
    if (level == 1) {                              // SOL_SOCKET
        *dlevel = SOL_SOCKET;
        switch (opt) {
        case 2: return SO_REUSEADDR;  case 4: return SO_ERROR;   case 6: return SO_BROADCAST;
        case 7: return SO_SNDBUF;     case 8: return SO_RCVBUF;  case 9: return SO_KEEPALIVE;
        case 13: return SO_LINGER;    case 15: return SO_REUSEPORT;
        case 20: return SO_RCVTIMEO;  case 21: return SO_SNDTIMEO;
        default: return -1;
        }
    }
    *dlevel = (int)level;                          // IPPROTO_TCP = 6, TCP_NODELAY = 1 agree
    return (int)opt;
}
static long dx_socket(long domain, long type, long proto) {
    int d = domain == 10 ? AF_INET6 : (int)domain;
    int fd = socket(d, (int)(type & 0xF), (int)proto);   // SOCK_STREAM/DGRAM agree
    if (fd < 0) return -lx_errno(errno);
    if (type & 0x800) fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);   // SOCK_NONBLOCK
    if (type & 0x80000) fcntl(fd, F_SETFD, FD_CLOEXEC);                     // SOCK_CLOEXEC
    int one = 1; setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one); // Linux callers expect EPIPE
    return fd;
}

// A read(2) INTO the JIT arena fails with EFAULT: the kernel's copy-out does
// not honour the thread's MAP_JIT write mode, and no fault reaches the flip
// handler.  (Restoring a core reads its JIT pages straight into the arena.)
// Read into ordinary memory and copy in with write mode open.
static int jit_overlap(uint64_t p, uint64_t n) { return n && (in_jit(p) || in_jit(p + n - 1)); }
static long jit_read(int fd, void *buf, size_t n) {
    if (!jit_overlap((uint64_t)buf, n)) {
        long r = read(fd, buf, n);
        if (getenv("MODUS_SHIM_TRACE") && (r < 0 || (size_t)r < n))
            fprintf(stderr, "modus-shim: read(fd=%d, buf=%p, n=%zu) = %ld\n", fd, buf, n, r);
        return r;
    }
    void *tmp = malloc(n);
    if (!tmp) { errno = ENOMEM; return -1; }
    long r = read(fd, tmp, n);
    int e = errno;
    if (r > 0) {
        pthread_jit_write_protect_np(0);
        memcpy(buf, tmp, (size_t)r);
        pthread_jit_write_protect_np(1);
    }
    free(tmp);
    errno = e;
    return r;
}

// -------------------------------------------------------------- threads ---
// The image's threads are Linux threads: clone(CLONE_VM|...|CLONE_THREAD|
// PARENT_SETTID|CHILD_CLEARTID) with a stack of its own, a per-thread window
// delta (translate-aarch64.lisp, THE PER-THREAD WINDOW, AARCH64), exit(93) for
// the thread alone, and a join that polls the TID word the kernel clears.
//
// clone returns TWICE, in the parent and in the child, with every register
// but x0 and SP the parent's.  A pthread cannot be born that way, so the
// stub's frame (every register at the call) is copied into a resume context
// and the new pthread jumps into the image with it: PC = the instruction after
// the stub call, SP = the image's stack, x0 = 0.  The C pthread's own stack
// is left behind after that; syscalls from the thread run on the image's
// stack, as the main thread's do.
//
// The delta lives in a pthread key the image reads at TPIDRRO_EL0 + 8*key.
// Keys are handed out lowest-free-first, so creating keys until we hold
// MODUS_TSD_KEY reserves it; the image was linked against the same number
// (hosted-layout-env.lisp +LAYOUT-DARWIN-TSD-KEY+).
#define MODUS_TSD_KEY 300
#define LX_SYS_SET_THREAD_DELTA 1000     // translate-aarch64 +A64-DARWIN-SYS-SET-THREAD-DELTA+

struct stub_frame {                       // syscall-stub.S, 608 bytes
    uint64_t x1_15[15];                   // x1..x15            @0
    uint64_t x17, x29, x30;               //                    @120
    uint64_t q[48];                       // q0-7, q16-31       @144
    uint64_t x19_28[10];                  // x19..x28           @528
};
struct resume_ctx { uint64_t x[31]; uint64_t sp, pc; };
extern void modus_resume(struct resume_ctx *) __attribute__((noreturn));

static pthread_key_t delta_key;
static __thread uint32_t *my_ctid;        // CLONE_CHILD_CLEARTID word, or NULL
static __thread int is_clone;             // a thread this shim started

static void reserve_delta_key(void) {
    pthread_key_t k;
    for (;;) {
        if (pthread_key_create(&k, NULL) != 0) die("pthread_key_create failed", 0);
        if (k == MODUS_TSD_KEY) break;
        if (k > MODUS_TSD_KEY) die("the per-thread-window pthread key is already taken", (long)k);
    }
    delta_key = k;
}

// A Linux TID is a positive 32-bit number; Darwin's thread ids are 64-bit
// and sequential.  Keep the low 31 bits (distinct for 2^31 consecutive
// threads) and never answer 0, which the join reads as "exited".
static long lx_tid(uint64_t id) {
    long t = (long)(id & 0x7FFFFFFF);
    return t ? t : 1;
}
static long my_tid(void) {
    if (pthread_main_np()) return getpid();     // Linux: the group leader's TID is the PID
    uint64_t id = 0;
    pthread_threadid_np(NULL, &id);
    return lx_tid(id);
}

struct clone_start { struct resume_ctx ctx; void *delta; uint32_t *ctid; };

static void *clone_child(void *arg) {
    struct clone_start cs = *(struct clone_start *)arg;
    free(arg);
    is_clone = 1;
    my_ctid = cs.ctid;
    pthread_setspecific(delta_key, cs.delta);   // the parent's window, as Linux inherits TPIDR_EL0
    // sigaltstack is per thread: the fault handler needs one here too.
    stack_t ss = { .ss_sp = malloc(1 << 16), .ss_size = 1 << 16, .ss_flags = 0 };
    sigaltstack(&ss, NULL);
    modus_resume(&cs.ctx);
}

static long dx_clone(long flags, long newsp, uint32_t *ptid, uint32_t *ctid,
                     struct stub_frame *f) {
    if ((flags & 0x10000) == 0) return -38;     // only CLONE_THREAD: fork is not a clone here
    struct clone_start *cs = calloc(1, sizeof *cs);
    for (int i = 1; i <= 15; i++) cs->ctx.x[i] = f->x1_15[i - 1];
    cs->ctx.x[17] = f->x17;
    for (int i = 19; i <= 28; i++) cs->ctx.x[i] = f->x19_28[i - 19];
    cs->ctx.x[29] = f->x29;
    cs->ctx.x[30] = f->x30;
    cs->ctx.x[0] = 0;
    // The stub returns to the instruction after its BLR, which pops the saved
    // LR: start 16 below the new stack so that pop leaves SP where clone put it.
    cs->ctx.sp = (uint64_t)newsp - 16;
    cs->ctx.pc = f->x30;
    cs->delta = pthread_getspecific(delta_key);
    cs->ctid = (flags & 0x200000) ? ctid : NULL;
    pthread_attr_t at;
    pthread_attr_init(&at);
    pthread_attr_setdetachstate(&at, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&at, 1 << 18);
    pthread_t th;
    int e = pthread_create(&th, &at, clone_child, cs);
    pthread_attr_destroy(&at);
    if (e) { free(cs); return -lx_errno(e); }
    uint64_t id = 0;
    pthread_threadid_np(th, &id);
    long tid = lx_tid(id);
    // PARENT_SETTID and CHILD_CLEARTID name the same word here; Linux writes it
    // before clone returns in the parent, and so must we — the join polls it.
    if (flags & 0x100000) __atomic_store_n(ptid, (uint32_t)tid, __ATOMIC_SEQ_CST);
    return tid;
}

static void thread_exit(void) {
    if (my_ctid) {
        __atomic_store_n(my_ctid, 0, __ATOMIC_SEQ_CST);
        os_sync_wake_by_address_all(my_ctid, 4, OS_SYNC_WAKE_BY_ADDRESS_NONE);
    }
    pthread_exit(NULL);
}

// futex(uaddr, op, val, timeout): WAIT and WAKE, private or not.
static long dx_futex(uint32_t *ua, long op, long val, const struct timespec *to) {
    switch (op & 0x7F) {
    case 0: {                                   // FUTEX_WAIT
        if (__atomic_load_n(ua, __ATOMIC_SEQ_CST) != (uint32_t)val) return -11;   // EAGAIN
        int r;
        if (to) {
            uint64_t ns = (uint64_t)to->tv_sec * 1000000000ULL + (uint64_t)to->tv_nsec;
            if (ns == 0) return -110;
            r = os_sync_wait_on_address_with_timeout(ua, (uint64_t)(uint32_t)val, 4,
                    OS_SYNC_WAIT_ON_ADDRESS_NONE, OS_CLOCK_MACH_ABSOLUTE_TIME, ns);
        } else {
            r = os_sync_wait_on_address(ua, (uint64_t)(uint32_t)val, 4, OS_SYNC_WAIT_ON_ADDRESS_NONE);
        }
        if (r >= 0) return 0;
        return errno == ETIMEDOUT ? -110 : errno == EINTR ? -4 : 0;
    }
    case 1: {                                   // FUTEX_WAKE
        if (val <= 0) return 0;
        int r = val == 1 ? os_sync_wake_by_address_any(ua, 4, OS_SYNC_WAKE_BY_ADDRESS_NONE)
                         : os_sync_wake_by_address_all(ua, 4, OS_SYNC_WAKE_BY_ADDRESS_NONE);
        return r == 0 ? 1 : 0;                  // ENOENT: nobody was waiting
    }
    default: return -38;
    }
}

static unsigned char unknown_seen[512];

static long modus_syscall_1(long a0, long a1, long a2, long a3, long a4, long a5, long nr,
                            struct stub_frame *frame);
static int strace_on = -1;
// MODUS_SHIM_STRACE=1: every translated call and its result, on stderr.
long modus_syscall(long a0, long a1, long a2, long a3, long a4, long a5, long nr,
                   struct stub_frame *frame) {
    if (strace_on < 0) strace_on = getenv("MODUS_SHIM_STRACE") != NULL;
    long r = modus_syscall_1(a0, a1, a2, a3, a4, a5, nr, frame);
    if (strace_on)
        fprintf(stderr, "[%ld] sys %ld(%#lx, %#lx, %#lx, %#lx) = %ld\n",
                my_tid(), nr, a0, a1, a2, a3, r);
    return r;
}

static long modus_syscall_1(long a0, long a1, long a2, long a3, long a4, long a5, long nr,
                            struct stub_frame *frame) {
    struct stat st;
    switch (nr) {
    case 63:  RET(jit_read((int)a0, (void *)a1, (size_t)a2));
    case 64:  RET(write((int)a0, (const void *)a1, (size_t)a2));
    case 56:  RET(openat(dx_dirfd(a0), (const char *)a1, dx_open_flags(a2), (int)a3));
    case 57:  forget_dir(a0); RET(close((int)a0));
    case 61:  return dx_getdents64(a0, (uint8_t *)a1, a2);
    case 62:  RET(lseek((int)a0, (off_t)a1, (int)a2));
    case 79: {                                     // newfstatat
        if (fstatat(dx_dirfd(a0), (const char *)a1, &st, dx_at_flags(a3)) < 0) return -lx_errno(errno);
        to_lx_stat(&st, (struct lx_stat *)a2); return 0; }
    case 80: {                                     // fstat
        if (fstat((int)a0, &st) < 0) return -lx_errno(errno);
        to_lx_stat(&st, (struct lx_stat *)a1); return 0; }
    case 222: return dx_mmap(a0, a1, a2, a3, a4, a5);
    case 215: RET(munmap((void *)a0, (size_t)a1));
    case 226: RET(mprotect((void *)a0, (size_t)a1, (int)a2));
    case 93: if (is_clone) thread_exit(); _exit((int)a0);   // exit: the THREAD
    case 94: _exit((int)a0);                                // exit_group
    case 220: return dx_clone(a0, a1, (uint32_t *)a2, (uint32_t *)a4, frame);
    case 98:  return dx_futex((uint32_t *)a0, a1, a2, (const struct timespec *)a3);
    case LX_SYS_SET_THREAD_DELTA: pthread_setspecific(delta_key, (void *)a0); return 0;
    case 113: RET(clock_gettime(dx_clock(a0), (struct timespec *)a1));
    case 169: RET(gettimeofday((struct timeval *)a0, NULL));
    case 101: RET(nanosleep((const struct timespec *)a0, (struct timespec *)a1));
    case 124: RET(sched_yield());
    case 172: return getpid();
    case 178: return my_tid();                     // gettid
    case 17: {                                     // getcwd: Linux returns the length
        if (!getcwd((char *)a0, (size_t)a1)) return -lx_errno(errno);
        return (long)strlen((char *)a0) + 1; }
    case 49:  RET(chdir((const char *)a0));
    case 34:  RET(mkdirat(dx_dirfd(a0), (const char *)a1, (mode_t)a2));
    case 35:  RET(unlinkat(dx_dirfd(a0), (const char *)a1, dx_at_flags(a2)));
    case 38:  RET(renameat(dx_dirfd(a0), (const char *)a1, dx_dirfd(a2), (const char *)a3));
    case 48:  RET(faccessat(dx_dirfd(a0), (const char *)a1, (int)a2, 0));
    case 78:  RET(readlinkat(dx_dirfd(a0), (const char *)a1, (char *)a2, (size_t)a3));
    case 46:  RET(ftruncate((int)a0, (off_t)a1));
    case 23:  RET(dup((int)a0));
    case 29:  return -25;                          // ioctl: ENOTTY (no terminal control yet)
    case 278: arc4random_buf((void *)a0, (size_t)a1); return a1;   // getrandom
    case 214: return -12;                          // brk: ENOMEM (the image mmaps)
    case 134: return dx_rt_sigaction(a0, (struct lx_sigaction *)a1, (struct lx_sigaction *)a2);
    case 135: return dx_rt_sigprocmask(a0, (const uint64_t *)a1, (uint64_t *)a2);
    case 129: RET(kill((pid_t)a0, dx_sig(a1)));
    case 198: return dx_socket(a0, a1, a2);
    case 200: case 203: {                          // bind, connect
        struct sockaddr_storage ss; socklen_t n = dx_sockaddr((const uint8_t *)a1, (socklen_t)a2, &ss);
        if (nr == 200) RET(bind((int)a0, (struct sockaddr *)&ss, n));
        RET(connect((int)a0, (struct sockaddr *)&ss, n)); }
    case 201: RET(listen((int)a0, (int)a1));
    case 202: case 242: {                          // accept, accept4
        struct sockaddr_storage ss; socklen_t n = sizeof ss;
        int fd = accept((int)a0, (struct sockaddr *)&ss, &n);
        if (fd < 0) return -lx_errno(errno);
        lx_sockaddr(&ss, (uint8_t *)a1, (uint32_t *)a2, n);
        int one = 1; setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
        return fd; }
    case 204: case 205: {                          // getsockname, getpeername
        struct sockaddr_storage ss; socklen_t n = sizeof ss;
        if ((nr == 204 ? getsockname : getpeername)((int)a0, (struct sockaddr *)&ss, &n) < 0) return -lx_errno(errno);
        lx_sockaddr(&ss, (uint8_t *)a1, (uint32_t *)a2, n); return 0; }
    case 206: {                                    // sendto
        struct sockaddr_storage ss; socklen_t n = a4 ? dx_sockaddr((const uint8_t *)a4, (socklen_t)a5, &ss) : 0;
        RET(sendto((int)a0, (const void *)a1, (size_t)a2, (int)(a3 & 0x1), a4 ? (struct sockaddr *)&ss : NULL, n)); }
    case 207: {                                    // recvfrom
        struct sockaddr_storage ss; socklen_t n = sizeof ss;
        long r = recvfrom((int)a0, (void *)a1, (size_t)a2, (int)(a3 & 0x2 ? MSG_PEEK : 0),
                          a4 ? (struct sockaddr *)&ss : NULL, a4 ? &n : NULL);
        if (r < 0) return -lx_errno(errno);
        if (a4) lx_sockaddr(&ss, (uint8_t *)a4, (uint32_t *)a5, n);
        return r; }
    case 208: case 209: {                          // setsockopt, getsockopt
        int lvl; int opt = dx_sockopt(a1, a2, &lvl);
        if (opt < 0) return nr == 208 ? 0 : -92;   // unknown option: accept a set, ENOPROTOOPT a get
        if (nr == 208) RET(setsockopt((int)a0, lvl, opt, (const void *)a3, (socklen_t)a4));
        socklen_t n = a4 ? *(uint32_t *)a4 : 0;
        if (getsockopt((int)a0, lvl, opt, (void *)a3, &n) < 0) return -lx_errno(errno);
        if (a4) *(uint32_t *)a4 = n;
        return 0; }
    case 210: RET(shutdown((int)a0, (int)a1));      // SHUT_* agree
    case 103: return 0;                            // setitimer
    default:
        if (nr >= 0 && nr < 512 && !unknown_seen[nr]) {
            unknown_seen[nr] = 1;
            fprintf(stderr, "modus-shim: unimplemented Linux syscall %ld\n", nr);
        }
        return -38;                                // ENOSYS
    }
}

// ---------------------------------------------------------- diagnostics ---
// Until the image installs its own handlers on Darwin (M2), a fault would
// die silently.  Print where it happened: PC and the registers, as offsets
// into the image where they point at it, so a symbol map can name them.
#include <sys/ucontext.h>
static uint64_t g_code_lo, g_code_hi;
static void pr_reg(const char *n, uint64_t v) {
    if (v >= g_code_lo && v < g_code_hi)
        fprintf(stderr, " %s=%#llx(+%#llx)", n, (unsigned long long)v, (unsigned long long)(v - g_code_lo));
    else fprintf(stderr, " %s=%#llx", n, (unsigned long long)v);
}
static void report_fault(int sig, siginfo_t *si, void *uc_);
static __thread uint64_t last_flip_pc; static __thread int same_pc_flips;   // per thread: W^X is

static void on_fault(int sig, siginfo_t *si, void *uc_) {
    ucontext_t *uc = uc_;
    uint64_t pc = __darwin_arm_thread_state64_get_pc(uc->uc_mcontext->__ss);
    uint64_t a = (uint64_t)si->si_addr;
    if ((sig == SIGBUS || sig == SIGSEGV) && in_jit(a)) {
        // A fetch from the JIT arena in write mode (pc == fault address), or
        // a write to it in exec mode.  Flip and retry.  JIT code WRITING the
        // arena would ping-pong (each flip re-faults its own fetch): stop
        // loudly rather than livelock.
        if (pc == last_flip_pc && ++same_pc_flips > 8) {
            fprintf(stderr, "\nmodus-shim: JIT code writes JIT memory at pc=%#llx (cannot run under MAP_JIT)\n",
                    (unsigned long long)pc);
            report_fault(sig, si, uc_);
        }
        if (pc != last_flip_pc) { last_flip_pc = pc; same_pc_flips = 0; }
        pthread_jit_write_protect_np(a == pc ? 1 : 0);
        return;
    }
    int slot = fault_slot(sig);
    // MODUS_SHIM_FAULTS=1: say where every fault the image recovers from was.
    static int show = -1;
    if (show < 0) show = getenv("MODUS_SHIM_FAULTS") != NULL;
    if (show) {
        __typeof__(uc->uc_mcontext->__ss) *ts = &uc->uc_mcontext->__ss;
        fprintf(stderr, "modus-shim: [%ld] signal %d", my_tid(), sig);
        pr_reg("pc", pc);
        fprintf(stderr, " addr=%p", si->si_addr);
        pr_reg("lr", __darwin_arm_thread_state64_get_lr(*ts));
        pr_reg("sp", __darwin_arm_thread_state64_get_sp(*ts));
        fprintf(stderr, "\n");
    }
    if (slot >= 0 && image_fault_handler[slot].handler > 1) {
        // The image's handler: a stub that ignores its arguments and branches
        // into the armed handler-case frame, never returning.
        ((void (*)(int, siginfo_t *, void *))(uintptr_t)image_fault_handler[slot].handler)
            (sig == SIGBUS ? 7 : 11, si, uc_);
    }
    report_fault(sig, si, uc_);
}

static void report_fault(int sig, siginfo_t *si, void *uc_) {
    ucontext_t *uc = uc_;
    __typeof__(uc->uc_mcontext->__ss) *ts = &uc->uc_mcontext->__ss;
    fprintf(stderr, "\nmodus-shim: signal %d at", sig);
    pr_reg("pc", __darwin_arm_thread_state64_get_pc(*ts));
    fprintf(stderr, " fault-addr=%p\n ", si->si_addr);
    for (int i = 0; i < 29; i++) { char n[8]; snprintf(n, sizeof n, "x%d", i); pr_reg(n, ts->__x[i]); if (i % 4 == 3) fprintf(stderr, "\n "); }
    pr_reg("fp", __darwin_arm_thread_state64_get_fp(*ts));
    pr_reg("lr", __darwin_arm_thread_state64_get_lr(*ts));
    pr_reg("sp", __darwin_arm_thread_state64_get_sp(*ts));
    fprintf(stderr, "\n");
    _exit(128 + sig);
}
static void install_fault_report(void) {
    static uint8_t altstack[1 << 16];
    stack_t ss = { .ss_sp = altstack, .ss_size = sizeof altstack, .ss_flags = 0 };
    sigaltstack(&ss, NULL);
    struct sigaction sa; memset(&sa, 0, sizeof sa);
    // SA_NODEFER: a chained image handler never returns (it branches into a
    // handler-case frame), so the signal must not stay blocked afterwards.
    sa.sa_sigaction = on_fault; sa.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_NODEFER;
    int sigs[] = { SIGSEGV, SIGBUS, SIGILL, SIGTRAP, SIGFPE };
    for (unsigned i = 0; i < sizeof sigs / sizeof *sigs; i++) sigaction(sigs[i], &sa, NULL);
}

// ------------------------------------------------------------------ ELF ---
struct elf64_ehdr { unsigned char ident[16]; uint16_t type, machine; uint32_t version;
                    uint64_t entry, phoff, shoff; uint32_t flags; uint16_t ehsize, phentsize,
                    phnum, shentsize, shnum, shstrndx; };
struct elf64_phdr { uint32_t type, flags; uint64_t offset, vaddr, paddr, filesz, memsz, align; };

int main(int argc, char **argv, char **envp) {
    unsigned long size = 0;
    uint8_t *img = getsectiondata(&_mh_execute_header, "__TEXT", "__modus", &size);
    if (!img || size < 64) die("no embedded image (__TEXT,__modus)", 0);
    if ((uintptr_t)img & (PAGE16K - 1)) die("embedded image is not 16 KB aligned", (long)(uintptr_t)img);
    const struct elf64_ehdr *eh = (const void *)img;
    if (memcmp(eh->ident, "\177ELF", 4) || eh->machine != 183) die("not an aarch64 ELF", 0);
    const struct elf64_phdr *ph = (const void *)(img + eh->phoff);
    if (ph->type != 1 || ph->offset != 0) die("unexpected program header", ph->type);

    // 1. the code: remap our own signed pages to the link address.
    mach_vm_address_t code = ph->vaddr;
    mach_vm_size_t file_span = ROUND_UP(ph->filesz, PAGE16K);
    vm_prot_t cur = 0, max = 0;
    kern_return_t kr = mach_vm_remap(mach_task_self(), &code, file_span, 0,
                                     VM_FLAGS_FIXED, mach_task_self(),
                                     (mach_vm_address_t)(uintptr_t)img, FALSE,
                                     &cur, &max, VM_INHERIT_NONE);
    if (kr != KERN_SUCCESS || code != ph->vaddr) die("mach_vm_remap of the image code failed", kr);
    // p_memsz slack past the file (a page of BSS on a high-linked image).
    if (ph->memsz > file_span) {
        mach_vm_address_t tail = ph->vaddr + file_span;
        kr = mach_vm_allocate(mach_task_self(), &tail, ROUND_UP(ph->memsz - file_span, PAGE16K), VM_FLAGS_FIXED);
        if (kr != KERN_SUCCESS) die("could not map the image's BSS tail", kr);
    }

    // 2. the syscall slot, one 16 KB page below the code base.
    mach_vm_address_t slot = ph->vaddr - PAGE16K;
    kr = mach_vm_allocate(mach_task_self(), &slot, PAGE16K, VM_FLAGS_FIXED);
    if (kr != KERN_SUCCESS) die("could not map the syscall slot page", kr);
    *(void **)(uintptr_t)slot = (void *)modus_syscall_stub;

    // 3. a Linux initial stack: argc, argv..., NULL, envp..., NULL, AT_NULL.
    size_t stack_size = 64ULL << 20;
    uint8_t *stack = mmap(NULL, stack_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (stack == MAP_FAILED) die("could not map the image stack", errno);
    int envc = 0; while (envp[envc]) envc++;
    size_t words = 1 + (size_t)argc + 1 + (size_t)envc + 1 + 2;
    uint64_t *sp = (uint64_t *)(stack + stack_size - ROUND_UP(words * 8, 16) - 64);
    sp = (uint64_t *)((uintptr_t)sp & ~15ULL);
    // Copy every argv/envp string to a 16-byte-aligned home.  Some runtime
    // paths read a char* back through a fixnum-shaped load, (* 2 (mem-ref
    // ... :u64)), which drops bit 0 — the restore path reads argv[2] that way
    // (the same class 455f7780 fixed for getenv).  Linux happened to hand us
    // even addresses; macOS does not.
    char **args = calloc((size_t)argc + (size_t)envc + 1, sizeof *args);
    for (int i = 0; i < argc + envc; i++) {
        const char *src = i < argc ? argv[i] : envp[i - argc];
        size_t n = strlen(src) + 1;
        char *dst = aligned_alloc(16, ROUND_UP(n, 16));
        memcpy(dst, src, n);
        args[i] = dst;
    }
    size_t k = 0;
    sp[k++] = (uint64_t)argc;
    for (int i = 0; i < argc; i++) sp[k++] = (uint64_t)(uintptr_t)args[i];
    sp[k++] = 0;
    for (int i = 0; i < envc; i++) sp[k++] = (uint64_t)(uintptr_t)args[argc + i];
    sp[k++] = 0;
    sp[k++] = 0; sp[k++] = 0;                      // auxv: AT_NULL

    g_code_lo = ph->vaddr; g_code_hi = ph->vaddr + ph->memsz;
    reserve_delta_key();
    install_fault_report();
    modus_enter((uint64_t)(uintptr_t)sp, eh->entry);
}
