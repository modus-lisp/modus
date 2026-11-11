// modus-audio-none.c — the speaker on a watchOS SDK with no AudioToolbox (watchOS 27's has none):
// pseudo-syscalls 1010-1012 answer ENOSYS, as an unknown call does, and the image plays nothing.
#include <stdint.h>
int64_t modus_audio_call(int64_t nr, int64_t a0, int64_t a1) {
    (void)nr; (void)a0; (void)a1;
    return -38;
}
