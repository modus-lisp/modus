// modus-audio-none.c — the speaker on a watchOS SDK with no AudioToolbox (watchOS 27's has none):
// pseudo-syscalls 1010-1012 answer ENOSYS, as an unknown call does, and the image plays nothing.
long modus_audio_call(long nr, long a0, long a1) {
    (void)nr; (void)a0; (void)a1;
    return -38;
}
