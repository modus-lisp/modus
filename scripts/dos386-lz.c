/* dos386-lz.c -- the compressed code store of a demand-paged bare i386 image.
 *
 *   dos386-lz c IN OUT CHUNK   compress IN in CHUNK-byte blocks:
 *                              OUT = u32 n, u32 offsets[n+1] (from the
 *                              data's start), then the blocks
 *   dos386-lz d IN OUT CHUNK   the inverse, decoded exactly as the entry
 *                              stub's decoder does (boot/boot-i386-cl.lisp)
 *
 * The block format is LZ4's: a token byte, its high nibble the literal count
 * and its low nibble the match length - 4 (15 in either: add following bytes
 * until one is not 255), the literals, then -- unless the block ends after
 * the literals -- a 16-bit little-endian offset back into what was decoded.
 * Each block is independent, so one block decodes into one chunk of pages.
 * scripts/dos386-core.py builds the store with it.
 *
 *   cc -O2 -o dos386-lz dos386-lz.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

static uint8_t *slurp(const char *fn, long *n) {
    FILE *f = fopen(fn, "rb");
    if (!f) { perror(fn); exit(1); }
    fseek(f, 0, SEEK_END); *n = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *b = malloc(*n + 1);
    if (fread(b, 1, *n, f) != (size_t)*n) { perror(fn); exit(1); }
    fclose(f);
    return b;
}

static uint8_t *put_len(uint8_t *o, long n) {          /* the 255-run tail of a count */
    while (n >= 255) { *o++ = 255; n -= 255; }
    *o++ = (uint8_t)n;
    return o;
}

#define HBITS 16
static long compress_block(const uint8_t *in, long n, uint8_t *out) {
    static int32_t head[1 << HBITS];
    static int32_t prev[1 << 20];
    for (int i = 0; i < (1 << HBITS); i++) head[i] = -1;
    uint8_t *o = out;
    long i = 0, lit = 0;
#define HASH(p) ((uint32_t)(((uint32_t)in[p] | in[p+1] << 8 | in[p+2] << 16 | (uint32_t)in[p+3] << 24) * 2654435761u) >> (32 - HBITS))
#define INSERT(p) do { if ((p) + 4 <= n) { uint32_t h_ = HASH(p); prev[p] = head[h_]; head[h_] = (int32_t)(p); } } while (0)
    while (i < n) {
        long best = 0, boff = 0;
        if (i + 4 <= n) {
            int32_t c = head[HASH(i)];
            for (int steps = 0; c >= 0 && steps < 64; steps++, c = prev[c]) {
                if (i - c > 65535) break;
                long l = 0;
                while (i + l < n && in[c + l] == in[i + l]) l++;
                if (l > best) { best = l; boff = i - c; }
            }
        }
        if (best >= 4) {
            long m = best - 4;
            *o++ = (uint8_t)((lit >= 15 ? 15 : lit) << 4 | (m >= 15 ? 15 : m));
            if (lit >= 15) o = put_len(o, lit - 15);
            memcpy(o, in + i - lit, lit); o += lit;
            *o++ = (uint8_t)boff; *o++ = (uint8_t)(boff >> 8);
            if (m >= 15) o = put_len(o, m - 15);
            for (long p = i; p < i + best; p++) INSERT(p);
            i += best; lit = 0;
        } else {
            INSERT(i);
            i++; lit++;
        }
    }
    *o++ = (uint8_t)((lit >= 15 ? 15 : lit) << 4);
    if (lit >= 15) o = put_len(o, lit - 15);
    memcpy(o, in + n - lit, lit); o += lit;
    return o - out;
}

static long decompress_block(const uint8_t *in, long n, uint8_t *out) {
    const uint8_t *p = in, *end = in + n;
    uint8_t *o = out;
    for (;;) {
        int t = *p++;
        long l = t >> 4;
        if (l == 15) { int b; do { b = *p++; l += b; } while (b == 255); }
        while (l--) *o++ = *p++;
        if (p >= end) break;
        long off = p[0] | p[1] << 8; p += 2;
        long m = (t & 15);
        if (m == 15) { int b; do { b = *p++; m += b; } while (b == 255); }
        m += 4;
        const uint8_t *s = o - off;
        while (m--) *o++ = *s++;
    }
    return o - out;
}

int main(int argc, char **argv) {
    if (argc != 5) { fprintf(stderr, "usage: dos386-lz c|d IN OUT CHUNK\n"); return 2; }
    long n, chunk = atol(argv[4]);
    uint8_t *in = slurp(argv[2], &n);
    FILE *f = fopen(argv[3], "wb");
    if (!f) { perror(argv[3]); return 1; }
    if (argv[1][0] == 'c') {
        uint32_t nb = (uint32_t)((n + chunk - 1) / chunk);
        uint32_t *off = calloc(nb + 1, 4);
        uint8_t *data = malloc(n + n / 8 + 64 * nb + 64), *d = data;
        for (uint32_t k = 0; k < nb; k++) {
            long len = (k + 1) * chunk <= n ? chunk : n - k * chunk;
            off[k] = (uint32_t)(d - data);
            d += compress_block(in + k * chunk, len, d);
        }
        off[nb] = (uint32_t)(d - data);
        fwrite(&nb, 4, 1, f); fwrite(off, 4, nb + 1, f); fwrite(data, 1, d - data, f);
    } else {
        uint32_t nb = ((uint32_t *)in)[0], *off = (uint32_t *)in + 1;
        uint8_t *data = (uint8_t *)(off + nb + 1);
        uint8_t *buf = malloc(chunk * 2);
        for (uint32_t k = 0; k < nb; k++)
            fwrite(buf, 1, decompress_block(data + off[k], off[k + 1] - off[k], buf), f);
    }
    fclose(f);
    return 0;
}
