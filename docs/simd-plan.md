# Bringing mill's vector capability to Modus

`github.com/modus-lisp/mill` runs ONNX graphs in pure CL at 2.7× realtime on
one EPYC core. Most of that came from a SIMD layer that is deliberately
tiny (`src/simd.lisp`): seven operations and a lane count —

    +f32-lanes+            4 on NEON, 8 on AVX2, 1 for the scalar reference
    f32v-pack              the vector type (float32x4_t / f32.8 / single-float)
    f32v-ref a i           unaligned lane load, SETF-able → lane store
    f32v-broadcast x       one scalar in every lane
    f32v-broadcast-ref a i element I of A in every lane
    f32v+ f32v- f32v* f32v/
    f32v-done              VZEROUPPER on x86, nothing elsewhere
    do-vectorized (iv is n) vector-body scalar-body

— with one admission rule: an operation must be a single NEON instruction
(`vld1q vdupq vaddq vsubq vmulq vdivq`). Every kernel is written in those
names, the lanes=1 arm is the same source with the width turned down (it
reproduces chord's 205,568 samples byte for byte against the AVX2 build),
and there is deliberately no FMA because it changes summation order. A port
is those seven names plus a constant; the kernels do not change.

On SBCL the seven names are macros over `sb-simd-avx2`. That is the only
SBCL-specific part, and it is exactly the part Modus has to supply.

## What Modus has today

- A single-float is a **boxed** heap object (subtag `#x64`, four slots
  sharing the double's IEEE payload). There is no unboxed float anywhere.
- `:fadd :fsub :fmul :fdiv :ftoi :fcmp` exist in the ISA, and each
  **allocates** its result (x64 lowers to ADDSD etc. on the payload, then
  boxes). So `(* weight x)` in a loop is an allocation per iteration; the
  Pi Zero measurements show what heap allocation in an inner loop costs.
- `(make-array n :element-type 'single-float)` is a generic **word array of
  boxed floats**. There is no packed 4-bytes-per-element float store; the
  packed kinds are u8 (subtag `#x11`) and word arrays.
- The register file is fully allocated (GPRs V0–V8 physical, V9–V15 spill),
  and the register-promotion experiment of 2026-09-10 showed the GPR pool
  cannot spare anything. The **vector unit is untouched**: q0–q31 on
  aarch64, xmm0–15 on x64 are free.

So mill's *scalar* arm would already run allocation-bound here, and the
vector arm needs storage, registers and instructions that do not exist.
Three layers, each useful on its own and each a prerequisite for the next.

## Layer 1 — packed single-float arrays and unboxed float locals

1. **Storage.** A new packed subtag for `(simple-array single-float (*))`,
   4 bytes per element, GC-opaque like u8 (no pointer slots). `make-array`
   with `:element-type 'single-float` allocates it; `aref`/`aset` on a
   declared `(simple-array single-float …)` variable compile to a raw
   32-bit load/store (`%f32-ref`/`%f32-set`), the same shape as the u8 fast
   path. The generic AREF/ASET dispatch learns the subtag so undeclared
   access still works. Double-float arrays can follow later (8-byte lanes);
   mill and chord/stave are f32 throughout.
2. **Unboxed float values.** A binding declared `single-float` (or inferred
   from an `%f32-ref`) is an **unboxed** value: on aarch64 it lives in an
   `s`/`d` register or a frame slot as raw bits; arithmetic on two unboxed
   floats emits `fmul s, s, s` with no allocation. Boxing happens only when
   the value escapes (stored into a generic place, passed to an undeclared
   callee, returned). This is the float twin of the fixnum typed tier that
   took reel from 3 to 60 fps, and it is what makes mill's scalar arm — the
   correctness reference every kernel is checked against — run at a speed
   worth having. Gate: mill's lanes=1 build on Modus x64 must reproduce its
   golden waveform, and the ANSI float chapters must stay green.

## Layer 2 — vector registers and lane operations in the ISA

1. **Register class.** A second register file in the ISA: `Q0–Q7`
   (128-bit). They never hold pointers, so the GC ignores them; they are
   caller-clobbered, so a vector value lives in a Q register only inside a
   call-free region (the `do-vectorized` body, which is how mill's kernels
   are written anyway) and is otherwise spilled to a 16-byte frame slot
   pair. Vector temps do not compete with the GPR temp pool — the reason
   GPR promotion lost — because they are a separate pool.
2. **Opcodes** (element kinds encoded in the opcode, 128-bit lanes):

       :vld  q, base, idx      lane load, unaligned (f32x4 / s32x4 / s16x8 / u8x16)
       :vst  q, base, idx      lane store
       :vdup q, gpr            broadcast (f32 from an unboxed float; ints from a fixnum)
       :vadd :vsub :vmul :vdiv q, q, q   per element kind (no FMA, by mill's rule)
       :vmovl / :vmovn          widen u8→s16, s16→s32 and narrow with saturation
       :vmla                    multiply-accumulate (integer only; needed by reel)
       :vext / :vshr            lane extract and shift-right-narrow

   The first eight are mill's contract; the integer widen/narrow/MLA set is
   what VP8's six-tap filter (u8 taps × s16 coefficients, `>> 7`, clamp) and
   the IDCT need, and they are also single NEON instructions (`uxtl`,
   `sqrshrun`, `mla`, `ext`).
3. **Back-ends.** aarch64: direct NEON encodings (the translator already
   has the scalar `fadd d` family; the vector forms are the same encoding
   family with the `Q` bit). x64: SSE2/SSE4.1 for 128-bit lanes
   (`movups addps mulps pmullw pmaddwd packuswb`), no VEX so no
   transition-penalty story to manage. The **interpreter** implements each
   opcode over a 16-byte scratch: that is the lanes=1 reference in the
   Modus world, and it is what the ANSI/JIT differential harness compares
   against. Other translators (riscv, ppc, i386, 68k, arm32) reject the
   opcodes → interpret, as they do for unknown ops today.
4. **Compiler surface.** Primitive forms `(%vld.f32x4 a i)`,
   `(setf (%vld.f32x4 a i) q)`, `(%vdup.f32 x)`, `(%vadd.f32x4 p q)` …
   compile to the opcodes; a LET binding whose init is a vector primitive
   (or declared `(vector-pack f32 4)`) gets a `:qreg` binding. The safety
   walker from the promotion work (no calls / no NLX in the body) decides
   whether the binding can stay in a register for the body's extent.

### Layer 2b, refined from reel's kernels (2026-09-16)

The A53 per-phase profile of the eager core (docs/videocore-hvs-notes.md)
puts 62% of an inter frame in three kernels — IDCT+add-residual 30 ms, the
loop filter 40 ms, MC 18 ms of 141 — so the integer lane set is chosen
from those three bodies (`transform.lisp`, `intra.lisp add-residual`,
`loopfilter.lisp %edge-*`, `inter.lisp mc-filter`), each op one NEON
instruction, each with an interpreter arm over a 16-byte scratch:

    lane kinds        u8x8 u8x16 s8x16 s16x4 s16x8 (s32x4 for the IDCT products)
    memory            :vld8 :vld16 :vst8 :vst16   u8 array + index, unaligned
                      (:vld4/:vst4 for add-residual's 4-pixel rows)
    broadcast         :vdup.s16 gpr -> s16x8, :vdup.u8 gpr -> u8x16
    widen / narrow    :uxtl u8x8->s16x8, :sxtl s16x4->s32x4
                      :sqxtun s16x8->u8x8 (saturate to 0..255)
                      :sqrshrun s16x8->u8x8 #n (round, shift, saturate: the
                        `(max 0 (min 255 (ash (+ x 64) -7)))` of mc-filter)
                      :xtn s32x4->s16x4
    arithmetic        :vadd :vsub (s16, s32, u8)  :vmul :vmla :vmls s16x8 by
                      a broadcast scalar (the six taps)  :sqdmulh s16 by
                      constant (IDCT 35468/20091 with the >>16)  :vshr/:vshl
                      immediate  :sqadd :sqsub s8x16 (loop filter adjust)
    compare / select  :uabd u8  :cmhs/:cmhi u8 (masks)  :vand :vorr :veor
                      :vbsl  (the loop filter's %edge-ok / %hev gates as
                      lane masks; `eor #x80` is the signed/unsigned trick)
    shuffle           :trn1/:trn2 :zip1/:zip2 s16x4 (the 4x4 IDCT transpose)

Register model for the first cut: Q0–Q7 caller-clobbered; a LET binding
whose init is a vector primitive is a `:qreg` binding and stays in a Q
register while its scope is call-free (the promotion work's safety
walker), else it spills to a 16-byte frame slot pair; vector expression
trees allocate Q temps depth-first.  The loop filter needs ~10 live
vectors per edge, so spills are part of the first cut, not later.
Kernel order by measured weight: mc-filter (self-contained, biggest
single expression), add-residual + IDCT, then the three %edge-* kernels.
Gate for each: reel's YSUM bit-exact against the scalar build, and the
JIT-vs-interpret differential on the vector probes.

## Layer 3 — mill on Modus, and reel

- `mill/src/simd.lisp` gains a `#+modus` arm: `+f32-lanes+` 4, and the
  seven macros expand to the Layer-2 primitives. `f32v-done` is `nil`.
  Nothing else in mill changes; its node-by-node golden compare is the
  acceptance test. The scalar arm on Modus is Layer 1 alone.
- reel's `mc-filter`, `%edge-*` and `vp8-idct` get integer-lane versions
  written in the same discipline (a vector body and a scalar remainder from
  one expression), which is where the Pi Zero's 5 fps becomes real-time:
  the six-tap filter is 16 pixels per instruction group instead of one
  pixel per ~40 memory-bound instructions.

## Order and cost

| step | what | scope | gate |
|---|---|---|---|
| 1a | packed f32 arrays | tags, make-array, aref/aset fast path, GC skip, printer | ANSI arrays chapter; probe |
| 1b | unboxed float locals + fmul/fadd without boxing | compiler typed tier, aarch64+x64 float regs | ANSI float chapters; mill lanes=1 golden |
| 2a | Q register class + f32x4 ops (mill's eight) | mvm.lisp, interp, translate-aarch64, translate-x64, compiler | JIT-vs-interpret differential on vector probes |
| 2b | integer lanes + widen/narrow/MLA | same files | reel bit-exact (YSUM) with vector kernels |
| 3 | mill `#+modus` arm; reel vector kernels | library repos | mill golden compare; reel YSUM; Pi 5 and Zero timings |

Step 1a is a day; 1b is the largest single piece (it touches the typed
arithmetic tier and both translators' register conventions); 2a is
mechanical once 1b's unboxed-float register story exists; 2b is a bounded
set of encodings. Nothing here is x64-specific and nothing needs FFI, which
keeps the "no C underneath" property that mill and Modus both hold.

Recommendation: do 1a and 1b first and measure mill's scalar arm on Modus
against SBCL's scalar arm. That number says how much of the gap is boxing
(likely most of it) before any vector instruction exists, and it makes the
Layer-2 gain measurable as a ratio against a real baseline, the same way
reel was taken from 3 to 60 fps: profile, name the cost, remove it.

### Layer 2b — LANDED and first kernel bit-exact (2026-09-16)

The integer-lane ISA is in (`ab6a927`): opcodes `#xE2`–`#xEA`, interpreter
reference arms, assembler-verified NEON encodings (aarch64-linux-gnu-as), the
`%vi-*` compiler primitives, and the `VI-PACK` register-resident LET class on
the existing FP vregs (v2..v7).  A vector value cannot escape (compile error);
x64 signals so the module falls to the interpreter reference.  `tests/simd/
vi-probe.lisp`: 13 probes pass on the Pi 5 both JIT (NEON) and MODUS_NO_JIT=1.

First kernel: reel's six-tap `mc-filter` (`reel/src/decode/inter-neon.lisp`,
`#+modus`, last-defun-wins; commit reel `edef631`).  **Bit-exact with the
scalar kernel across 1152 cases** (every fx/fy, block sizes 16x16/8x8/4x4/8x4/
4x8/16x8, random + extreme + ramp data).  A76 both-axes x4000: 16x16 395->260
ms (1.52x), 8x8 291->248, 4x4 261->244.  The A76 is out-of-order so the scalar
path was already well-scheduled; the in-order A53 (the Zero, the target) should
gain more — a fresh reel core with inter-neon + a phase re-profile is the owed
measurement.  The kernel widens u8->s16->s32 and multiplies in s32 (two halves
per 8 pixels); a smlal-based accumulate (s16xs16->s32 widening MLA) would be
fewer instructions — an optimisation, the current form is the correctness
baseline.  NEXT by profile weight: add-residual + vp8-idct (30 ms/frame), then
the loop-filter %edge-* kernels (40 ms).  ANSI gate still owed on ab6a927
before any of Layer 2b nears main.

### Layer 2b progress + findings (2026-09-16 late)

Infrastructure DONE and verified: the integer-lane ISA (opcodes #xE2-#xEA plus
smin/smax/umin/umax/abs/sxtl8), the interpreter reference arms, assembler-
verified NEON, %vi-* primitives, the VI-PACK :ivector register class, a 12-wide
vector pool (v2-v7 + v16-v21 / xmm2-7 + xmm8-13, caller-saved; scopes are
call-free), and let* fp/vector bindings (compile-let* now matches compile-let and
carries dtype for cross-references).  Two-arms verified on the Pi 5.

Kernels:
- mc-filter (six-tap MC): bit-exact 1152 cases, 1.5x on the A76.  THE win so far.
- %edge-simple (loop filter, simple mode): bit-exact, but ~1x.  Lesson: it is a
  LIGHT kernel (4 samples, one gate) AND the simple-mode filter, off the hot path
  for normal streams.  Splitting it into a per-8 sub-function added call overhead.

The real loop-filter cost (40 ms/frame) is %edge-mb + %edge-sub — the NORMAL
filter: the interior gate (%edge-ok / %hev) plus multi-weight adjustment (mb: 3
weights over p2..q2).  Heavy like mc-filter, so a real vector payoff, and now
register-feasible with the 12-wide pool.  These are the next targets, then
vp8-idct (needs .4s transpose ops — trn1/2 zip1/2 .4S — not yet in the ISA; the
4x4 transpose between the column and row passes).

MEASUREMENT PITFALL re-learned: a light kernel wrapped in a per-N sub-function
call shows no speedup even when native; inline the lane body into the edge loop.
And always bench inside a native defun (the top-level dotimes is interpreted).

Owed before main: ANSI gate on the compiler changes (bfa461b/e1ad202), and the
A53 measurement of the combined kernels via a fresh reel core.

### Measurement reality (2026-09-16 night) — NEON is fast; the CLI bench lied

Settled with a micro-bench: `(defun vloop (d n) (dotimes (k n) (%vi-st16 d 0
(%vi-add16 (%vi-ld16 d 0) (%vi-dup16 1)))))` runs 5,000,000 iterations in 62 ms
on the Pi 5 = ~3 ns per vi op = genuine native NEON (interpreted would be
~12,000 ms).  So the lane ops are fast and mc-filter's 1.5x was real native.

BUT the loop-filter CLI benchmarks were confounded and must not be trusted:
- `(funcall SYMBOL ...)` calls the interpreter TRAMPOLINE, not the native code,
  even after the function is native — always call the bench fn directly.
- a native caller's baked call to a runtime-defined callee does NOT update when
  the callee later goes native (jit-eager / redefinition invisibility), so a
  `bench -> %edge-sub -> %edge-sub-h8` chain interprets the inner calls (~34 us
  /call, flat scalar vs vector).  The clean measurement is either a single
  inlined function (no runtime cross-calls) or the whole decoder from a jit-eager
  core, where the decode path's calls are all resolved native at build.

### Honest CLI numbers (2026-09-17) — NEON edge-sub beats scalar, 1.65x on A76

`modus:jit-eager` now takes an optional FORM (commit 0808b85): it makes every
registered runtime DEFUN native, THEN compiles+runs FORM with the JIT forced on,
so FORM's call sites bind to the native entries instead of trampolines.  With
that, the loop-filter kernels measure clean on the Pi 5 (A76), 200000 calls:

| kernel                         | time    | per call |
|--------------------------------|---------|----------|
| scalar `%edge-sub`             | ~231 ms | 1.15 us  |
| NEON `%edge-sub` (calls h8)    | ~139 ms | 0.69 us  |
| NEON `%edge-sub-h8` (raw)      | ~129 ms | 0.65 us  |

NEON is **1.65x** scalar, and `%edge-sub` ≈ `%edge-sub-h8` proves the internal
runtime-defun->runtime-defun call binds NATIVE (%jit-retry-drain works) — the
earlier "nested calls interpret" fear was wrong.

TWO measurement artifacts, now controlled, produced every earlier ~1x / ~40 us
result — do not repeat them:
1. an intermediate `bench-*` fn adds a trampoline layer with a ~40 us floor.
   Time the kernel call INLINE in the jit-eager FORM, never via a helper.
2. `jit-eager` re-runs `%jit-eager-all` (translate ALL modules) on EVERY call,
   so the one-time translate cost lands on whichever FORM you time first.  Do a
   bare `(modus:jit-eager)` warmup, THEN time each FORM.
`jit-eager -> (46 46 4)`: the 4 non-native modules are `%INIT-GENERA-COMPAT` and
three ASDF fns (pre-existing, unrelated), NOT the NEON kernels — every reel
kernel translates, translate-err-count 0.

NEXT (the fps that matters): rebuild the reel core with inter-neon +
loopfilter-neon + jit-eager, netboot to the Zero, re-run the A53 phase profile.
The A53 win should exceed the A76's 1.65x (weak scalar, relatively stronger
NEON).  %edge-mb (macroblock edge, 3-weight) is the remaining loop-filter
kernel; vp8-idct needs .4s transpose ops.

Also: jit-eager now lives in the MODUS package (0808b85).  A MODUS runtime
package for the whole CL-USER runtime surface (ssh-boot, net-install-and-call…)
is a worthwhile separate cleanup.
