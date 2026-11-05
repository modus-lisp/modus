# MVM Self-Hosting Pipeline

## Overview

The self-hosting pipeline produces a bootable Modus kernel in three stages:

```
SBCL + MVM     →  Gen0 (x86-64 ELF, ~1.2MB)
Gen0 QEMU      →  Gen1 (natively compiled, ~13.5MB)
Gen1 QEMU      →  Gen2 (identical to Gen1)
```

SBCL is only needed once. After Gen0 boots, the kernel can recompile itself indefinitely.

## Gen0: SBCL → MVM → x86-64

`build-kernel-mvm` in `build.lisp` compiles all ~940 runtime functions:

1. **Boot preamble**: Multiboot header, 32→64 bit transition, GDT, IDT, paging, serial init, AP trampoline
2. **MVM compilation**: All `*runtime-functions*` forms → MVM bytecode → x86-64 native code
3. **register-builtins**: Generates NFN table entries for ~440 callable functions (uses old cross-compiler for this one function)
4. **Source blob**: Plain s-expression source (~558KB) embedded in the image.  *(2026-11-05: out by default -- `MODUS_SOURCE_BLOB=1` keeps it.  Self-hosting now reads source files, and nothing reads the blob.)*
5. **Metadata**: Preamble size, total size, source offset patched into fixed addresses

The kernel is a flat Multiboot ELF loaded at 0x100000.

### Build command

```bash
sbcl --control-stack-size 64 \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:load-system :modus)' \
     --eval '(modus.build:build-kernel-mvm "/tmp/modus-gen0.elf")' \
     --eval '(quit)'
```

## Gen1: Self-compiled by Gen0

Gen0 boots in QEMU and runs `(build-image)` at the REPL. The runtime's native compiler reads the embedded source blob and compiles all functions into a new kernel image in memory.

### build-image steps

1. **img-init**: Initialize image buffer at 0x08000000
2. **img-emit-boot-preamble**: Copy boot preamble from Gen0
3. **img-compile-all-functions**: Read source, compile each defun via native compiler, emit x86-64 code to image buffer
4. **img-compile-register-builtins**: Generate and compile NFN registration code
5. **img-compile-init-symbol-table**: Generate symbol table initialization
6. **img-compile-kernel-main**: Compile the kernel entry point
7. **img-emit-call-site**: Emit the startup code that calls kernel-main (including register-builtins address store)
8. **img-copy-source-blob**: Copy source into the new image (so Gen1 can repeat)
9. **img-patch-metadata**: Write preamble size, total size, source offset

### Image extraction

The build scripts use QEMU QMP to extract the compiled image from guest memory:

```python
# Connect to QMP, dump guest memory at 0x08000000
pmemsave(val=0x08000000, size=image_size, filename="gen1.elf")
```

### Build script

```bash
./scripts/run-x64-gen1-repl.sh --rebuild
```

This script:
1. Computes source hash for caching
2. Builds Gen0 if needed (or uses cached)
3. Boots Gen0 in QEMU with QMP
4. Sends `(build-image)` to the REPL
5. Waits for completion marker ("D:" followed by image size)
6. Extracts Gen1 via QMP pmemsave
7. Boots Gen1

## Key addresses

| Address | Contents |
|---------|----------|
| 0x100000 | Kernel load address (Multiboot) |
| 0x300000 | Metadata: preamble size, AP trampoline offset |
| 0x330000 | NFN table (2048 slots, 32KB, name hash → code address) |
| 0x4FF000 | Source blob metadata (base, length) |
| 0x4FF080 | Image-compile mode flag |
| 0x4FF0B0 | register-builtins address (for Gen1 call-site) |
| 0x08000000 | Image output buffer (Gen1 being compiled) |
| 0x0A000000 | Image NFN table (used during img-compile) |
| 0x10000000 | Cons heap (moved here to avoid collision with image buffer) |

## Key fixes during implementation

- **Loop/return patching**: `patch-jmp32-at` was using `code-patch-u32` (broken with 3-arg `+` in MVM). Fixed by delegating to `patch-jump`.
- **register-builtins in Gen1**: `img-emit-call-site` didn't store the register-builtins address. Fixed by saving it at 0x4FF0B0 during step 8b and emitting a `mov [0x4FF0B0], addr` in the call-site.
- **smp-copy-trampoline**: Crashed when no trampoline source was present. Fixed with null-source guard.
- **Cons heap collision**: The cons allocator at 0x06000000 overlapped with the image buffer at 0x08000000. Moved cons heap to 0x10000000.

## Verification

Gen1 passes all runtime tests:
- Arithmetic: `(+ 3 7)` → 10, `(* 6 7)` → 42
- Loops: `(loop (return 42))` → 42
- Factorial: 10! → 3628800
- RTC: `(print-time)` → correct UTC timestamp
- Self-hosting: Gen1 can run `(build-image)` to produce Gen2
