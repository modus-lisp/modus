# History rewrite — 2026-08-21

`git filter-repo --strip-blobs-bigger-than 50M` was run on this repository.

## Why

Nine build binaries had been committed by accident over the project's life.
Four exceeded GitHub's hard 100 MB per-file limit and made `git push` fail on a
pre-receive hook, so **no history had ever been pushed** — `origin/main` sat at a
single unrelated commit while 2297 commits of work stayed local:

    modus-ansi-216   124 MB      modus-aa64        57 MB
    modus-ansi-215   124 MB      modus-aa64-try    55 MB
    gate-off         123 MB      modus-aa64-jit    55 MB
    gate-jit         123 MB      modus-aa64-fix    55 MB
                                 modus-aa64-base   55 MB

A later commit had already `git rm`'d them from the working tree, but the blobs
remained in history, which is what GitHub rejects.  Only a history rewrite
removes them.

## Effect

    commits       2297 -> 2296   (one commit became empty and was pruned:
                                  "Untrack the 59MB modus-aa64 build artifact",
                                  which now removed a file that never existed)
    refs           273 -> 272    (refs/remotes/origin/main; filter-repo drops
                                  the remote by design)
    .git          363 MB -> 62 MB
    blobs >50MB      9 -> 0

**404 commit SHAs changed; 2014 were preserved.**  Only commits at or after the
three that introduced a blob were rewritten; everything earlier kept its hash.

The rewrite was safe to perform because none of this history had ever reached
origin — no clone anywhere else could be invalidated by it.

## Why this file exists

This repository is written entirely by an AI agent whose context does not
persist between sessions.  Commit messages and task notes are the only
continuity across those resets, and many of them cite commits by SHA.  404 of
those citations silently stopped resolving the moment this rewrite ran.  The
table below is how a future session recovers them: look up the old hash from an
old note, get the new one.

The mapping is also preserved verbatim, including unchanged entries, in
`.git/filter-repo/commit-map` (local only, not committed).

## Mapping (old -> new), sorted by subject

| old | new | subject |
|-----|-----|---------|
| `b212bc00107d` | `add8fb239ef2` | #201 step 2: hoist the in-image float slot accessors into ONE file |
| `a868e67d2fbe` | `4488caf53f66` | #201: GATE-RESULT — 64-shard NET gate, zero real regressions |
| `ec32bc7d0ca6` | `8d489b76719b` | #201: width-neutral float layout — 4 slots x 16-bit tagged chunks |
| `1446a9bc3e20` | `8529fbbb2337` | #204 step 1: a bare-metal x64 image whose REPL is the REAL CL, not the second Lisp |
| `b5684d7a90ec` | `056fd92be5db` | #204 step 1: a bare-metal x64 image whose REPL is the REAL CL, not the second Lisp |
| `b1630e1d2b05` | `b9f61bfdf23c` | #209 rung 1: bare-metal Raspberry Pi 3B image on the CL/mvm stack |
| `923445812ee3` | `8db746525574` | #209 rung 2: the bare-metal Pi CL image fetches a .tar.gz over real HTTP |
| `b0294c5afa9d` | `165c3c36914e` | #210 rung 1 stage 1: bake translate-aarch64 into the selfhost image (dead code) |
| `904dca04a954` | `f737d534244b` | #210 rung 1 stage 2: co-init the AArch64 translator tables |
| `6d22f05dc672` | `1ec931a31ea2` | #210 rung 1 stage 3: register *target-aarch64* in the image target registry |
| `d7c461d966b4` | `18e7956bb48c` | #210 rung 1 stage 4: modus --compile-aarch64 -- the cross-arch emit |
| `ba08e5cc9d5d` | `76876fe1db20` | #210 rung 1 stage 5: fix the blocker -- LET of a special does not bind in-image |
| `8d9766b07210` | `2a0456bb5fc3` | #212: bare-metal global reads — *ERROR-OUTPUT* was an unwritable fd-2 stream |
| `e88537bfd37c` | `38268febe651` | #215: unresolved calls targeted bytecode offset 0, not the safe stub |
| `d730ff3cebb2` | `cafc0ba5b5af` | #216: :fn-addr unresolved fallback emitted offset 0, not the NIL sentinel |
| `a353aa33e332` | `6f220ac3c572` | #219: unresolved calls target offset 0 — the SSH images' "dhcp-discover hang" |
| `be321a5c110d` | `f59c539091e4` | #220 evidence: aarch64 ANSI range gate 13300..14600 = +28 / -0 |
| `75b929808637` | `7fadd1ae7f06` | #220: aarch64 (mod x y) returned a hard 0 — :mod clobbered its own dividend |
| `04d767d78dfe` | `d63540a098a1` | #221 addendum: +op-percpu-set+ large-offset clobbers its own value via x17; tighten the x9 claims |
| `9ae49526ac22` | `9c1339d16c9f` | #221 evidence: GATE-RESULT-clobber.md — ensure-src sweep, runtime confirmation, full-corpus gate |
| `6441f3980a4b` | `a40bd9874ab3` | #221: four more aarch64 ensure-src scratch clobbers — (consp nil)=T, (atom nil)=NIL, obj-set wild st |
| `078bc0cc22be` | `e983c51d1146` | #240 part 2: a free (declare (special *x*)) must not save/restore |
| `fd2df6e92db9` | `be8e4b63ed75` | #240: dynamic bindings must be restored on non-local exit (unwind-protect) |
| `37f4d405068c` | `885a8c12478d` | #245: the shipping AArch64 CLI never called init-all-globals |
| `db397994e32d` | `af8b1f587b9b` | #254: compile-form dropped ANY array literal of rank > 1 — silently NIL |
| `c95e7ca38d7e` | `62d3bb81a101` | #259: record the i386 zero-init disproof and the copy_object residual |
| `b1eb5ec3aac7` | `dd46a4c43e82` | #263 rung 3: the bare-metal Pi image installs + loads + calls a fetched library |
| `bbae87593cc0` | `18a98d8c18d9` | #266: build-rpi-cl-repl is a THIN TAIL over build-cli-common (-871 lines) |
| `f5f1ac73ceaa` | `73b217472d16` | #271: bare-metal RPi reads its config from firmware; %cli-getenv is REAL |
| `9b7fb0d62321` | `d5a854a0714e` | #272: interpreted (setf (mem-ref A :u8) v) was a silent no-op — three ISA infidelities in interp.lis |
| `b270a41608d3` | `4eb3cb90aa59` | #272: interpreted (setf (mem-ref A :u8) v) was a silent no-op — three ISA infidelities in interp.lis |
| `916a8867e2ba` | `6c22f671cc67` | #273 fault A: zero the BSS window in the boot preamble, delete the enumerated list |
| `1867137f29ef` | `92c9ad4bcf42` | #274: aarch64 alloc zero-init — port the x64 hardening that was never carried over |
| `8efb421384e5` | `4de76c4560cc` | %COND-REG-FIND's package guard must not call %PKG-NAME: TYPEP re-entry |
| `36197e54f1b8` | `0fb202aa2748` | %MVM-GENSYM: make the names collision-proof by construction, not by luck |
| `55c4d2be70a7` | `ce4257f9ff59` | %bignum-to-float: width-parameterise the limb radix (was hardcoded 2^62) |
| `f774459ecbc5` | `e0526ae4feea` | (typep <multi-dimensional array> 'sequence) was T; CLHS says NIL |
| `232c1a315ff5` | `c30ac34548ec` | A lambda/DEFUN parameter declared SPECIAL was bound lexically only |
| `7560f3500f58` | `7f43b50469c1` | A recovered hardware fault escaped every bytecode HANDLER-CASE |
| `bf371dfe1580` | `f30a3f06b1f4` | ASDF's interface over Modus's own loader — not vendored ASDF |
| `f3b42f5e1204` | `d46b32a7f44d` | ASSERT is a macro (places not evaluated) + fix (setf (get …)) puthash arg order |
| `3f6c9939aed5` | `36ff134c995f` | Advertise :LITTLE-ENDIAN in *features* (babel PROGRAM-ERROR root cause) |
| `4493267006f8` | `e6b4dfd4cca3` | Battery: cover all six registries in BOTH directions + the cross-expansion shapes |
| `4bb23eeb379f` | `06b78b4080d5` | Battery: cover all six registries in BOTH directions + the cross-expansion shapes |
| `0bcefa631d5d` | `3f86d0d98689` | Battery: method-combination rows |
| `25f2ecaa792b` | `24bde8a93981` | Build-time DEFMETHOD recorded no key metadata — 7.1.2/7.6.5 were inert for compiled methods |
| `f6f975c2fb6b` | `d03ecb1da3d5` | CATCH/THROW and cross-unit RETURN-FROM truncated to a single value |
| `52efb474803d` | `caa0e48752c5` | CHAR-CODE-LIMIT: define it, and set it to the conformant #x110000 |
| `6ab77eda2463` | `da0efe2b664b` | CHECK F: ratchet mvm/compiler.lisp against CL:GENSYM |
| `b5ae31417dc4` | `979746f404dc` | CLAUDE.md: correct Limitation #7 — defvars were UNBOUND, not NIL |
| `dac536ee45f6` | `137b1b1e934f` | CLAUDE.md: document check-global-inits alongside check-parses |
| `e8bfeb8409f3` | `6404daa4c238` | CLAUDE.md: document the i386 GC arena guard band (B3) |
| `8a6eed73a65a` | `df973b9e6437` | CLAUDE.md: record the i386 convention-slot address class + what unknown-opcode is NOT |
| `941864e3787b` | `204e23182bfb` | CLAUDE.md: record the i386 convention-slot address class + what unknown-opcode is NOT |
| `cb8d9c7fb118` | `8f3673dd4d85` | CLAUDE.md: repoint the macro-table citation after the CLI lineage merge |
| `2987a9c31557` | `4d41713ebb58` | CLAUDE.md: separate the merge verdicts — #261 lands, #260 is blocked on a SIGSEGV |
| `3af5f02c383d` | `a061af4de526` | CLAUDE.md: separate the merge verdicts — #261 lands, #260 is blocked on a SIGSEGV |
| `a8bb7ae33c0d` | `7d84b329cf21` | CLAUDE.md: x64-linux gate build needs 12288 MB, not 2048 |
| `fc9d1a70da3f` | `317e24a8a0c2` | CLOS class + condition registries are per-package (task #241) |
| `467b53ef4815` | `4acaee9dda02` | CLOS generic-function registry is per-package: no more cross-package GF collision |
| `99f0e0bcd0e6` | `01a4354afefc` | CLOS: READTABLE is its own built-in class, not CONS |
| `aa17e6e9c916` | `60b3a49fc0c3` | COERCE of a general character VECTOR to STRING returned its argument |
| `0ab80b0a67b5` | `cfa81b19c005` | Confirm the macro NAME on hash hit — *macro-table* had the same silent- miscompile class the dispatc |
| `abf353c3df13` | `cdde72d97763` | Consolidate the BUILD matrix: one mvm/build.lisp for arch × mode × board |
| `85f678d83d58` | `2543f74398e1` | Consolidate the run configurations: one scripts/run.sh for ~14 QEMU launchers |
| `b2a5f9ccbe22` | `838b4529f45e` | Converge the i386 CLI onto x64's bring-up: 2145 -> 384 lines, one lineage |
| `e3ccd348af37` | `1f76f06085f3` | Cooperative atomic read-modify-write ops + the bordeaux-threads backend prelude |
| `f9f203c314e2` | `4bec13408a56` | DEFSTRUCT :PREDICATE and :COPIER named options were read past in silence |
| `09e54592bdee` | `b3e6dd7eb4eb` | DEFSTRUCT accessors: type-check the instance instead of dereferencing it |
| `d74be35145bb` | `7e66b91bce8d` | DIAG ONLY (bmap-fix-diag): port the MODUS_ALEX_DIAG harness onto current gc.lisp |
| `abaccc61ccf9` | `27968672341e` | DIAG ONLY (maindiag): main + the SAME ported MODUS_ALEX_DIAG harness |
| `35e413887379` | `879abdacbd22` | DIAGNOSTIC ONLY (#262): interpreter PC-history + compile-time bytecode verifier |
| `726fe50c43d6` | `776f7dd1c3e3` | DO/DO*: carry step-form PRESENCE, not its value (literal-NIL step must assign) |
| `4052a2e64de6` | `f6ba709bd425` | DWC2: port-reset must not clear PPWR — bare W1C write cut port power |
| `910a3005b93f` | `dd37617ca95e` | Document the build matrix + audit all 36 scripts by actually building them |
| `1f3304f1df04` | `67d83b6edee8` | Extract mvm/build-cli-common.lisp: ONE shared clean-image assembly |
| `8b61a22b4e01` | `c84592d442c9` | Five package-blind registries: deftype, symbol-macro, setf, compiler-macro, defstruct |
| `126dcf502cd2` | `eef6669e53b9` | Fix a duplicated defun head in the aarch64 argv slot; add a blob READ CHECK |
| `168d4cb374ca` | `854408fd060e` | Fix in-image DEFSTRUCT: nested backquote inside ,@(or ...) silently miscompiles |
| `ea3ed6362d91` | `158d1ea7034a` | Fix the name hash at 29 bits on every target; confirm the name on hash match |
| `37223d4f0c0b` | `e9282a7cca4c` | Fix two unescaped-quote read errors that made build-i386-cli and build-mvm unbuildable |
| `ce3be513d799` | `25136012cb71` | GATE #211: deletion is clean; the read-package switch is NOT the constants |
| `9a322eb196f2` | `da634a59862b` | GATE for the 29-bit name hash: x64 ANSI 0 regressions, aarch64 probes green |
| `d3bd262fe2d0` | `fb2bd301dcf3` | GATE-RESULT #237: :GENERA measurement — ladder 28->21, gate byte-identical |
| `7a795d15a3cd` | `508f87b471dd` | GATE-RESULT-210-rung1: confirm the deliverable rebuilds from the committed tree |
| `c0a0412c4553` | `8db46d108623` | GATE-RESULT-244-accessors: cost numbers, design rationale, residual |
| `c789369b93e1` | `cd67d2c8558a` | GATE-RESULT-244: ANSI gate, own noise floor, ladder with control |
| `1251ce8b691d` | `db17f03f3d92` | GATE-RESULT-i386-convergence: byte proof, divergence table, clustered inventory |
| `49844ffa805f` | `829ec681c845` | GATE-RESULT-i386-convergence: the first i386 ladder + alexandria numbers |
| `34327c7ccd4e` | `1d2b5eb0db75` | GATE-RESULT-i386-float: evidence for the i386 float codegen |
| `e18bd508774f` | `3ccfa4df429e` | GATE-RESULT-jit-defun.md: note how to undo the worktree sparse-checkout |
| `4604ab8b6799` | `c937cd751318` | GATE-RESULT-jit-defun.md: the measurement |
| `d300a72856ae` | `ce9faf18da52` | GATE-RESULT-jit: the JIT-on ANSI gate, the fallback census, and what blocks JIT-only |
| `15e8fc758d87` | `8cd86219160e` | GATE-RESULT-libfrontier.md: the library-frontier measurement |
| `06a8d94acb1f` | `2fe4ccd9f556` | GATE-RESULT-macro-gc.md: root cause, the five-instance class, and gate evidence |
| `4794c7ded9f8` | `14df653d1ead` | Gate fix: a global-variable WRITE must key by NAME, not by the symbol object |
| `2d84acdebff6` | `41b503c35b81` | Gate probes 9300-9309: lock the build-time initarg union in both directions |
| `776ae198504c` | `6444f9190f63` | Harden %GF-SYM-PKG-NAME: %PKG-NAME is an unguarded (AREF (CDR P) 0) |
| `1461e65ff5a5` | `e10c0e3027e7` | JIT co-init: V9..V15/V22 must be NIL, not the fixnum 0 make-array leaves |
| `bf0f62a93310` | `201fcf5ebcb9` | JIT differential coverage: a JIT-vs-interpret oracle and a fallback census |
| `3101736f78da` | `483ca2cbcb9c` | JIT: install runtime-defined functions as real native code |
| `7fa8749a8e64` | `f6deebfff978` | JIT: never bake a heap address into an exec page (runtime macros die on GC) |
| `e810e97706f3` | `fd52b1c27c94` | JIT: reproduce the native MV block instead of re-running the form |
| `83a4f98aef22` | `e181f9497cb9` | LABELS/FLET names were invisible inside a capturing nested LAMBDA |
| `1c8b23092de8` | `909cfde42ecc` | LOOP: a WHILE/UNTIL after a body clause must test AT its source position |
| `3607338b9315` | `c6799daa4e35` | LOOP: accept the CLHS bare simple-type-spec after a FOR/AS variable |
| `9a1b40cdfabe` | `e63787e3b03a` | Loader: bind *PACKAGE*/*READTABLE* per CLHS LOAD; honour :if-feature + :pathname |
| `31599b61c6d6` | `0723c483e5f4` | MV consumers: stale MV count leaked into single-value returns |
| `af91df7437db` | `4177a7dccf92` | MV-count leaked out of an AND/OR-tailed function (phantom extra values) |
| `293ce0a49e3e` | `f21cbb925d7c` | MVM compiler: ANF-normalize over-nested arithmetic instead of refusing it (#205) |
| `140ae66f635b` | `1e27fc3b5d4f` | Merge #201(a): width-neutral float layout — 4 slots x 16-bit tagged chunks |
| `ce8e522a88ad` | `d80a19904766` | Merge #209 rung 1: bare-metal Raspberry Pi on the CL/mvm stack |
| `f82544f35a42` | `9e336dab9a54` | Merge #209 rung 2: the bare-metal Pi CL image fetches a .tar.gz over real HTTP |
| `45b62f1c5bc8` | `34a58db343fd` | Merge #209 rung 3: bare-metal Pi FETCHES, INSTALLS, LOADS and CALLS a library |
| `60b2c3222ae9` | `76ce66057ccd` | Merge #210 rung 1: x64 image cross-emits a running aarch64 ELF |
| `8dd4318efe81` | `cd23fd9880f9` | Merge #215: unresolved calls target the %UNRESOLVED-FN stub, not offset 0 |
| `4f46d51ed16a` | `e9b260fa7d41` | Merge #216: :fn-addr emits the NIL sentinel for unresolved names |
| `3a6a7f8aa0b1` | `0a9052d4f87d` | Merge #219: unresolved calls to offset 0 were the SSH images' 'dhcp hang' |
| `303c6defb6e7` | `4e213c44e88a` | Merge #220: aarch64 :mod clobbered its own dividend via the ensure-src scratch |
| `603e6729b583` | `273b1d3f962c` | Merge #221: six aarch64 ensure-src scratch clobbers + full 102-site sweep |
| `2eb3b138acee` | `ff7e1303c296` | Merge #222: runtime-defined functions install as native code — the JIT keystone |
| `b1654c30f901` | `4cb93f045968` | Merge #227: never bake a heap address into an exec page — four classes of GC-stale corruption closed |
| `0652a45d377f` | `f17d652b83e3` | Merge #245: AArch64 CLI now calls init-all-globals (150 globals were unbound) |
| `39eb8b5f9f70` | `945015edd650` | Merge #251 (reproducible builds: MVM compiler gensym counter) + #253 (gate-runner ratchet fix) |
| `55bcb6aef9d6` | `328e4ad6f5dc` | Merge #254: rank>1 array literals compile instead of silently becoming NIL |
| `d30eaaa75fb6` | `e45984b68ed2` | Merge JIT differential: JIT-on gate clean, MV double-execution found, V9-V15 init fixed |
| `4a8a1f0947c2` | `928fb0114703` | Merge SSH cell migration: build scripts 33 -> 28 |
| `544ff0d5f0fc` | `9673ac5bf1a8` | Merge aarch64 GC arena guard invariant (build-time assert; no codegen change) |
| `1e84418433cc` | `cb0ca5709e9c` | Merge aarch64 mini-UART RX + chain loader address protocol |
| `293744744758` | `f1348829074b` | Merge bmap-fix: #160 bitmaps on bare-metal aarch64 + the BIGNUM-ASH-in-GC fix |
| `ebf45b636e9e` | `fc761c75b95c` | Merge i386 B3: GC arena guard band (12 of 13 SIGSEGVs → exit 0, ladder 2→8) |
| `b749370fed6d` | `34a0453e9ed6` | Merge i386 allocation zero-init as hardening (#259) — hypothesis DISPROVED |
| `e91594460726` | `36f520795c49` | Merge i386 float codegen: TRANSLATOR GAPS 13 -> 1 (#200/#201) |
| `f62645dcda3b` | `37854a5942c6` | Merge remote-tracking branch 'origin/main' |
| `11774cae7727` | `adf5bc427a13` | Merge run-script retirement: run-*.sh 24 -> 19, and fix run.sh's false-pass ssh probe |
| `f7514dc27be1` | `50f3057dcaf3` | Merge the two ~94%-identical ANSI gate harnesses into one (#208) |
| `a3e8767b2d0f` | `44dc4fc042c5` | Merge: #'FORWARD-FN inside one EVAL-WHEN module materialised as a raw offset — ladder 20 -> 16 |
| `f8ec62b6737b` | `f027d1234b65` | Merge: (defvar *x* nil) must BIND the variable, not leave it unbound |
| `b44701d86872` | `2b5c0b0a0540` | Merge: 13 real CL macros were reported as functions — init-compiler-macro-set never ran in three bui |
| `1e54537c784a` | `a733240901d5` | Merge: 27 standard CL FUNCTIONS were user-visible MACROS — visibility removed, inlining kept |
| `7fa12a07a6b0` | `3ee4e4cbf262` | Merge: ASDF's INTERFACE over Modus's own loader — ladder failures 28 -> 20 |
| `0c00bd9fb1ed` | `b1446c427a00` | Merge: CLOS class registry + condition CPL walks are package-aware — cross-package handler-case fixe |
| `101c5eb4d0b8` | `998fb18d151f` | Merge: CLOS generic-function registry is package-aware — fixes SILENT MIS-DISPATCH |
| `6de6fc3c4d78` | `f8dbae1edff9` | Merge: DEFSTRUCT accessors type-check — and the check is NEGATIVE cost |
| `52e8eb984a23` | `10bc0fae8c07` | Merge: DO/DO* honour a step form that is literally NIL — puri 10 -> 0, ladder 38 -> 28 |
| `65ef6cd6d7b9` | `7d0dc5284b49` | Merge: JIT reproduces the native MV block — no path double-executes side effects |
| `c2896ac48796` | `1b1b93d39315` | Merge: MAKE-INSTANCE's direct path now runs user initialization methods (CLHS 7.1.1) |
| `2273a255dd85` | `f69185786fc2` | Merge: MV count leak + define-modify-macro/MAX/MIN double-evaluation |
| `8f3321e76b26` | `cb2049618278` | Merge: Modus presents as :GENERA — ladder 28 -> 21, ANSI image byte-identical |
| `0c6bdd34fe94` | `7723273aa088` | Merge: O(1) package symbol-table index — recovers 25715/25791/25794, shard 56 139s->21s |
| `84cfbf9e797c` | `b02edb6a0702` | Merge: READER-ERROR triage tooling — 0 of 14 are Modus reader bugs |
| `dc793ab79e37` | `baa6e830f2d7` | Merge: RTEST package — run real library test suites (alexandria 218/250) |
| `88c26b989ca8` | `8c81ae98a6af` | Merge: SETF fallback interns SET-<NAME> in the accessor's package — alexandria 22/22 |
| `fc36c1a6282e` | `783d9bb8fb54` | Merge: aarch64 macro cluster — the x64 GC-safety gate was never ported |
| `3a7f5241e9fd` | `6a135016fd92` | Merge: alexandria 207 -> 222/230, ANSI 17510 -> 17520 (nine conformance fixes) |
| `f04dcbdba571` | `26fb81756e19` | Merge: alexandria 222 -> 227/230 — capture analysis ran before macroexpansion |
| `74ec206bea46` | `cd0ec59734ee` | Merge: alexandria 230/230 — a bare integer could be reinterpreted as a closure |
| `d18862a8f051` | `bb2eff6b75c7` | Merge: babel works — char-code-limit, nested-backquote capture, *features* endianness, LOOP bare typ |
| `0b704c34fef0` | `1a008244f9cb` | Merge: build-time DEFMETHOD records method meta; reinitialize-instance and change-class validate ini |
| `b68583c5792d` | `8bb316b3e9f4` | Merge: build-time check for defvars whose initform never runs — finds a LIVE bug in the shipping AAr |
| `f98bdef48c4b` | `8681f064cb37` | Merge: converge the aarch64 CLI onto x64's bring-up — and it FIXED four defects |
| `57733150c584` | `4b55d55fc99a` | Merge: converge the i386 CLI — all three platforms finally have a number |
| `ab9b8c19fcff` | `2b036e5758a3` | Merge: defstruct :predicate/:copier options + special-declared parameters — ANSI +6, zero lost |
| `bba7748a684f` | `b15ba295364a` | Merge: drop the interpreter's write-only HEAP retention list |
| `a311671c31d7` | `e5cd38419e66` | Merge: dynamic bindings are escape-safe — LET/LET*/MVB/DO/&aux no longer leak |
| `9a3e48e57508` | `e5167840e4f5` | Merge: make the 'survives GC' probes actually collect — validates #222, finds a live macro bug |
| `6fcc6eec1dfb` | `23a6cb7d3dee` | Merge: make-instance initarg validation includes init-method &key names (CLHS 7.1.2) |
| `e4d26a843d2f` | `5c98e1b15379` | Merge: per-package runtime macro expanders — fixes a same-name collision AND a hang class |
| `4db4ab6e3254` | `a91f0c2bcd0b` | Merge: real ASSERT macro + (setf (get ...)) puthash arg order — ladder 73 -> 38 |
| `1d24646d26c3` | `888adaa7f94d` | Merge: recovered hardware faults pierce every bytecode HANDLER-CASE — the #236 mechanism |
| `86b37e9c09f5` | `67fc2daf43b6` | Merge: runtime (defun (setf NAME) ...) registers as NAME's setf writer |
| `b9c8197e6caf` | `740b3ae2221d` | Merge: seven Quicklisp-ladder defects — ladder 16/22 -> 19/22 clean |
| `b1fdc6a0a748` | `da5ff0813b00` | Merge: six more package-blind registries — deftype, symbol-macro, setf, compiler-macro, defstruct, m |
| `14da18426733` | `8595a8ef9855` | Merge: special-variable store is package-aware — the registry family is CLOSED |
| `d01ab2588038` | `f52058c0ec73` | Merge: the aarch64 CLI can be MEASURED at all — first honest arm numbers |
| `c8bdb8298b7c` | `b6e950f63c46` | Merge: the loader is real — LOAD binds *package*/*readtable*, all 22 libs on install-tarball |
| `8ca0b6d84be3` | `1357a5ea4d75` | Merge: three of four unbuildable build scripts fixed — i386 is buildable again, plus CHECK E |
| `d63b677c6284` | `a46d0d0415b2` | Merge: two more build-check rules — and 17 live LET-of-special bugs, plus a reproducibility defect |
| `a53025135932` | `af7d4c68e532` | Method combinations are per-package: %FIND-MC's name probe was blind |
| `07711bd1dcd6` | `64de12de3106` | Migrate the 5 SSH/actors cells into build.lisp; retire their scripts (33 -> 28) |
| `67151d21218f` | `052ee96d70b0` | Modus presents as :GENERA — feature, compat surface, and the whole-ladder measurement |
| `dc76bd19fad0` | `35255d4e28c1` | Narrow the FNV intermediates to 16-bit state — bit-identical, no regeneration |
| `83d79661dea6` | `ec5c6615c00f` | Nested backquote: capture vars hidden under `,' in a closure's free-var scan |
| `c6181c50150c` | `6f29799d0e59` | Package symtab: O(1) hash index — recovers the 3 >U+00FF reader tests |
| `e513ab0fe018` | `9dc70f8a3392` | Per-package macro expanders: same-named macros in different packages no longer collide |
| `0752effb563c` | `07d6bebd50d7` | Populate the compiler-macro name set in the generic images (task #242) |
| `9a66bb1c3e8d` | `0f395cbefd14` | Prove run.sh per target; retire 5 REPL runners (run-*.sh 24 -> 19) |
| `7905ef0e509d` | `59ecfd8514c0` | Quicklisp ladder: five real defects behind cl-base64/cl-utilities/bt/cl-ppcre |
| `e944d9f68207` | `dda55876caac` | RANDOM with a FLOAT limit returned values outside [0,LIMIT) |
| `678ddc3ccfa4` | `b070b49078e2` | READ-SEQUENCE / WRITE-SEQUENCE parsed their &KEY args POSITIONALLY |
| `79e8c547b684` | `b1c2fcdbbe64` | REINITIALIZE-INSTANCE validated nothing; CHANGE-CLASS validated the wrong union |
| `5ec137b590cd` | `791f39961a57` | RPi CL: select the MINI UART for chain-loaded images, not PL011 |
| `2607fb40d5ec` | `ebd1e04fe083` | RPi bare metal: Modus CL REPL RUNS ON REAL Pi Zero 2 W hardware (#209 rung 4) |
| `3a6bcfd01b08` | `c014dc855164` | RPi bring-up: source-blob cull knob + UART loader FIFO-drain and bounds check |
| `fdd58fe6ed9c` | `7115afe9786c` | RTEST package: run real library test suites (alexandria's own 230 deftests) |
| `5a5265107acc` | `49332b08f68f` | Record the 64-shard ANSI gate for #205: NET == BASE, 0 real regressions |
| `14696f8720b8` | `d84bd45f1128` | Record the 64-shard ANSI gate for #206/#207: NET == BASE, 0 real regressions |
| `a4aefbcf7b86` | `9bdac210b142` | Regenerate real-ansi-gen.lisp: 230 corpus methods now carry key metadata |
| `efa8c6764e84` | `34c76fa00bd9` | Reproducible builds: the MVM compiler gets its own gensym counter |
| `0730ad4e3381` | `07eb595d6cf7` | Retire 3 proven build scripts: 36 -> 33 (actual consolidation, not a facade) |
| `723e9e98f6ab` | `3106518ac5be` | Revert real-ansi-gen.lisp: build-dump artifact swept in by git add -u |
| `8f1ca30253bf` | `a56750a49a43` | Revert the gate probes: registering an init method in the parent poisons every forked file |
| `ae50f3cffd09` | `26726696efbe` | Right-size the #208 byte-identity note: it is a measurement convenience |
| `5cd1eafcb5af` | `8e92e6e18b61` | Runtime (defun (setf NAME) …) registers as NAME's setf writer |
| `135f9473bd55` | `e492a8ed18a9` | Runtime CHECK-TYPE was a NO-OP; runtime WITH-STANDARD-IO-SYNTAX was a PROGN |
| `39b8eee2d4b1` | `f60c92732549` | SETF fallback: intern SET-<NAME> in the ACCESSOR's package, not *package* |
| `b4d5f9d93101` | `0d2da2fa66b0` | STRING of a native symbol/keyword returned the object, not its name |
| `bb72d984394f` | `10aaeba5f987` | Special variables are per-package: DA::*V* no longer reads DB::*V* |
| `cad6e4def895` | `3a19d0c371a1` | Special variables are per-package: DA::*V* no longer reads DB::*V* |
| `0733a6205153` | `15cf87968104` | Specials registry: pre-gate the fold, and close the three remaining doors |
| `163f4db8123c` | `09fc4276b4bf` | UART chain loader: carry the load address on the wire; fix kernel-main shadowing |
| `a60f3f2ba6f5` | `5310e70fb9fd` | UNGATED EXPERIMENT: package-discriminate %FIND-CLOS-CLASS's name-hash pass |
| `e3cf36bd5451` | `89a2eea1c5ac` | Untrack agent scratch artifacts committed by mistake |
| `9c9b39eadae9` | `(pruned)` | Untrack the 59MB modus-aa64 build artifact |
| `6b7d20cfadc0` | `ad5b57aeb455` | Untrack ~960 MB of accidentally-committed build binaries; ignore them |
| `f4e3d383cc74` | `4fce3295b6ef` | Validate READ-SEQUENCE / WRITE-SEQUENCE keyword arguments |
| `58795c8db0c2` | `b3762744667b` | Verify x64 float correctness before rebuilding the representation (#201) |
| `307e393ce877` | `91a9a74ec257` | WIP (DOES NOT WORK): wire #160 bitmaps for bare-metal aarch64 |
| `a50e334a75d9` | `f81a8c9b55ec` | WIP task#244: reproduce iterate #L fault from a 534-line prefix |
| `54379b2d36d8` | `cf04cf6dc2dd` | WITH-STANDARD-IO-SYNTAX was silently a PROGN in clean images |
| `9fefc2eb643c` | `8a0d2e372222` | WS5 #203 gap 2: wrap closures at the storage sinks so they survive across top-level forms |
| `2130a61fd01c` | `a60eb6ad511d` | WS5 #203 gap 3: a failing --load/--script now aborts and exits nonzero |
| `67e60a644f4c` | `96c9ed701493` | WS5 #203 gap 5: share the runtime backquote expander; restore the JIT guard's interpret-fallback con |
| `f20024892b6a` | `fbaf6aa791a7` | WS5 #203 gaps 1+2b: *ERROR-OUTPUT* is a real fd-2 stream; compiler WARNs leave stdout |
| `d139c7348bad` | `d88cf23f6471` | WS5 #203: JIT re-execution guard — lands, but does NOT fix the doubling; the re-flip gate's premise  |
| `f3d0aef25db6` | `8d1eb176347a` | WS5 #203: deep-wrap storage-sink arguments so a closure inside a structure survives across top-level |
| `cd7570d132a7` | `8c286017d93a` | WS5 #203: metric self-reports re-execution; localise the x64 doubling to a call landing at the modul |
| `f9ba3c3d51ca` | `5ac94b83cfae` | WS5 #203: revert #199's aarch64 JIT-on default — the JIT re-executes top-level forms |
| `2d95d3eaf2c2` | `804df87c807e` | WS5 #203: the x64 "call-by-name" bug IS the JIT re-execution defect — default OFF |
| `a573c538457b` | `ada65648a1f4` | WS5 #203: x64 build-chatter off a shipped binary's stdout; add the RUN-time metric |
| `245e67d34a2d` | `5b4d0d1b8b56` | WS5 #206/#207: flip the runtime JIT default back ON, x64 and aarch64 |
| `aec834175d7b` | `daa9c8a0c56f` | WS5 #206: JIT call-reloc must require a NATIVE callee — fixes the doubling |
| `3b9b4a4cc72c` | `8f4bc8c6f5db` | WS5 #206: same native-callee guard for aarch64 — real improvement, gate NOT met |
| `7071abbd5f6a` | `59087cdded3f` | WS5 #211 fix: the ANSI corpus DOES carry (in-package …) — contain it per file |
| `7a66219471cb` | `2f1173671d47` | WS5 #211 fix: the ANSI corpus DOES carry (in-package …) — contain it per file |
| `0cac558f835e` | `f0b0c63b5872` | WS5 #211 part 1: honor (in-package …) while reading — and it fixes DEFINE-REGISTERS |
| `c544a6997319` | `f008718b4209` | WS5 #211 part 1: honor (in-package …) while reading — and it fixes DEFINE-REGISTERS |
| `5b2ab4a1dcfe` | `841d0468cbcb` | WS5 #211 part 2: fold (package, name) into the function-table key |
| `e041372f0e7d` | `e01f54b34316` | WS5 #211 part 2: fold (package, name) into the function-table key |
| `c3cd8879bab2` | `30e78d61ba07` | WS5 #211 retry: narrow the read-package switch to an ALLOW-LIST (MODUS.ASM only) |
| `fc80a44535c9` | `12cb642df614` | WS5 #211 retry: narrow the read-package switch to an ALLOW-LIST (MODUS.ASM only) |
| `0f8583b2c374` | `9ec5af410469` | WS5 #211: contain build-x64-cl-repl's mvm-text — an 8th, unwrapped copy |
| `a69afd77e4dc` | `03a7ea1a8b18` | WS5 #211: make the build pipeline package-aware (per-file reset + honour in-package) |
| `db4a615a43a3` | `3eb8e6e2c518` | WS5 #211: make the build pipeline package-aware (per-file reset + honour in-package) |
| `d2c8da09083a` | `4ae3d8869e82` | WS5 #211: the containment was being deleted after it was inserted |
| `62025c43c408` | `b6df506a7199` | WS5 #213: the structures-03 stand-in must read in CL-TEST, not MODUS.MVM |
| `16a0cd8aedf7` | `ecdd3a92d78f` | WS5 #214: LOOP's IT anaphor is recognised by NAME, not symbol identity |
| `657c15442cb0` | `f36bfa95bf67` | WS5 #214b: the pprint logical-block CATCH tag must be package-proof |
| `297856f554ab` | `540e3fb820a7` | WS5 #214c: pprint-pop / pprint-exit outside a logical block must signal |
| `e932e3e32e76` | `875a91ef2998` | WS5 #223: GC-updated JIT constant vector — fallback reason 0, JIT-on gate NOT clean |
| `ee8a5b54dca0` | `c4c2cd9fb11a` | WS5 #223: add a constant-vector coverage guard; four more hypotheses ruled out |
| `150a4f9a7bc8` | `d5804474649a` | WS5 Phase 2a: compile-ash resolves NAMED constants to the inline shift path |
| `0c47b6c12f91` | `0b4236442356` | WS5 Phase 2b: the numeric width now FOLLOWS THE TARGET (i386 = 30-bit) |
| `be6839921243` | `3aa3dd6ef1ff` | WS5 Phase 3.1: unimplemented i386 traps NAME THEMSELVES — and that falsified my diagnosis |
| `fc5435b1a88c` | `f370dcc3781a` | WS5 Phase 3.2: VR-clobber round 2 — the nine object/memory ops, + a stated invariant |
| `02b16e44093f` | `9ef68aff36e1` | WS5 Phase 3.3: MECHANIZE the i386 register invariant — 15 violating opcodes found |
| `89ba65f96b4a` | `224179d2aad8` | WS5 Phase 3.4: fix all 15 invariant-violating opcodes — violation list is EMPTY |
| `04ed5bbe5eac` | `e3c4b89f7ba9` | WS5 Phase 3.5: re-measure mem-ref :u32 from scratch — the earlier attribution was half wrong |
| `f79b14ca5b39` | `ded574e983fc` | WS5 Phase 3.6: mem-ref :u32 bidirectional promotion (width-neutral) |
| `11a6ebe493d8` | `26d761f58c81` | WS5 Phase 3.7 + 4: compile-ash bignum safety — SHA256("abc") CORRECT on i386 |
| `59d75b40a7f0` | `663f0e5a7e9d` | WS5 RETRACTION: the bignum band is NOT broken, and ChaCha20 is CORRECT |
| `21347e473b2f` | `8f2c3842999d` | WS5 aarch64 JIT: relocate IN-MODULE fn-addrs — fixes funcall of every JIT-built closure |
| `be1aef101ebd` | `d04127386e4f` | WS5 aarch64: bake the Linux/AArch64 file-I/O syscall overrides into the CLI |
| `6fd8ad8a18b1` | `e59d7de3e76d` | WS5 aarch64: wire the shared SBCL-faithful cli-toplevel into build-aarch64-cli |
| `4cf2908d1315` | `19070c03ebef` | WS5 i386 GC: the collector never forwards a pointer — measured, and why |
| `6d8c4c234c9c` | `16b22fa938bc` | WS5 i386 LAYER 5: the compiler is in the image; EVAL now blocked on ONE trap |
| `692bd44988df` | `2f62b586034a` | WS5 i386 cleanup: one regression suite, one run script, one place for the knobs |
| `37a66c52c80b` | `8647e10b5477` | WS5 i386: (eq (eval t) t) was FALSE — the T immediate corrupted by op-LI |
| `e21077d0f8b0` | `a14e4745ae67` | WS5 i386: (eval 42) evaluates — CONSP said T for NIL, and MAKE-ARRAY was missing |
| `36eb269d5d26` | `14881c8f7933` | WS5 i386: BNULL could not see NIL — handler-case works under eval |
| `9cc5b80b9cd7` | `fbcc4dfd83cb` | WS5 i386: argv is NOT reachable — the kernel stack is above the 2^30 mem-ref ceiling |
| `e953df108c03` | `6f9028bb86b5` | WS5 i386: bake the build/runtime hash-agreement check; narrowing is BLOCKED |
| `bddf80c5ef89` | `68fbc54d40f7` | WS5 i386: derive the setjmp/longjmp design; the result register is a trap |
| `c590880e40c6` | `eea0824629fd` | WS5 i386: fix the c3 probe — it asserted a phantom gap; probe 10 is now 84/0 |
| `911dd8c29030` | `a897253f6680` | WS5 i386: implement TRAP #x0510/#x0511/#x0512 (setjmp/longjmp/clear-handler) |
| `cac871fe6238` | `aef4f9b106ec` | WS5 i386: implement trap #x0530 (COPY-OVERFLOW-ARGS) — boot init now completes |
| `bf043564bded` | `fa8db7d1cd14` | WS5 i386: init-all-globals completes — the init thunk stored under a BIGNUM key |
| `e00c851ef2c8` | `2ec81e7b3bea` | WS5 i386: isolate the HANDLER-CASE-under-eval gap (c1-c3), and clear a red herring |
| `cb9f7aa32300` | `f579b860a0af` | WS5 i386: locate the residual macro fault — the expander call passes one extra NIL |
| `8a94c7a6087b` | `c39f2f76706f` | WS5 i386: native Cheney collector — the third arch arm; 64 KiB SHA green |
| `fffb6d81431d` | `87dbca7a2378` | WS5 i386: port %init-sym-name-auto; the (eval 42) frontier is the 60-bit hash |
| `f0b8ef336370` | `b5064f8d5f2f` | WS5 i386: port the generated bootstrap — SFT + init-all-globals; frontier = setjmp |
| `f370f42fcc73` | `d4a6f066e48a` | WS5 i386: prove cli-toplevel's arch-specific half; the rest is blocked on layer 5 |
| `22bca5ebdaaf` | `571a19df618d` | WS5 i386: runtime QUOTE was NIL — x86-32 masks a shift count to 5 bits |
| `fa6b9c1b7ed0` | `cff8469f6099` | WS5 i386: runtime-metric output is now IDENTICAL to SBCL; cli-toplevel isolated |
| `de378e5b017e` | `241c0af09ce5` | WS5 i386: stage the FULL argv + envp into the BSS — argv[0] and env are reachable |
| `9d70d8049453` | `d8705ee07a35` | WS5 i386: the interpreter's canonical NIL was a BIGNUM — runtime macros work |
| `a60d917c2929` | `46ff09826bec` | WS5 i386: the macro/backquote bootstrap, real file I/O, and LOAD |
| `1973d7cb0a35` | `aa9062bd2e64` | WS5 i386: wire cli-toplevel over the staged argv — flags work, actions do not yet |
| `46f034e6342c` | `dd2b0b84909e` | WS5 width parameterization Phase 1 — ACCEPTED: zero compiled logic changed |
| `a03e971bc6ad` | `a8db942a5835` | WS5 width parameterization Phase 1 — NAMED CONSTANTS LANDED, BYTE-IDENTITY GATE FAILED |
| `327e606ed6cd` | `d40f5ee22c9a` | WS5: >4-arg convention already worked; ChaCha bug localized; i386 crypto perf measured |
| `3a8749b47644` | `d7b33827b942` | WS5: ChaCha localized to BIGNUM arithmetic above 32 bits (not arity, not frame slots) |
| `85889af1e2d3` | `d63272cd94d5` | WS5: ChaCha20 probe — the >4-argument gap fails SILENTLY, and loud traps miss it |
| `ba04682e4ab0` | `0a68b3d9a1b2` | WS5: GC VERDICT — i386 has NO collector; bulk SIGSEGV is arena exhaustion |
| `268eebe436f5` | `c20853122dd0` | WS5: GC port BLOCKED by a measured structural issue; arena fallback has a 2^31 ceiling |
| `5687501b0cf3` | `4d402d61b211` | WS5: gate the SELF-HOST path — Phase 2a is clean on it; add it to the standing gate |
| `f2c6cf95223f` | `975e62c27f87` | WS5: gate the i386 collector OFF — honest crash restored; bitmap port scoped precisely |
| `deee8e379b37` | `aa57e2fbd88e` | WS5: gdb round — EDX-clobber hypothesis DEAD; corruption is the collector; stopping |
| `6e75db37fa38` | `fa52981f366d` | WS5: i386 GC now RUNS (stack relocation worked) — but is unhardened and corrupts |
| `38c027689534` | `760526e708b6` | WS5: multiply promotion — the bignum half-limb width was hardcoded to 62-bit |
| `38e1ba66aa9f` | `7632f6ccf059` | WS5: object-start bitmap LANDED and live, but the collector still corrupts — gated off |
| `81704ddb04f8` | `f466a7dc2083` | WS5: probes 1+2 results — corruption is the BITMAP CODEGEN, not the collector |
| `81f78c4aa6b6` | `f556436809c7` | aarch64 CLI: *default-pathname-defaults* = "" (cwd), not "/tmp/" |
| `2fa42faf787f` | `407e1ab5e047` | aarch64 CLI: bake the library-compatibility surface (rtest, tar/install-tarball, runtime CL macros,  |
| `6d8111e0e304` | `5d00a20d3ffd` | aarch64 CLI: converge onto build-cli-common (drop the gate-runner harness) |
| `76c25d8155bb` | `8bc611ac0d2c` | aarch64 CLI: correct the features-fix comment to its measured scope |
| `726c9d7f141c` | `a029ec8d4e21` | aarch64 CLI: correct the genera-gate comment — it DOES cost parity |
| `180c17c372b3` | `18d57c4702d5` | aarch64 CLI: do NOT bake runtime-cl-macros.lisp — it breaks DOTIMES here |
| `bc24029e98f2` | `53ac3d93b886` | aarch64 CLI: make the :GENERA install opt-in (MODUS_GENERA=1) — it SIGSEGVs at boot |
| `165c32afbaf3` | `69b0cc0aeeb8` | aarch64 CLI: push :unix :linux :little-endian :64-bit at boot |
| `b87f2a7ab37f` | `fda1bf55ba16` | aarch64 JIT: port the GC-safety gate + carry cpatches (fixes stale macro consts) |
| `c74ace8cf0ad` | `07283080e204` | aarch64: assert the GC arena's overshoot guard instead of relying on luck |
| `120b307d9613` | `e93eb2d8ff4c` | aarch64: parameterize the serial RX poll so the mini UART can READ |
| `a75d512fd444` | `311a334750d6` | alexandria-1: empty the skip list -- all 230 tests terminate and pass |
| `9fbdd116c4a9` | `6bb663d2f11b` | bare-metal RPi: native aarch64 collector + halt-not-sys-exit — alexandria 16/22 -> 22/22 |
| `7fcee4ccc1cf` | `b2ac85620398` | boot-linux-i386: mark the "i386 HAS NO COLLECTOR" docstring STALE (#218) |
| `c7c554f91313` | `1b9d484e2966` | boot-rpi-cl: sync faults PRINT ESR/ELR/FAR/SP instead of silently spinning |
| `1567397ef19a` | `673da3acfc18` | boot-rpi: migrate the entry/ISR emitters to a64-buffer (build-rpi-ssh builds again) |
| `429a7375f524` | `53e0352c2c03` | build CLI wrappers: MODUS_DUMP_FULL_SOURCE gate for byte-identity proofs |
| `5d1fe014c011` | `8c0951064b57` | build-checks CHECK E: sweep every first-party .lisp for read errors (#252) |
| `06114bb74411` | `42f206516479` | build-checks: LET-of-unregistered-special (#248) + compiler-WARN histogram (#249) |
| `a10c7d53fb06` | `6baba7a2199b` | build-checks: baseline #248 FINDING 7 — cross.lisp's 8 translator hand-offs |
| `12f08b2f5d20` | `1db64dad155a` | build-checks: baseline build-mvm's 20 check-A findings (#252) |
| `e4c12202106a` | `e0b8d6d964e3` | build-checks: baseline the two aarch64 gate runners (#253) |
| `e9b3a56b7180` | `b4fb2275af14` | build-checks: byte-identity pin — the host gensym counter is an image input |
| `bac58873b6c9` | `2bbef00fc1a2` | build-checks: drop the three gensym-counter pins — the root cause is fixed |
| `59ade414ef17` | `c3f7c1aa8337` | build-checks: exempt ANSI gate runners from the corpus warn ratchet (#253) |
| `e6aac44fda3d` | `09355d29439c` | build-checks: fail the build when a global's initialisation never runs |
| `18bec992315f` | `2e243560360b` | build-checks: file the gate runners as FINDING 3; correct a false claim in the comments |
| `b8ff0bbaac1d` | `404bfd790a16` | build-checks: identify the image by its OUTER build script; skip the 17 MB gate blobs |
| `2cbae1c185c4` | `a1029713eb0d` | build-checks: record the two #248 findings confirmed on the shipping binary |
| `eb44ca0d0e9f` | `1019289c1de7` | build-checks: separate 'check found something' from 'check broke' |
| `1ed271b40cf9` | `b84adf667817` | build-checks: tighten the call graph, exclude name registries, add the ratchet |
| `ca431b502a8a` | `3c6f8f60e391` | build-checks: verify finding 1 on the actual aarch64 binary, not by reading code |
| `de21afa35089` | `5c48fc057b7a` | build-cli-common: add *CLI-BARE-METAL-NET-SOURCE* slot (#266 increment 1) |
| `ba693fa58d83` | `dbe620d4b103` | build-cli-common: admit a third arch (:i386) — new *CLI-ARCH-IO-SCRATCH-SOURCE* slot |
| `92e8a1bc9a94` | `b8ed3f52d9e0` | build-cli-common: bare-metal seam, so bare-metal images can be thin tails |
| `26fe8bcbd0fc` | `ddc255886812` | build-fixpoint: record the #252 diagnosis at the ceiling that stops it |
| `c9bef2f0b0c1` | `a83f055b570c` | build-rpi-cl-repl: MODUS_NET_NOAUTO=1 — compile the net stack in, don't start it |
| `ac75d270db0e` | `309f2f935273` | build-rpi-cl-repl: add the MODUS_DUMP_FULL_SOURCE gate (instrument for #266) |
| `83689a947d81` | `75dcb1784c17` | build-rpi-cl-repl: correct the stale bitmap-placement comments |
| `7f177f977a9e` | `f13e35d10db6` | build.lisp / BUILDS.md: drop MODUS_I386_LAYER, retired with the i386 convergence |
| `9815f466f25d` | `6abd431d443b` | build.lisp: transcribe output paths from the SCRIPTS — 7 of 28 were wrong |
| `5c5797b59e3f` | `6de3338a7b12` | compile-defvar: a NIL initform must BIND the variable, not leave it unbound |
| `9dbbf08809cb` | `56dd3a2904bd` | compiler: DEFINE-SETF-EXPANDER implements the real 5-value protocol |
| `4efca9ea55b1` | `2c831fae10a9` | compiler: cell-boxing analysis must macroexpand too |
| `87203c1ad98c` | `af9323d60493` | compiler: free-variable analysis must macroexpand before it walks |
| `98bb497681d0` | `931a1b9ec4ca` | conditions: bound %SIGNAL-* reentrancy — an error inside GETHASH ate the kernel |
| `1aaeb41ceb53` | `1eed0f2adc7a` | define-modify-macro and MAX/MIN each double-evaluated their arguments |
| `baf1f84fd185` | `26b6ca237f98` | defstruct: (:CONSTRUCTOR name) must SUPPRESS the default MAKE-<name> |
| `dfb5ad1ee5d9` | `6326fbe40835` | diag: allocation-free heap+stack GC verifiers — found the unforwarded roots |
| `5cdf94604045` | `f77f966bd058` | diag: globals-health probe + heap-pressure burn (proved it is collection #2) |
| `0c8f832c8d41` | `da04c851d004` | diag: instrumented tar-do-entries (MODUS_ALEX_DIAG) — proved the tar walk exact |
| `b6bb62867d47` | `958506614c9b` | diag: revert the bogus -1 shift — :u32 reads were right all along |
| `64dc79093075` | `221fd48fda6e` | diag: verify the instrument — my :u32 verifiers were reading DOUBLED values |
| `efa12bd2aa17` | `a5e162170c77` | docs: collector consolidation design — 4 implementations of one algorithm |
| `38e3a1b5fd30` | `d4cad15dae1f` | docs: i386 floats are a REPRESENTATION wall, not a codegen gap (#201) |
| `8ed38afced4f` | `e6d197eaf54c` | eval2: a forward #'FN inside a module escaped as a raw bytecode offset |
| `c2609e4d6c61` | `e6c43be38946` | fileio: READ-BYTE signals on a non-input stream instead of answering NIL |
| `b65730cb3544` | `6c1b027e8940` | gc.lisp: %gc-read64 returned word/2 — nothing was ever forwarded |
| `750ab3c02fb0` | `26f713844ab3` | gc.lisp: the #160 bitmap helpers called BIGNUM-ASH — allocation inside the GC |
| `faaf93a3a201` | `92b7a8b18edd` | i386 #260 under test: (%GET-NARGS) macro shims (30c0b6b + e01d32e), rebased on 2987a9c |
| `0256c2ac21bf` | `f3cf7da165b3` | i386 B3: rewrap a docstring line (comment only, no codegen change) |
| `18b223bf5691` | `aa6212a87519` | i386 B3: the GC arena needs x64's 16 MB overshoot guard band |
| `9c51c771e840` | `e31db00f5c98` | i386: JIT exec-page traps (#x0531-#x0534) + SAP-NEW / SAP-ADDR |
| `59547813995b` | `c0f6aacf7288` | i386: MV-count is a SHARED contract address, not a private globals slot (#261) |
| `ab3bcfb64ab4` | `db74e1d95103` | i386: MV-count is a SHARED contract address, not a private globals slot (#261) |
| `71c13d667ad4` | `7ebe373feb2d` | i386: SSE2 float codegen — FADD/FSUB/FMUL/FDIV/ITOF/FTOI/FCMP |
| `e01d32e87741` | `b8bd0fe7f7ad` | i386: capture BOTH convention slots before the first call in the macro shims |
| `2955dc4540c0` | `7f7e14638f7d` | i386: initialise every allocation word the collector will read (#259) |
| `30c0b6b4c99b` | `1278caa46808` | i386: read nargs through the (%GET-NARGS) primitive, not a literal address (#260) |
| `1b7090ad5703` | `b4c447ea4417` | i386: unblock the real CLI image — dead paths + a layer default that hid it |
| `f1294d85b58d` | `0ce9a3bf0804` | install-tarball: ASDF name designators downcase; read the .asd leniently |
| `0f04e4aaa952` | `89e92a6255d5` | interp: ALLOC-ARRAY / ALLOC-STRING double-untagged the count (make-array n -> n/2) |
| `d760670a17f2` | `f0d581b149d3` | interp: SHR/SAR are WORD-level, matching the native translators |
| `a45895a484f3` | `dd39fca27fed` | interp: drop the write-only HEAP retention list (unbounded leak) |
| `c26cdbafa25f` | `6b5f495fd227` | interp: stop wrapping bare data fixnums as lambda bytecode offsets |
| `9116a7919932` | `04d4112433f1` | jit-diff: make the "survives GC" probes actually collect — and they find a live bug |
| `2225432e808e` | `e4ecbd55368e` | jit-diff: measure mutual recursion and forward references instead of assuming |
| `6ab8d10349a6` | `77f246db0917` | macro-function: standard CL FUNCTIONS are no longer reported as MACROS |
| `28794063b9d9` | `362b5d917859` | make-instance: initarg validity includes init-method &key params (CLHS 7.1.2) |
| `de155c10f96b` | `7f89dff6ff59` | merge latch + fault reporter into the diag branch |
| `df48a9b1bd02` | `d2854e218fad` | probes: mgl-pax + named-readtables bootstrap load probe |
| `c898ad75e2a3` | `f3dba2b9cb21` | probes: registry hang-class repros (deftype / symbol-macro SIGSEGV) |
| `c639243710f6` | `631b804194bb` | rpi-cl: MODUS_ALEX_DIAG build flag for diagnosing bare-metal library loads |
| `5e013cfcb5e1` | `9626f6207886` | rpi-cl: set VBAR_EL2 as well as VBAR_EL1 — the image runs at EL2 |
| `9c3f047ea5ec` | `3e281a2300a5` | rtest: deftest expander must accept BOTH Modus macro calling conventions |
| `0af8eae55cc0` | `0cd3c08cfb15` | rtest: survive a non-terminating test (RT:RUN marker + honest skip list) |
| `f4907483af52` | `e51b77b1aaed` | rtest: un-SKIP GAUSSIAN-RANDOM.1 — the compiler bug behind it is fixed |
| `11a5f1423e86` | `36ace50f4828` | run-uefi-repl: send CR, not LF (the same silent-false-pass as run-repl-eval) |
| `271d8995ea0d` | `102eda05f80a` | scripts: aarch64 library-ladder runner (qemu-user shim over the lf drivers) |
| `b085a861a9ad` | `3e5e040abef8` | sequences: DELETE / DELETE-IF / DELETE-IF-NOT splice lists in place |
| `fe23468928b5` | `6749fcafe44b` | tags.lisp: record that #x60 is the float subtag, not mvm-bytecode (#201 step 1) |
| `762c3a35e748` | `1d613e81032e` | task #246 WIP: route all make-instance paths through %make-instance-list |
| `5d2c869e850e` | `3529b500101e` | task #246: differential batteries for the make-instance init protocol |
| `4d7c64ad89e3` | `316fd4b8c160` | task #246: set *clos-applying-defaults* with SETQ+unwind-protect, not LET |
| `9939bca2d886` | `2e8df20953dc` | tests/rtest: alexandria-1 + alexandria-2 suite drivers and the cluster tool |
| `f911fb40dd1b` | `1d9f71bd5543` | tools: reader-failure triage instrumentation (form position + read/eval trace) |
| `979f7683b219` | `c539e1683ad2` | wip(alexhang-2): gc.lisp exact-halves fix + bogus-header latch diag |
| `a2dce3ff2e62` | `d89a8e9ea73c` | wip: MODUS_ALEX_DIAG on top of the make-array fixes |
| `1efdd8f50a6a` | `33e18315d475` | worktree: ignore build artifacts and sweep outputs |
| `b26e5f4a2429` | `db224717b38c` | x64-asm: delete DEFINE-REGISTERS — 48 dead constants in a flat namespace (#211) |
| `ff531f95c644` | `b5405c997dc3` | x64-asm: delete DEFINE-REGISTERS — 48 dead constants in a flat namespace (#211) |
