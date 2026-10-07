;;;; lib/save-image-x64.lisp -- the hosted x86-64 slots of SAVE-AND-DIE.
;;;; Baked after lib/save-image.lisp in the x64 hosted CLI only (last-defun-wins).
;;;; boot/boot-linux-x64.lisp maps the heap at +linux-x64-fixed-heap-base+ and
;;;; the JIT arena at +linux-x64-jit-arena-base+ (the same defaults as the
;;;; AArch64 CLI, so save-image's :jit-arena-base default already matches); the
;;;; one thing that differs is WHERE the arena's bump word lives: 0x10000FB8,
;;;; because the aarch64 word (0x10000F58) is inside x64's per-region cell table
;;;; (0x10000F08..0x10000F88).  translate-x64's #x0531 arm reads the same word.
(defun %core-jit-bump-slot () #x10000FB8)
(defun %core-heap-alloc-start () #x400)   ; +linux-x64-heap-alloc-start+
;;; x64 has NO config word for the cons-kind bitmap: translate-x64 derives its
;;; base as [0x10000E18] + *mcgc-kindbitmap-delta* (boot-linux-x64.lisp).  The
;;; shared reader of 0x10000E40 answers 0 here, and a write(2) from address
;;; boff then fails -- the core came out exactly one bitmap slice short.  The
;;; delta is a HOST value; build-cli-common bakes it as %CORE-KINDBITMAP-DELTA.
(defun %core-cons-bitmap-base () (+ (%core-bitmap-base) (%core-kindbitmap-delta)))
