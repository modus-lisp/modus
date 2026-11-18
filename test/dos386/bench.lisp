;;;; bench.lisp -- small portable workloads for comparing modus with SBCL.
;;;;
;;;;   sbcl --script test/dos386/bench.lisp
;;;;   qemu-i386-static modus-i386-cli --script test/dos386/bench.lisp
;;;;
;;;; Each line: NAME  MILLISECONDS-PER-REPETITION  REPS  RESULT.  The result is checked against a known
;;;; answer (BAD if it differs), so a fast wrong answer cannot pass.  Each
;;;; workload runs once untimed first (on modus a DEFUN may go native after
;;;; 64 calls or 10 ms; i386 has no JIT, so there it stays interpreted).
;;;; BENCH_SCALE=N multiplies every repetition count (default 1).

(load (merge-pathnames "bench-defs.lisp" (or *load-truename* "test/dos386/")))
(bench-main
 (let ((s (ignore-errors (sb-ext:posix-getenv "BENCH_SCALE"))))
   (if (and s (plusp (length s))) (parse-integer s) 1)))
(load (merge-pathnames "bench-clos.lisp" (or *load-truename* "test/dos386/")))
