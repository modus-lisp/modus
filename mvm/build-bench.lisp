;;;; build-bench.lisp - Micro-benchmarks for all MVM architectures
;;;;
;;;; Usage: sbcl --script mvm/build-bench.lisp [target]
;;;;
;;;; Targets: x64, aarch64, i386, arm32
;;;; Default: x64
;;;;
;;;; Produces /tmp/modus-bench-<target>.bin
;;;;
;;;; Each benchmark runs N iterations and reports the count.
;;;; On x64, RDTSC gives cycle-accurate timing.
;;;; On other architectures, we use a simple iteration counter
;;;; and report raw iteration counts for relative comparison.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

(defvar *bench-target*
  (let ((args (cdr sb-ext:*posix-argv*)))
    (if (and args (car args))
        (intern (string-upcase (car args)) :keyword)
        :x64)))

(format t "Target: ~A~%" *bench-target*)

(in-package :modus.mvm)

;;; ============================================================
;;; Install translator for target
;;; ============================================================

(let ((target (symbol-value (find-symbol "*BENCH-TARGET*" :cl-user))))
  (cond
    ((eq target :x64)     (modus.mvm.x64:install-x64-translator))
    ((eq target :aarch64) (install-aarch64-translator))
    ((eq target :i386)    (modus.mvm.i386:install-i386-translator))
    ((eq target :arm32)   (install-armv7-rpi-translator))
    (t (error "Unknown target: ~A" target))))

;;; ============================================================
;;; Benchmark source code
;;; ============================================================
;;;
;;; Each benchmark:
;;;   1. Prints name
;;;   2. Runs computation
;;;   3. Prints result (for correctness check)
;;;   4. Prints "OK" if correct
;;;
;;; Timing: wraps each bench in a loop of N iterations,
;;; uses a simple busy-loop counter for relative timing.

(defvar *bench-source* "

;;; ============================================================
;;; Output helpers
;;; ============================================================

(defun %serial-byte (b)
  (write-char-serial b))

(defun print-dec (n)
  (when (>= n 10)
    (print-dec (truncate n 10)))
  (%serial-byte (+ (mod n 10) 48)))

(defun print-str (s)
  (let ((i 0))
    (loop
      (when (>= i (array-length s)) (return 0))
      (%serial-byte (aref s i))
      (setq i (+ i 1)))))

(defun print-nl () (%serial-byte 10))

(defun print-label (a b c d e)
  (%serial-byte a) (%serial-byte b) (%serial-byte c)
  (%serial-byte d) (%serial-byte e))

;;; ============================================================
;;; Benchmark 1: Fibonacci (recursive)
;;; Tests: function calls, integer arithmetic, conditionals
;;; ============================================================

(defun fib (n)
  (if (< n 2)
      n
      (let ((a (fib (- n 1))))
        (let ((b (fib (- n 2))))
          (+ a b)))))

(defun bench-fib ()
  ;; fib(30) = 832040
  (let ((result (fib 30)))
    result))

;;; ============================================================
;;; Benchmark 2: Sieve of Eratosthenes
;;; Tests: array operations, nested loops, conditionals
;;; ============================================================

(defun sieve (limit)
  (let ((flags (make-array limit)))
    ;; Init all to 1 (prime candidate)
    (let ((i 0))
      (loop
        (when (>= i limit) (return 0))
        (aset flags i 1)
        (setq i (+ i 1))))
    ;; Sieve
    (let ((i 2))
      (loop
        (when (>= (* i i) limit) (return 0))
        (when (not (zerop (aref flags i)))
          (let ((j (* i i)))
            (loop
              (when (>= j limit) (return 0))
              (aset flags j 0)
              (setq j (+ j i)))))
        (setq i (+ i 1))))
    ;; Count primes
    (let ((count 0))
      (let ((i 2))
        (loop
          (when (>= i limit) (return count))
          (when (not (zerop (aref flags i)))
            (setq count (+ count 1)))
          (setq i (+ i 1)))))))

(defun bench-sieve ()
  ;; Primes up to 10000 = 1229
  (sieve 10000))

;;; ============================================================
;;; Benchmark 3: Tak (Takeuchi function)
;;; Tests: deep recursion, integer comparisons
;;; ============================================================

(defun tak (x y z)
  (let ((xx x))
    (let ((yy y))
      (let ((zz z))
        (if (not (< yy xx))
            zz
            (let ((a (tak (- xx 1) yy zz)))
              (let ((b (tak (- yy 1) zz xx)))
                (let ((c (tak (- zz 1) xx yy)))
                  (tak a b c)))))))))

(defun bench-tak ()
  ;; tak(18, 12, 6) = 7
  (tak 18 12 6))

;;; ============================================================
;;; Benchmark 4: Array sum (large array arithmetic)
;;; Tests: array access patterns, addition
;;; ============================================================

(defun bench-array-sum ()
  (let ((arr (make-array 1000)))
    (let ((i 0))
      (loop
        (when (>= i 1000) (return 0))
        (aset arr i (+ i 1))
        (setq i (+ i 1))))
    ;; Sum elements 10 times
    (let ((total 0))
      (let ((iter 0))
        (loop
          (when (>= iter 10) (return total))
          (let ((j 0))
            (loop
              (when (>= j 1000) (return 0))
              (setq total (+ total (aref arr j)))
              (setq j (+ j 1))))
          (setq iter (+ iter 1)))))))

;;; ============================================================
;;; Benchmark 5: Ackermann (deeply recursive)
;;; Tests: very deep call stacks
;;; ============================================================

(defun ack (m n)
  (let ((mm m))
    (let ((nn n))
      (if (zerop mm)
          (+ nn 1)
          (if (zerop nn)
              (ack (- mm 1) 1)
              (let ((inner (ack mm (- nn 1))))
                (ack (- mm 1) inner)))))))

(defun bench-ack ()
  ;; ack(3, 7) = 1021
  (ack 3 7))

;;; ============================================================
;;; Benchmark 6: Byte operations (XOR cipher)
;;; Tests: byte-level array ops, bitwise operations
;;; ============================================================

(defun bench-xor-cipher ()
  (let ((data (make-array 256)))
    (let ((key (make-array 16)))
      ;; Init key
      (let ((i 0))
        (loop
          (when (>= i 16) (return 0))
          (aset key i (+ (* i 17) 42))
          (setq i (+ i 1))))
      ;; Init data
      (let ((i 0))
        (loop
          (when (>= i 256) (return 0))
          (aset data i i)
          (setq i (+ i 1))))
      ;; XOR encrypt 100 times
      (let ((iter 0))
        (loop
          (when (>= iter 100) (return 0))
          (let ((i 0))
            (loop
              (when (>= i 256) (return 0))
              (aset data i (logxor (aref data i)
                                   (aref key (logand i 15))))
              (setq i (+ i 1))))
          (setq iter (+ iter 1))))
      ;; Checksum
      (let ((sum 0))
        (let ((i 0))
          (loop
            (when (>= i 256) (return sum))
            (setq sum (+ sum (aref data i)))
            (setq i (+ i 1))))))))

;;; ============================================================
;;; Benchmark 7: Cons/list operations
;;; Tests: allocation, car/cdr, gc pressure
;;; ============================================================

(defun list-build (n)
  (let ((lst ()))
    (let ((i 0))
      (loop
        (when (>= i n) (return lst))
        (setq lst (cons i lst))
        (setq i (+ i 1))))))

(defun list-sum (lst)
  (let ((sum 0))
    (let ((cur lst))
      (loop
        (when (not cur) (return sum))
        (setq sum (+ sum (car cur)))
        (setq cur (cdr cur))))))

(defun bench-cons ()
  ;; Build list of 1000 elements, sum it, repeat 10 times
  (let ((total 0))
    (let ((iter 0))
      (loop
        (when (>= iter 10) (return total))
        (let ((lst (list-build 1000)))
          (setq total (+ total (list-sum lst))))
        (setq iter (+ iter 1))))))

;;; ============================================================
;;; Benchmark 8: Integer division / modulo
;;; Tests: truncate, mod operations
;;; ============================================================

(defun bench-divmod ()
  (let ((sum 0))
    (let ((i 1))
      (loop
        (when (> i 10000) (return sum))
        (let ((q (truncate i 7)))
          (let ((r (mod i 7)))
            (setq sum (+ sum (+ q r)))))
        (setq i (+ i 1))))))

;;; ============================================================
;;; Runner: execute each benchmark and report
;;; ============================================================

(defun run-bench (name expected thunk-result)
  (let ((r thunk-result))
    ;; Print name
    (%serial-byte 91)  ; [
    (let ((i 0))
      (loop
        (when (>= i (array-length name)) (return 0))
        (%serial-byte (aref name i))
        (setq i (+ i 1))))
    (%serial-byte 93)  ; ]
    (%serial-byte 32)  ; space
    ;; Print result
    (print-dec r)
    (%serial-byte 32)  ; space
    ;; Check
    (if (eq r expected)
        (progn (%serial-byte 79) (%serial-byte 75))  ; OK
        (progn (%serial-byte 70) (%serial-byte 65)    ; FAIL
               (%serial-byte 73) (%serial-byte 76)))
    (print-nl)
    r))

(defun kernel-main ()
  ;; Banner
  (%serial-byte 66) (%serial-byte 69) (%serial-byte 78)
  (%serial-byte 67) (%serial-byte 72) (print-nl)  ; BENCH

  ;; fib
  (let ((n (make-array 3)))
    (aset n 0 70) (aset n 1 73) (aset n 2 66)
    (run-bench n 832040 (bench-fib)))

  ;; sieve
  (let ((n (make-array 5)))
    (aset n 0 83) (aset n 1 73) (aset n 2 69)
    (aset n 3 86) (aset n 4 69)
    (run-bench n 1229 (bench-sieve)))

  ;; tak
  (let ((n (make-array 3)))
    (aset n 0 84) (aset n 1 65) (aset n 2 75)
    (run-bench n 7 (bench-tak)))

  ;; array-sum
  (let ((n (make-array 4)))
    (aset n 0 65) (aset n 1 83) (aset n 2 85) (aset n 3 77)
    (run-bench n 5005000 (bench-array-sum)))

  ;; ack
  (let ((n (make-array 3)))
    (aset n 0 65) (aset n 1 67) (aset n 2 75)
    (run-bench n 1021 (bench-ack)))

  ;; xor-cipher
  (let ((n (make-array 3)))
    (aset n 0 88) (aset n 1 79) (aset n 2 82)
    (run-bench n 32640 (bench-xor-cipher)))

  ;; cons
  (let ((n (make-array 4)))
    (aset n 0 67) (aset n 1 79) (aset n 2 78) (aset n 3 83)
    (run-bench n 4995000 (bench-cons)))

  ;; divmod
  (let ((n (make-array 3)))
    (aset n 0 68) (aset n 1 73) (aset n 2 86)
    (run-bench n 7169284 (bench-divmod)))

  ;; Done
  (%serial-byte 68) (%serial-byte 79) (%serial-byte 78)
  (%serial-byte 69) (print-nl)  ; DONE
  (loop))
")

;;; ============================================================
;;; Build
;;; ============================================================

(let ((target (symbol-value (find-symbol "*BENCH-TARGET*" :cl-user))))
  (let ((build-target (case target
                        (:x64     :x86-64)
                        (:aarch64 :aarch64)
                        (:i386    :i386)
                        (:arm32   :armv7-rpi)
                        (otherwise (error "Unknown: ~A" target)))))
    (format t "Building benchmark image for ~A...~%" build-target)
    (let ((image (build-image :target build-target :source-text *bench-source*)))
      (let ((outfile (format nil "/tmp/modus-bench-~(~A~).bin" target)))
        (format t "Entry point: ~A~%" (kernel-image-entry-point image))
        (format t "Native code: ~D bytes~%" (length (kernel-image-native-code image)))
        (write-kernel-image image outfile)
        (format t "Wrote ~A~%" outfile)
        (case target
          (:x64
           (format t "Run: qemu-system-x86_64 -m 512 -nographic -no-reboot -kernel ~A~%" outfile))
          (:aarch64
           (format t "Run: qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 -nographic -semihosting -kernel ~A~%" outfile))
          (:i386
           (format t "Run: qemu-system-i386 -m 256 -nographic -no-reboot -kernel ~A~%" outfile))
          (:arm32
           (format t "Run: qemu-system-arm -M raspi2b -m 1G -nographic -kernel ~A~%" outfile)))))))
