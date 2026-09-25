;;;; r29-rest-many.lisp -- expect 42
;;;;
;;; &REST WITH MORE THAN FOUR ARGUMENTS.  The &rest prologue builds its list by
;;; loading argument k as `obj-ref VFP k' for every k, so arguments past the four
;;; register ones must be copied into the frame first -- at RUN time, because
;;; only the nargs slot knows how many there are.  That copy is TRAP #x0530
;;; (COPY-OVERFLOW-ARGS), which x64 and i386 implement and RISC-V had no arm for.
;;;
;;; r27 covers the FIXED-count copy on frame-enter; this is the other half, and
;;; the one &KEY parsing depends on: in the real CL image every &key call with
;;; more than four arguments read its keywords from uninitialised slots
;;; ("unknown keyword argument" with garbage values) where x64 reported nothing.
;;;
;;; THE CALL MUST GO THROUGH A VARIABLE.  For a KNOWN &rest callee the compiler
;;; builds the list in the CALLER and passes it as one argument, so a direct
;;; (sum-all 1 2 ...) never reaches the runtime copy -- the first version of this
;;; rung passed against a translator with no #x0530 arm at all, which is how
;;; that was found.  FUNCALL of a function held in a variable is an unknown
;;; callee: all eight arguments travel as arguments.
;;;
;;; Eight arguments, each a different value, summed: a list built from garbage,
;;; or truncated at four, or copied in the wrong order with a gap, cannot sum
;;; to 42 by accident often enough to matter -- and the four-argument
;;; truncation in particular gives 10.

(defun sum-all (&rest xs)
  (let ((s 0))
    (loop
      (when (null xs) (return s))
      (setq s (+ s (car xs)))
      (setq xs (cdr xs)))))

(defun probe ()
  (let ((f #'sum-all))
    (funcall f 1 2 3 4 5 6 7 14)))      ; 28 + 14 = 42
