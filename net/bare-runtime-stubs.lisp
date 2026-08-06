;;;; bare-runtime-stubs.lisp — CL-runtime stubs for net/-only images.
;;;;
;;;; A bare-metal net image (x64-ssh, aarch64-ssh/actors/isolated,
;;;; i386-ssh, arm32-ssh, rpi-*) is built from net/*.lisp + repl-source +
;;;; an inline kernel-main.  It does NOT link mvm/cl-*.lisp, so the CL
;;;; runtime helpers the COMPILER emits calls to (generic arithmetic
;;;; fallbacks, float/ratio predicates, the condition signallers) have no
;;;; definition in the image.
;;;;
;;;; An unresolved call is NOT a no-op: emit-bytecode-for-ir's :call arm
;;;; targets bytecode offset 0 when the callee is unknown — that is the
;;;; module's FIRST function, entered with the caller's arguments.  Since
;;;; that function makes unresolved calls of its own, control never comes
;;;; back: the stack walks off its base and the machine takes a #GP into
;;;; the boot halt loop.  That was task #219 (the SSH images wedging in
;;;; dhcp-discover).
;;;;
;;;; Two defences, both here:
;;;;   1. %UNRESOLVED-FN — the compiler looks this name up (see the :call
;;;;      arm) and points every unresolved call at it, so a gap returns
;;;;      NIL instead of executing garbage.  The build log still lists
;;;;      every unresolved callee, so nothing is hidden.
;;;;   2. Real definitions for the helpers that are on a HOT path, where
;;;;      NIL would be a wrong answer rather than an unreached one.
;;;;
;;;; This file is loaded FIRST in each image's net list so anything later
;;;; can override it (last-defun-wins).

;;; The unresolved-call landing pad.  Reached only through the compiler's
;;; :call fallback, never by name.
(defun %unresolved-fn () nil)

;;; Emitted by compile-truncate / compile-mod / compile-floor and friends
;;; to decide between the inline fixnum division and the generic path.
;;; A net image has no IEEE floats, so this is constantly false — but it
;;; is on the HOT path: (truncate len 2) in ip-checksum runs for every
;;; outgoing IP packet, which is where DHCP DISCOVER used to die.
(defun %ieee-float-p (x) nil)
(defun %complex-p (x) nil)

;;; EXACT-DIVIDE — compile-/ dispatches (/ a b) with two integers here.
;;; In the CL runtime it may build a RATIO; a net image has no ratios and
;;; every (/ …) in net/ is an exact or deliberately-truncating integer
;;; division (print-dec's (/ n 10), dwc2's packet-count arithmetic).
;;; Unresolved, print-dec's (/ n 10) returned NIL and print-dec then
;;; recursed on NIL forever — the DHCP:IP= wedge.
(defun exact-divide (a b) (truncate a b))
(defun %rational-divide (a b) (truncate a b))

;;; BIGNUM-ASH — compile-ash inlines :shl/:sar only for a CONSTANT count
;;; ≤ 30; a VARIABLE count becomes a runtime BIGNUM-ASH call.  net/ has
;;; exactly two such sites, both `(ash 1 bit-idx)` in crypto.lisp's
;;; Ed25519 scalar multiply — i.e. on the SSH key-exchange path.  Nothing
;;; here ever exceeds a fixnum, so shift by repeated doubling/halving.
(defun bignum-ash (n k)
  (if (< k 0)
      (let ((v n)
            (c (- 0 k)))
        (loop
          (if (zerop c)
              (return v)
              (progn (setq v (truncate v 2))
                     (setq c (- c 1))))))
      (let ((v n)
            (c k))
        (loop
          (if (zerop c)
              (return v)
              (progn (setq v (* v 2))
                     (setq c (- c 1))))))))

;;; NTH.  Also hot, and for a non-obvious reason: compile-mod expands
;;; (mod n d) to (nth-value 1 (truncate n d)), and NTH-VALUE is a macro
;;; over (nth N (multiple-value-list …)).  With NTH unresolved every MOD
;;; returned NIL — which is how both E1000 ring cursors ended up holding
;;; NIL>>1 (state+0x10 = state+0x14 = 0x6F568000), so the RX descriptor
;;; address computed off a garbage cursor and #PF'd.
(defun nth (n lst)
  (let ((k n)
        (l lst))
    (loop
      (if (null l)
          (return ())
          (if (zerop k)
              (return (car l))
              (progn (setq k (- k 1))
                     (setq l (cdr l))))))))
