;;;; hosted-handler-neighbour.lisp — DO A WORKER'S HANDLER FRAMES STAY IN ITS OWN WINDOW?
;;;;
;;;;   ./modus --script test/hosted-handler-neighbour.lisp
;;;;
;;;; Per-thread window blocks are 4 KB at a 0x1000 stride (net/hosted-sync.lisp
;;;; %THR-TLS-BLOCK: page + 0x2000 + cpu*0x1000), and every window access is
;;;; FS-relative with FS base = block - 0x10000000.  So an emitted address of
;;;; 0x10001000 is NOT "just past the window" — it is EXACTLY the next cpu's
;;;; window, and a handler stack placed there (main b490cfb, 512 frames on
;;;; x64-Linux) writes thread N's frames over thread N+1's MV buffer, handler
;;;; depth and dynamic bindings.  See docs/handler-stack-collision.md.
;;;;
;;;; Method: read two slots of cpu 2's window from MAIN, spawn ONE worker (cpu 1)
;;;; that nests 100 handler-cases and returns, join, read again from MAIN.  No
;;;; live victim, nothing instrumented inside the nesting, so the observation
;;;; cannot move the bug.  A frame-shaped word appearing at +0x640 (frame 50) is
;;;; the neighbour being written.  +0x1900 (frame 200) must stay 0 with only 100
;;;; frames pushed — that is the positive control that the read is real.
(defvar *fail* 0)
(defun ok (msg v want) (format t "  ~a ~a = ~a~%" (if (equal v want) "ok  " "FAIL") msg v)
  (unless (equal v want) (setq *fail* (+ *fail* 1))))
(defun blk-of (cpu) (+ (%thr-page) #x2000 (* cpu #x1000)))
(defun nest (n) (if (= n 0) :done (handler-case (nest (- n 1)) (error (c) (list :err n c)))))
(defun slots () (let ((b (blk-of 2))) (list (%gc-read64 (+ b #x640)) (%gc-read64 (+ b #x1900)))))
(defvar *before* (slots))
(let ((w (sb-thread:make-thread (lambda () (nest 100))))) (sb-thread:join-thread w))
(defvar *after* (slots))
(format t "cpu2 +0x640 (frame 50 slot):  before=~x after=~x~%" (first *before*) (first *after*))
(format t "cpu2 +0x1900 (frame 200 slot): before=~x after=~x~%" (second *before*) (second *after*))
(ok "cpu 2's window +0x640 untouched by cpu 1's 100 handler frames" (= (first *before*) (first *after*)) t)
(ok "cpu 2's window +0x1900 untouched (only 100 frames were pushed — control)" (= (second *before*) (second *after*)) t)
(format t "~a~%" (if (= *fail* 0) "HANDLER FRAMES STAY IN THEIR OWN WINDOW: PASS" "HANDLER FRAMES LAND IN THE NEIGHBOUR'S WINDOW: FAIL"))
(sys-exit (if (= *fail* 0) 0 1))
