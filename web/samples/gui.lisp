;;;; gui.lisp - a tiny GUI bridge for Modus running in the browser.
;;;;
;;;; Lisp (in the worker) drives the page's DOM and a WebGL canvas by sending
;;;; tab-separated command lines through syscall 5000; input events come back
;;;; through a ring the page fills, drained with 5001 and waited on with 5002.
;;;; Browser only - the syscalls are no-ops under the node CLI.
;;;;
;;;;   (load "gui.lisp")   ; then (load "calc.lisp") or (load "snake.lisp")

;; Fixed scratch buffers in the BSS block.  These overlap the introspection
;; RPC buffers, which is safe: that RPC only runs while the worker is idle in
;; a stdin read, whereas a GUI program is busy in its own event loop.
;; (Written as literals, not specials, so loaded code needs no proclamation.)

(defun %gui-write (s)
  (let ((n (length s)) (i 0))
    (loop (when (>= i n) (return n))
      (setf (mem-ref (+ #x10018000 i) :u8) (char-code (char s i)))
      (setq i (+ i 1)))))

(defun gui-cmd (s) (syscall3 5000 #x10018000 (%gui-write s) 0) nil)
(defun gui-wait (ms) (syscall3 5002 ms 0 0) nil)

(defun %num (n) (write-to-string n))
(defun %tab () (code-char 9))

(defun %tj (fields)               ; join a list of strings with tabs
  (if (null fields) ""
      (let ((r (car fields)) (rest (cdr fields)) (tab (string (%tab))))
        (loop (when (null rest) (return r))
          (setq r (concatenate 'string r tab (car rest)))
          (setq rest (cdr rest))))))

(defun gui-line (&rest fields) (gui-cmd (%tj fields)))

(defun gui-reset ()          (gui-cmd "reset"))
(defun gui-panel (title)     (gui-line "panel" title))
(defun gui-el (id parent tag)(gui-line "el" id parent tag))
(defun gui-text (id s)       (gui-line "text" id s))
(defun gui-class (id s)      (gui-line "class" id s))
(defun gui-style (id s)      (gui-line "style" id s))
(defun gui-on (id ev)        (gui-line "on" id ev))
(defun gui-canvas (w h)      (gui-line "canvas" (%num w) (%num h)))
(defun gui-bg (r g b)        (gui-line "bg" (%num r) (%num g) (%num b)))
(defun gui-sprites (s)       (gui-line "sprites" s))
(defun gui-animate (on)      (gui-line "animate" (if on "1" "0")))
(defun gui-keys (on)         (gui-line "keys" (if on "1" "0")))
(defun gui-close ()          (gui-cmd "close"))

;; split STR on the character code CH; returns a list of substrings.
(defun %gui-split (str ch)
  (let ((n (length str)) (start 0) (i 0) (acc nil))
    (loop
      (when (>= i n) (push (subseq str start i) acc) (return (nreverse acc)))
      (when (= (char-code (char str i)) ch)
        (push (subseq str start i) acc) (setq start (+ i 1)))
      (setq i (+ i 1)))))

(defun gui-drain ()               ; raw event text, or NIL
  (let ((n (syscall3 5001 #x1001C000 8192 0)))
    (if (<= n 0) nil
        (let ((s (%make-string-array n)) (i 0))
          (loop (when (>= i n) (return s))
            (aset s i (mem-ref (+ #x1001C000 i) :u8)) (setq i (+ i 1)))))))

;; A list of events; each is a list of tab-separated fields, e.g.
;;   ("click" "b7")  ("key" "ArrowLeft" "1")  ("tick" "16")
(defun gui-events ()
  (let ((raw (gui-drain)) (out nil))
    (when raw
      (dolist (line (%gui-split raw 10))
        (when (> (length line) 0) (push (%gui-split line 9) out))))
    (nreverse out)))

;; small helpers used by the samples
(defun ev-is (ev kind) (and ev (string= (car ev) kind)))
(defun key-down-p (ev name)
  (and (ev-is ev "key") (string= (cadr ev) name) (string= (caddr ev) "1")))

(format t "gui.lisp loaded: (calc) and (snake) after loading their files.~%")
