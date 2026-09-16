;;;; web-rpc.lisp - the browser introspection RPC (completion / apropos /
;;;; describe), stripped OUT of the baked image and loaded verified at boot.
;;;; Pure CL-USER: memory ops + a maphash over *sym-name-table*, no I/O.
;;;; Page writes a request to a fixed buffer, wakes the worker, and the host
;;;; calls %web-rpc from inside the idle stdin read (see host-browser.js).

;; ---- introspection RPC for the browser UI (completion + symbol explorer) ----
;; The page can't talk to the image while it's blocked reading stdin, so it
;; writes a request into a fixed BSS buffer, wakes the worker, and the worker
;; calls %web-rpc from inside the idle read; %web-rpc writes a text response
;; into another fixed buffer.  Request: [op:u8][NUL-terminated arg].  Response:
;; [len:u64][bytes].  op 'c'=complete(prefix) 'a'=apropos(substring)
;; 'd'=describe(name).  All memory ops; no I/O; runs re-entrantly.
;; NB: (aref STRING i) yields a CHARACTER; %prim-aref yields the raw char CODE,
;; which is what we want to write as bytes and to compare.
(defun %web-req () 268525568)   ; #x10016000
(defun %web-res () 268529664)   ; #x10017000  ([len:u64] then bytes at +8)
(defun %web-res-cap () 30000)
(defvar *web-cur* nil)
(defun %web-put-byte (b)
  (when (< *web-cur* (%web-res-cap))
    (setf (mem-ref (+ (%web-res) 8 *web-cur*) :u8) b)
    (setq *web-cur* (+ *web-cur* 1))))
(defun %web-put-str (s)
  (let ((n (length s)) (i 0))
    (loop (when (>= i n) (return nil)) (%web-put-byte (%prim-aref s i)) (setq i (+ i 1)))))
(defun %web-cstr (addr)
  (let ((len 0))
    (loop (when (= (mem-ref (+ addr len) :u8) 0) (return nil)) (setq len (+ len 1)))
    (let ((s (%make-string-array len)) (i 0))
      (loop (when (>= i len) (return s)) (aset s i (mem-ref (+ addr i) :u8)) (setq i (+ i 1))))))
(defun %web-prefix-p (v up n)
  (and (>= (length v) n)
       (let ((ok t) (i 0))
         (loop (when (or (not ok) (>= i n)) (return ok))
           (unless (= (%prim-aref v i) (%prim-aref up i)) (setq ok nil))
           (setq i (+ i 1))))))
(defun %web-substr-p (v up)
  (let ((hl (length v)) (nl (length up)))
    (if (> nl hl) nil
        (let ((i 0) (found nil))
          (loop (when (or found (> i (- hl nl))) (return found))
            (let ((j 0) (m t))
              (loop (when (or (not m) (>= j nl)) (return nil))
                (unless (= (%prim-aref v (+ i j)) (%prim-aref up j)) (setq m nil))
                (setq j (+ j 1)))
              (when m (setq found t)))
            (setq i (+ i 1)))
          found))))
(defun %web-trunc (s max)
  (if (> (length s) max) (subseq s 0 max) s))
(defun %web-match (needle substr-p)
  (let ((up (string-upcase needle)) (n (length needle)) (cap 400) (cnt 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (and (stringp v) (< cnt cap) (< *web-cur* (%web-res-cap))
                          (if substr-p (%web-substr-p v up) (%web-prefix-p v up n)))
                 (%web-put-str v) (%web-put-byte 10) (setq cnt (+ cnt 1))))
             *sym-name-table*)))
(defun %web-value-str (v)
  ;; Only print value types that are safe/bounded to print; opaque objects
  ;; (packages, hash-tables, streams, instances) print pathologically here, so
  ;; show their type instead.  Lists/atoms are bounded with print-level/length.
  (if (or (integerp v) (bignump v) (ratiop v) (%ieee-float-p v)
          (characterp v) (stringp v) (symbolp v) (null v) (eq v t) (consp v))
      (%web-trunc (handler-case (let ((*print-length* 12) (*print-level* 3))
                                  (write-to-string v))
                    (t (c) "?")) 300)
      (handler-case (concatenate 'string "#<" (write-to-string (type-of v)) ">")
        (t (c) "#<object>"))))
(defun %web-describe (name)
  (let* ((up (string-upcase name)) (sym (intern up))
         (kind (cond ((special-operator-p sym) "special-operator")
                     ((macro-function sym) "macro")
                     ((fboundp sym) "function")
                     ((boundp sym) "variable")
                     (t "unknown"))))
    (%web-put-str "name") (%web-put-byte 9) (%web-put-str up) (%web-put-byte 10)
    (%web-put-str "kind") (%web-put-byte 9) (%web-put-str kind) (%web-put-byte 10)
    (when (fboundp sym) (%web-put-str "callable") (%web-put-byte 9) (%web-put-str "t") (%web-put-byte 10))
    (when (boundp sym)
      (%web-put-str "value") (%web-put-byte 9)
      (%web-put-str (%web-value-str (symbol-value sym)))
      (%web-put-byte 10))))
(defun %web-rpc-impl ()
  (setq *web-cur* 0)
  (let ((op (mem-ref (%web-req) :u8)) (arg (%web-cstr (+ (%web-req) 1))))
    (cond
      ((= op 99)  (%web-match arg nil))   ; c
      ((= op 97)  (%web-match arg t))      ; a
      ((= op 100) (%web-describe arg))     ; d
      (t (%web-put-str "?"))))
  (setf (mem-ref (%web-res) :u64) *web-cur*)
  nil)

;; register this implementation with the baked stub (see build-web.lisp)
(setq *web-rpc-fn* (function %web-rpc-impl))
