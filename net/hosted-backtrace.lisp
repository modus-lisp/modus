;;;; hosted-backtrace.lisp — a backtrace on hosted x86-64.
;;;;
;;;; Every compiled function opens with  push rbp / mov rbp,rsp,  so the frames form a chain:
;;;; [FP] is the caller's frame pointer and [FP+8] the return address into the caller.
;;;; %FRAME-POINTER (a compiler intrinsic) gives this function's FP; walking up the chain
;;;; yields the return addresses, and each is named against the code it falls in:
;;;;
;;;;   - code built into the image: *SYMBOL-FUNCTION-TABLE* maps names to functions, a
;;;;     function value is its entry address + 3 (the tag), and the nearest entry at or below
;;;;     a return address is the function that contains it -- NAME+OFFSET;
;;;;   - code the JIT made at run time (the arena at #x3000000000) is labelled JIT@ADDR;
;;;;   - anything else ?@ADDR.
;;;;
;;;; The walk stops at the first link that does not go UP the stack, goes implausibly far, or
;;;; is not 8-aligned: a corrupt chain ends the backtrace instead of faulting it.
;;;;
;;;; A Lisp-signalled error reaches HANDLER-BIND handlers before anything unwinds, so calling
;;;; this from one shows the stack at the error -- SBCL's own idiom, and SB-DEBUG:PRINT-
;;;; BACKTRACE is defined on it (net/sb-sys-shim.lisp).  A FAULT (a trap from compiled code,
;;;; the shared-store guard among them) is delivered by longjmp, so its frames are gone by the
;;;; time a handler runs.  So the signal stub walks them first (translate-x64, THE FAULT'S
;;;; BACKTRACE) into this thread's window, and %FAULT-BACKTRACE-LIST names them: the stack of
;;;; the LAST hardware fault on this thread, valid until the next one.

(defun %bt-word (addr)
  "The 64-bit word at ADDR, exactly (two 32-bit loads: a :U64 load is not exact)."
  (+ (mem-ref addr :u32) (* 4294967296 (mem-ref (+ addr 4) :u32))))

(defun %backtrace-addresses (&optional (max 60))
  "Return addresses up the frame chain from the caller of this function, innermost first."
  (let* ((fp (%frame-pointer)) (start fp) (out nil) (n 0))
    (loop
      (when (or (>= n max) (< fp 4096) (/= 0 (logand fp 7))) (return))
      (let ((next (%bt-word fp)) (ret (%bt-word (+ fp 8))))
        (when (< ret 4096) (return))
        (setq out (cons ret out) n (+ n 1))
        (when (or (<= next fp) (> (- next start) 268435456)) (return))
        (setq fp next)))
    (cdr (reverse out))))                    ; drop our own return into the caller

(defun %bt-u (v off n)
  "Little-endian unsigned N-byte integer at OFF in the octet vector V."
  (let ((x 0)) (dotimes (i n x) (setq x (+ x (ash (aref v (+ off i)) (* 8 i)))))))

(defun %bt-read (s off n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (file-position s off)
    (read-sequence v s)
    v))

(defun %bt-elf-table ()
  "(entry-address . name) for every function symbol in this executable's own ELF symbol
   table -- what nm prints -- sorted by address; NIL if it cannot be read.  Every function
   the image was built with is there, not just the ones *SYMBOL-FUNCTION-TABLE* publishes."
  (handler-case
      (with-open-file (s "/proc/self/exe" :element-type '(unsigned-byte 8))
        (let* ((eh (%bt-read s 0 64))
               (shoff (%bt-u eh #x28 8)) (shentsize (%bt-u eh #x3A 2)) (shnum (%bt-u eh #x3C 2))
               (sh (%bt-read s shoff (* shnum shentsize)))
               (pairs nil))
          (dotimes (i shnum)
            (let ((b (* i shentsize)))
              (when (= 2 (%bt-u sh (+ b 4) 4))                      ; SHT_SYMTAB
                (let* ((symoff (%bt-u sh (+ b 24) 8)) (symsize (%bt-u sh (+ b 32) 8))
                       (link (%bt-u sh (+ b 40) 4)) (lb (* link shentsize))
                       (stroff (%bt-u sh (+ lb 24) 8)) (strsize (%bt-u sh (+ lb 32) 8))
                       (syms (%bt-read s symoff symsize))
                       (strs (%bt-read s stroff strsize)))
                  (dotimes (k (floor symsize 24))
                    (let* ((e (* k 24))
                           (type (logand (aref syms (+ e 4)) 15))
                           (value (%bt-u syms (+ e 8) 8))
                           (nm (%bt-u syms e 4)))
                      (when (and (member type '(0 2)) (> value 4096) (plusp nm))
                        (let ((end (or (position 0 strs :start nm) strsize)))
                          (setq pairs (cons (cons value (map 'string (function code-char)
                                                            (subseq strs nm end)))
                                            pairs))))))))))
          (and pairs (sort (coerce pairs 'vector) (function <) :key (function car)))))
    (error () nil)))

(defun %bt-code-table ()
  "A vector of (entry-address . name), sorted by address, for the image's functions: the ELF
   symbol table when it can be read, else what *SYMBOL-FUNCTION-TABLE* publishes."
  (or (%bt-elf-table) (%bt-sft-table)))

(defun %bt-sft-table ()
  "A vector of (entry-address . name), sorted by address, for the image's published functions."
  (let ((pairs nil)
        (scratch (+ (%tls-self-base) (%conv-addr #x100050A0))))
    (maphash (lambda (name f)
               (when (and (stringp name) (functionp f))
                 (let ((w (%gc-word-of f scratch)))
                   (when (= 3 (logand w 15))
                     (setq pairs (cons (cons (- w 3) name) pairs))))))
             *symbol-function-table*)
    (sort (coerce pairs 'vector) (function <) :key (function car))))

(defun %bt-jit-table ()
  "(entry-address . name) for functions the JIT compiled, sorted by address: the entries of
   *SYMBOL-FUNCTION-TABLE* -- which follows every DEFUN, run-time ones included -- whose code
   lives in the JIT arena."
  (let ((pairs nil)
        (scratch (+ (%tls-self-base) (%conv-addr #x100050A0))))
    (maphash (lambda (name f)
               (when (and (stringp name) (functionp f))
                 (let ((w (%gc-word-of f scratch)))
                   (when (and (= 3 (logand w 15)) (>= w #x3000000000))
                     (setq pairs (cons (cons (- w 3) name) pairs))))))
             *symbol-function-table*)
    (sort (coerce pairs 'vector) (function <) :key (function car))))

(defun %bt-lookup (addr table)
  "The entry of TABLE at or below ADDR, or NIL."
  (let ((lo 0) (hi (- (length table) 1)) (best nil))
    (loop
      (when (> lo hi) (return))
      (let ((mid (floor (+ lo hi) 2)))
        (if (<= (car (aref table mid)) addr)
            (setq best mid lo (+ mid 1))
            (setq hi (- mid 1)))))
    (and best (aref table best))))

(defun %bt-name (addr table &optional jit-table)
  "NAME+0xOFF for ADDR, or JIT@/?@ when no known function contains it."
  (cond
    ((>= addr #x3000000000)
     ;; The JIT arena is bump-allocated and only named functions are on record, so code it
     ;; made for a toplevel form or a lambda sits just past some unrelated DEFUN.  Credit the
     ;; nearest name only within 16 KB (a large function); farther, say where it is.
     (let ((e (and jit-table (%bt-lookup addr jit-table))))
       (cond ((and e (< (- addr (car e)) 16384))
              (format nil "~a+0x~x [jit]" (cdr e) (- addr (car e))))
             (e (format nil "jit@~x (past ~a)" addr (cdr e)))
             (t (format nil "jit@~x" addr)))))
    (t (let ((lo 0) (hi (- (length table) 1)) (best nil))
         (loop
           (when (> lo hi) (return))
           (let ((mid (floor (+ lo hi) 2)))
             (if (<= (car (aref table mid)) addr)
                 (setq best mid lo (+ mid 1))
                 (setq hi (- mid 1)))))
         (if best
             (let ((e (aref table best)))
               (format nil "~a+0x~x" (cdr e) (- addr (car e))))
             (format nil "?@~x" addr))))))

(defun %backtrace-list (&optional (max 60))
  "The current stack as a list of frame descriptions, innermost first (this call excluded)."
  (let* ((addrs (%backtrace-addresses max))
         (table (%bt-code-table))
         (jit (and (some (lambda (a) (>= a #x3000000000)) addrs) (%bt-jit-table))))
    (mapcar (lambda (a) (%bt-name a table jit)) addrs)))

(defun %print-backtrace (&optional (stream *error-output*) (max 60))
  (let ((i 0))
    (dolist (f (%backtrace-list max))
      (format stream "~&~3d: ~a~%" i f)
      (setq i (+ i 1))))
  (force-output stream)
  nil)

(defun %fault-backtrace-addresses ()
  "The addresses the signal stub recorded at this thread's last hardware fault -- the faulting
   instruction, then the return addresses up the chain -- or NIL if it recorded none."
  (let* ((base (+ (%tls-self-base) (%conv-addr #x10005200)))
         (n (%bt-word base)) (out nil))
    (when (<= n 63)
      (dotimes (i n) (setq out (cons (%bt-word (+ base 8 (* 8 i))) out))))
    (nreverse out)))

(defun %condition-fault-frames (condition)
  "The frames %ATTACH-FAULT-FRAMES gave CONDITION, or NIL."
  (and condition (%condition-p condition) (%condition-slot condition '%fault-frames)))

(defun %fault-backtrace-list (&optional condition)
  "The stack at a hardware fault, named as %BACKTRACE-LIST names frames: CONDITION's own
   frames when it is a fault's condition, else this thread's last fault."
  (let* ((addrs (or (%condition-fault-frames condition) (%fault-backtrace-addresses)))
         (table (and addrs (%bt-code-table)))
         (jit (and (some (lambda (a) (>= a #x3000000000)) addrs) (%bt-jit-table))))
    (mapcar (lambda (a) (%bt-name a table jit)) addrs)))

(defun %print-fault-backtrace (&optional (stream *error-output*) condition)
  (let ((i 0))
    (dolist (f (%fault-backtrace-list condition))
      (format stream "~&~3d: ~a~%" i f)
      (setq i (+ i 1))))
  (force-output stream)
  nil)
