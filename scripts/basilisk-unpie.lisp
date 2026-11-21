;;;; basilisk-unpie.lisp -- relink an x86-64 PIE as an ET_EXEC at BASE.
;;;;
;;;;   sbcl --script scripts/basilisk-unpie.lisp /usr/bin/BasiliskII-jit BasiliskII-jit-lo 0x10000000
;;;;   MALLOC_MMAP_MAX_=0 ./BasiliskII-jit-lo --jit true --jitcachesize 16384 ...
;;;;
;;;; For Debian's BasiliskII-jit, whose JIT bakes 32-bit absolute addresses of the
;;;; emulator's globals and heap: a PIE loads above 4 GB and they truncate.  At a
;;;; low BASE the executable, and the brk heap the kernel puts after it, are both
;;;; below 2 GB; MALLOC_MMAP_MAX_=0 keeps every malloc in that heap.  Measured
;;;; before: SIGSEGV at 0x558b9b50 (= 0x5555558b9b50 truncated) before the ROM
;;;; loads, with System 7.5.3 as with modus.  After: both boot.
;;;;
;;;; Rewritten: e_type / e_entry, program and section headers, dynamic pointers,
;;;; every relocation offset, RELATIVE addends, defined symbols, the PLT GOT slots
;;;; (lazy binding adds l_addr, which is 0 for an EXEC), DF_1_PIE.  The ELF64
;;;; structures are read here (System V gABI, x86-64 psABI), no library.  It
;;;; replaced basilisk-unpie.py (pyelftools), byte for byte (2026-11-21).

(defpackage #:basilisk-unpie (:use #:cl))
(in-package #:basilisk-unpie)

(defun die (fmt &rest args)
  (format *error-output* "~&basilisk-unpie: ~?~%" fmt args)
  (sb-ext:exit :code 1 :abort t))

(defun read-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in) v)))

(defun le (v off n)
  (loop for i below n sum (ash (aref v (+ off i)) (* 8 i))))
(defun set-le (v off n x)
  (dotimes (i n) (setf (aref v (+ off i)) (ldb (byte 8 (* 8 i)) x))))

(defparameter +ptr-tags+
  ;; DT_PLTGOT HASH STRTAB SYMTAB RELA INIT FINI REL JMPREL INIT_ARRAY FINI_ARRAY
  ;; PREINIT_ARRAY, GNU_HASH VERSYM VERDEF VERNEED
  '(3 4 5 6 7 12 13 17 23 25 26 32 #x6ffffef5 #x6ffffff0 #x6ffffffc #x6ffffffe))
(defconstant +dt-flags-1+ #x6ffffffb)
(defconstant +df-1-pie+ #x08000000)

(defun main (src dst base-str)
  (let* ((orig (read-file src)) (data (copy-seq orig))
         (base (if (and (> (length base-str) 2) (string-equal (subseq base-str 0 2) "0x"))
                   (parse-integer base-str :start 2 :radix 16)
                   (parse-integer base-str)))
         (counts nil))
    (unless (and (= (le orig 16 2) 3) (= (le orig 18 2) 62))   ; ET_DYN, EM_X86_64
      (die "~A is not an x86-64 PIE" src))
    (labels ((u64 (off) (le data off 8))
             (add64 (off &optional (d base)) (set-le data off 8 (logand (+ (u64 off) d) #xFFFFFFFFFFFFFFFF)))
             ;; the ORIGINAL file's structures, as pyelftools read them
             (o64 (off) (le orig off 8)) (o32 (off) (le orig off 4)) (o16 (off) (le orig off 2))
             (v2o (va)                       ; link-time vaddr -> file offset
               (let ((ph (o64 32)) (phs (o16 54)))
                 (dotimes (i (o16 56))
                   (let ((o (+ ph (* i phs))))
                     (when (and (= (o32 o) 1)  ; PT_LOAD
                                (<= (o64 (+ o 16)) va) (< va (+ (o64 (+ o 16)) (o64 (+ o 32)))))
                       (return-from v2o (+ (- va (o64 (+ o 16))) (o64 (+ o 8))))))))))
      (set-le data 16 2 2)                  ; ET_EXEC
      (add64 24)                            ; e_entry
      (let ((ph (o64 32)) (phs (o16 54)))
        (dotimes (i (o16 56))
          (let ((o (+ ph (* i phs))))
            (unless (= (o32 o) #x6474e551)    ; PT_GNU_STACK
              (add64 (+ o 16)) (add64 (+ o 24))))))   ; p_vaddr, p_paddr
      (let* ((sh (o64 40)) (shs (o16 58)) (shn (o16 60))
             (strtab (+ sh (* (o16 62) shs)))
             (strtab-off (o64 (+ strtab 24))))
        (flet ((sec (i) (+ sh (* i shs)))
               (name (s) (let* ((at (+ strtab-off (o32 s))) (end (position 0 orig :start at)))
                           (map 'string #'code-char (subseq orig at end)))))
          (dotimes (i shn)
            (let ((o (sec i))) (unless (zerop (o64 (+ o 16))) (add64 (+ o 16)))))   ; sh_addr
          (dotimes (i shn)
            (let* ((s (sec i)) (type (o32 (+ s 4))) (off (o64 (+ s 24))) (size (o64 (+ s 32))))
              (case type
                (6                              ; SHT_DYNAMIC, to DT_NULL
                 (loop for o from off by 16
                       for tag = (o64 o)
                       until (zerop tag)
                       do (cond ((member tag +ptr-tags+) (add64 (+ o 8)))
                                ((= tag +dt-flags-1+)
                                 (set-le data (+ o 8) 8 (logand (u64 (+ o 8)) (lognot +df-1-pie+)))))))
                (9 (die "a REL section (~A): RELA expected" (name s)))
                (4                              ; SHT_RELA
                 (loop for o from off below (+ off size) by 24
                       for ty = (logand (o64 (+ o 8)) #xFFFFFFFF)
                       do (let ((e (assoc ty counts))) (if e (incf (cdr e)) (setq counts (append counts (list (cons ty 1))))))
                          (case ty
                            (8 (add64 (+ o 16)))  ; RELATIVE: the addend is an address
                            (7 (let ((fo (v2o (o64 o))))   ; JUMP_SLOT: its GOT word holds a PLT address
                                 (when fo (add64 fo))))
                            ((1 5 6) nil)         ; 64, COPY, GLOB_DAT: symbol-based
                            (t (die "relocation type ~D" ty)))
                          (add64 o)))             ; r_offset
                ((2 11)                         ; SHT_SYMTAB, SHT_DYNSYM
                 (loop for o from off below (+ off size) by 24
                       do (let ((shndx (o16 (+ o 6))))
                            (unless (or (= shndx 0) (= shndx #xFFF1) (zerop (o64 (+ o 8))))
                              (add64 (+ o 8)))))))))
          ;; GOT[0] = _DYNAMIC's link address
          (dotimes (i shn)
            (let ((s (sec i)))
              (when (string= (name s) ".got.plt")
                (let ((off (o64 (+ s 24)))) (unless (zerop (u64 off)) (add64 off)))
                (return)))))))
    (with-open-file (o dst :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence data o))
    (format t "basilisk-unpie: ~A -> ~A at 0x~(~x~); relocations {~{~A~^, ~}}~%" src dst base
            (mapcar (lambda (e) (format nil "~D: ~D" (car e) (cdr e))) counts))))

(apply #'main (rest sb-ext:*posix-argv*))
