;;;; mvm/macho-writer.lisp -- lay out, link and ad-hoc sign an arm64 macOS executable.
;;;;
;;;; Input: a SHIM-OBJECT (mvm/macho-cc.lisp: the host's code, strings, data, imports and
;;;; relocations) and the Darwin-layout modus image (an aarch64 ELF, embedded unchanged).
;;;; Output: the bytes of a Mach-O MH_EXECUTE that dyld loads, as Apple's ld would lay it out:
;;;;
;;;;   __PAGEZERO    vm 0 .. 4 GB, no file, no access (arm64 requires it: xnu
;;;;                 bsd/kern/mach_loader.c, "64 bit ARM binary must have hard page zero")
;;;;   __TEXT  r-x   fileoff 0: mach header, load commands,
;;;;                 __text     the host's code
;;;;                 __cstring  its strings
;;;;                 __modus    the image, 16 KB aligned (the host mach_vm_remaps those pages
;;;;                            to the image's link address, as host/macos/build-macos.sh's
;;;;                            -sectcreate / -sectalign put it)
;;;;   __DATA_CONST rw-, SG_READ_ONLY:  __got, one pointer per libSystem import, bound by dyld
;;;;   __DATA  rw-   __data: the host's zero-initialised variables
;;;;   __LINKEDIT r--  bind opcodes, symbol table, indirect symbols, strings, code signature
;;;;
;;;; Every segment starts on a 16 KB page (arm64's page size).  Imports are classic dyld
;;;; info (LC_DYLD_INFO_ONLY bind opcodes into S_NON_LAZY_SYMBOL_POINTERS), the encoding Go's
;;;; linker uses for every darwin/arm64 binary it writes -- the format Apple's ld wrote for all
;;;; deployment targets before macOS 12 and dyld still accepts; there are no rebases, because
;;;; the host's code reaches everything PC-relatively.
;;;;
;;;; THE SIGNATURE is what Apple's ld writes when it links an arm64 executable (ld64
;;;; src/ld/LinkEdit.hpp, CodeSignatureAtom::encode): an embedded-signature SuperBlob holding
;;;; one CodeDirectory, version 0x20400, flags CS_ADHOC | CS_LINKER_SIGNED (0x20002), SHA-256
;;;; over 4 KB pages of everything before the signature, the identifier the output file's leaf
;;;; name, execSeg base 0 / limit __TEXT's file size / CS_EXECSEG_MAIN_BINARY, no special
;;;; slots and no requirements; padded to 8 bytes.  (Go's cmd/internal/codesign implements
;;;; the same.)  Constants: xnu osfmk/kern/cs_blobs.h, EXTERNAL_HEADERS/mach-o/loader.h.

(in-package :modus.macho)

(defconstant +page+ #x4000)
(defconstant +text-vmaddr+ #x100000000)
(defconstant +cs-page-bits+ 12)

;;; ------------------------------------------------------------------ bytes ----

(defun make-octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun put-le (buf at value nbytes)
  (dotimes (i nbytes) (setf (aref buf (+ at i)) (ldb (byte 8 (* 8 i)) value))))
(defun put-be (buf at value nbytes)
  (dotimes (i nbytes) (setf (aref buf (+ at i)) (ldb (byte 8 (* 8 (- nbytes 1 i))) value))))
(defun get-le (buf at nbytes)
  (let ((v 0)) (dotimes (i nbytes v) (setq v (logior v (ash (aref buf (+ at i)) (* 8 i)))))))
(defun get-be (buf at nbytes)
  (let ((v 0)) (dotimes (i nbytes v) (setq v (logior (ash v 8) (aref buf (+ at i)))))))
(defun put-name16 (buf at name)
  (dotimes (i (length name)) (setf (aref buf (+ at i)) (char-code (char name i)))))
(defun ascii-octets (s)
  (let ((o (make-octets (length s))))
    (dotimes (i (length s) o) (setf (aref o i) (char-code (char s i))))))
(defun align-to (n a) (* a (ceiling n a)))

(defun uleb (n)
  (let ((out nil))
    (loop (let ((b (logand n #x7F)))
            (setq n (ash n -7))
            (if (zerop n) (progn (push b out) (return)) (push (logior b #x80) out))))
    (nreverse out)))

;;; ----------------------------------------------------------------- layout ----

(defstruct layout
  hdr-size text-off text-size cstring-off cstring-size modus-off modus-size text-seg-size
  dc-off got-size dc-size data-off data-size data-seg-size
  le-off bind-off bind-size sym-off nsyms ind-off nind str-off str-size sig-off sig-size le-size
  symbols    ; ((NAME TYPE SECT DESC VALUE) ...) in table order
  nlocal nextdef nundef
  imports)   ; ((NAME CSYM RESULT) ...) = GOT order

(defparameter +load-cmd-sizes+
  ;; segment_command_64 = 72, section_64 = 80
  '((:pagezero . 72) (:text . 312) (:data-const . 152) (:data . 152) (:linkedit . 72)
    (:dyld-info . 48) (:symtab . 24) (:dysymtab . 80) (:dylinker . 32) (:uuid . 24)
    (:build-version . 24) (:main . 24) (:dylib . 56) (:code-signature . 16)))

(defun mangle (sym)
  "A shim name as a C symbol: _lower_case."
  (concatenate 'string "_" (substitute #\_ #\- (string-downcase (symbol-name sym)))))

(defun cs-size (code-limit ident)
  "Bytes of the embedded signature for CODE-LIMIT signed bytes (before the 8-byte padding)."
  (+ 12 8 88 (1+ (length ident)) (* 32 (ceiling code-limit (ash 1 +cs-page-bits+)))))

(defun compute-layout (obj elf-length ident)
  (let* ((l (make-layout))
         (sizeofcmds (reduce #'+ (mapcar #'cdr +load-cmd-sizes+)))
         (imports (shim-object-imports obj)))
    (setf (layout-imports l) imports)
    (setf (layout-hdr-size l) (+ 32 sizeofcmds))
    (setf (layout-text-off l) (align-to (layout-hdr-size l) 16)
          (layout-text-size l) (* 4 (length (shim-object-text obj))))
    (setf (layout-cstring-off l) (+ (layout-text-off l) (layout-text-size l))
          (layout-cstring-size l) (length (shim-object-cstring obj)))
    (setf (layout-modus-off l) (align-to (+ (layout-cstring-off l) (layout-cstring-size l)) +page+)
          (layout-modus-size l) elf-length)
    (setf (layout-text-seg-size l) (align-to (+ (layout-modus-off l) elf-length) +page+))
    (setf (layout-dc-off l) (layout-text-seg-size l)
          (layout-got-size l) (* 8 (length imports))
          (layout-dc-size l) (align-to (max 8 (* 8 (length imports))) +page+))
    (setf (layout-data-off l) (+ (layout-dc-off l) (layout-dc-size l))
          (layout-data-size l) (max 8 (align-to (shim-object-data-size obj) 8))
          (layout-data-seg-size l) (align-to (max 8 (shim-object-data-size obj)) +page+))
    ;; the symbol table: locals (the host's functions), the one external definition, the
    ;; imports (undefined), each group sorted by name as ld64 sorts them
    (let* ((locals (sort (mapcar (lambda (f) (list (mangle f) #x0E 1 0 f))
                                 (shim-object-functions obj))
                         #'string< :key #'first))
           (extdef (list (list "__mh_execute_header" #x0F 1 #x10 :mh-header)))
           (undef (sort (mapcar (lambda (i) (list (second i) #x01 0 #x0100 0)) imports)
                        #'string< :key #'first)))
      (setf (layout-symbols l) (append locals extdef undef)
            (layout-nlocal l) (length locals) (layout-nextdef l) 1 (layout-nundef l) (length undef)))
    (let* ((le (+ (layout-data-off l) (layout-data-seg-size l)))
           (bind (bind-opcodes l))
           (nsyms (length (layout-symbols l)))
           (strsize (align-to (+ 1 (reduce #'+ (mapcar (lambda (s) (1+ (length (first s))))
                                                       (layout-symbols l))))
                              8)))
      (setf (layout-le-off l) le
            (layout-bind-off l) le (layout-bind-size l) (length bind)
            (layout-sym-off l) (+ le (length bind)) (layout-nsyms l) nsyms)
      (setf (layout-ind-off l) (+ (layout-sym-off l) (* 16 nsyms)) (layout-nind l) (length imports))
      (setf (layout-str-off l) (+ (layout-ind-off l) (* 4 (length imports))) (layout-str-size l) strsize)
      (setf (layout-sig-off l) (align-to (+ (layout-str-off l) strsize) 16))
      (setf (layout-sig-size l) (align-to (cs-size (layout-sig-off l) ident) 8))
      (setf (layout-le-size l) (- (+ (layout-sig-off l) (layout-sig-size l)) le)))
    l))

(defun bind-opcodes (l)
  "BIND opcodes binding each GOT slot (segment 2, __DATA_CONST) to its libSystem symbol."
  (let ((out nil))
    (loop for imp in (layout-imports l) for i from 0
          do (push #x11 out)                                   ; SET_DYLIB_ORDINAL_IMM 1
             (push #x40 out)                                   ; SET_SYMBOL_TRAILING_FLAGS_IMM 0
             (loop for ch across (second imp) do (push (char-code ch) out))
             (push 0 out)
             (push #x51 out)                                   ; SET_TYPE_IMM BIND_TYPE_POINTER
             (push #x72 out)                                   ; SET_SEGMENT_AND_OFFSET_ULEB seg 2
             (dolist (b (uleb (* 8 i))) (push b out))
             (push #x90 out))                                  ; DO_BIND
    (push #x00 out)                                            ; DONE
    (let ((v (coerce (nreverse out) 'list)))
      (append v (make-list (- (align-to (length v) 8) (length v)) :initial-element 0)))))

;;; ------------------------------------------------------------- relocation ----

(defun section-vma (l section)
  (+ +text-vmaddr+
     (ecase section
       (:text (layout-text-off l)) (:cstring (layout-cstring-off l)) (:modus (layout-modus-off l))
       (:data (layout-data-off l)) (:got (layout-dc-off l)))))

(defun label-vma (obj l target)
  (cond ((and (consp target) (eq (first target) :got))
         (let ((i (position (second target) (layout-imports l) :key #'first)))
           (unless i (error "macho: no import ~S" (second target)))
           (+ (section-vma l :got) (* 8 i))))
        ((eq target 'modus-image) (section-vma l :modus))
        (t (let ((e (gethash target (shim-object-labels obj))))
             (unless e (error "macho: undefined label ~S" target))
             (+ (section-vma l (car e)) (cdr e))))))

(defun link-text (obj l)
  "The host's code with every relocation resolved, as octets."
  (let* ((words (copy-seq (shim-object-text obj)))
         (text-vma (section-vma l :text)))
    (dolist (r (shim-object-relocs obj))
      (destructuring-bind (index kind target) r
        (let* ((p (+ text-vma (* 4 index)))
               (s (label-vma obj l target))
               (w (aref words index)))
          (setf (aref words index)
                (ecase kind
                  (:b26 (logior w (sbits (/ (- s p) 4) 26)))
                  (:b19 (logior w (ash (sbits (/ (- s p) 4) 19) 5)))
                  (:adrp (let ((d (- (ash s -12) (ash p -12))))
                           (logior w (ash (ldb (byte 2 0) (sbits d 21)) 29)
                                   (ash (ldb (byte 19 2) (sbits d 21)) 5))))
                  (:add-lo12 (logior w (ash (ldb (byte 12 0) s) 10)))
                  (:ldr64-lo12 (unless (zerop (mod s 8)) (error "macho: unaligned LDR target ~S" target))
                   (logior w (ash (/ (ldb (byte 12 0) s) 8) 10))))))))
    (let ((o (make-octets (* 4 (length words)))))
      (dotimes (i (length words) o) (put-le o (* 4 i) (aref words i) 4)))))

;;; ----------------------------------------------------------------- writer ----

(defun write-segment (buf at name vmaddr vmsize fileoff filesize maxprot initprot nsects flags)
  (put-le buf at #x19 4) (put-le buf (+ at 4) (+ 72 (* 80 nsects)) 4)
  (put-name16 buf (+ at 8) name)
  (put-le buf (+ at 24) vmaddr 8) (put-le buf (+ at 32) vmsize 8)
  (put-le buf (+ at 40) fileoff 8) (put-le buf (+ at 48) filesize 8)
  (put-le buf (+ at 56) maxprot 4) (put-le buf (+ at 60) initprot 4)
  (put-le buf (+ at 64) nsects 4) (put-le buf (+ at 68) flags 4)
  (+ at 72))
(defun write-section (buf at sect seg addr size offset align flags &optional (reserved1 0))
  (put-name16 buf at sect) (put-name16 buf (+ at 16) seg)
  (put-le buf (+ at 32) addr 8) (put-le buf (+ at 40) size 8)
  (put-le buf (+ at 48) offset 4) (put-le buf (+ at 52) align 4)
  (put-le buf (+ at 56) 0 4) (put-le buf (+ at 60) 0 4)          ; reloff, nreloc
  (put-le buf (+ at 64) flags 4) (put-le buf (+ at 68) reserved1 4)
  (put-le buf (+ at 72) 0 4) (put-le buf (+ at 76) 0 4)
  (+ at 80))

(defun build-macho (obj elf &key (ident "modus") sha256 (minos #x000E0400) (sdk #x000E0400))
  "The executable's bytes.  ELF: the image (octets).  SHA256: (lambda (octets start end)) ->
   32 octets.  MINOS / SDK: LC_BUILD_VERSION's xxxx.yy.zz words (default 14.4, the oldest
   macOS with os_sync_wait_on_address)."
  (let* ((l (compute-layout obj (length elf) ident))
         (size (+ (layout-le-off l) (layout-le-size l)))
         (buf (make-octets size))
         (le-vmsize (align-to (layout-le-size l) +page+))
         (entry (gethash (shim-object-entry obj) (shim-object-labels obj)))
         (at 32))
    ;; header
    (put-le buf 0 #xFEEDFACF 4) (put-le buf 4 #x0100000C 4) (put-le buf 8 0 4)
    (put-le buf 12 2 4)                                                ; MH_EXECUTE
    (put-le buf 16 (length +load-cmd-sizes+) 4)
    (put-le buf 20 (- (layout-hdr-size l) 32) 4)
    (put-le buf 24 #x00200085 4)              ; MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE
    (put-le buf 28 0 4)
    ;; segments
    (setq at (write-segment buf at "__PAGEZERO" 0 #x100000000 0 0 0 0 0 0))
    (setq at (write-segment buf at "__TEXT" +text-vmaddr+ (layout-text-seg-size l) 0
                            (layout-text-seg-size l) 5 5 3 0))
    (setq at (write-section buf at "__text" "__TEXT" (section-vma l :text) (layout-text-size l)
                            (layout-text-off l) 4 #x80000400))  ; PURE_INSTRUCTIONS|SOME_INSTRUCTIONS
    (setq at (write-section buf at "__cstring" "__TEXT" (section-vma l :cstring)
                            (layout-cstring-size l) (layout-cstring-off l) 0 #x2))  ; S_CSTRING_LITERALS
    (setq at (write-section buf at "__modus" "__TEXT" (section-vma l :modus) (layout-modus-size l)
                            (layout-modus-off l) 14 0))
    (setq at (write-segment buf at "__DATA_CONST" (section-vma l :got) (layout-dc-size l)
                            (layout-dc-off l) (layout-dc-size l) 3 3 1 #x10))      ; SG_READ_ONLY
    (setq at (write-section buf at "__got" "__DATA_CONST" (section-vma l :got) (layout-got-size l)
                            (layout-dc-off l) 3 #x6 0))     ; S_NON_LAZY_SYMBOL_POINTERS, indirect 0
    (setq at (write-segment buf at "__DATA" (section-vma l :data) (layout-data-seg-size l)
                            (layout-data-off l) (layout-data-seg-size l) 3 3 1 0))
    (setq at (write-section buf at "__data" "__DATA" (section-vma l :data) (layout-data-size l)
                            (layout-data-off l) 4 0))
    (setq at (write-segment buf at "__LINKEDIT" (+ +text-vmaddr+ (layout-le-off l)) le-vmsize
                            (layout-le-off l) (layout-le-size l) 1 1 0 0))
    ;; LC_DYLD_INFO_ONLY
    (put-le buf at #x80000022 4) (put-le buf (+ at 4) 48 4)
    (put-le buf (+ at 16) (layout-bind-off l) 4) (put-le buf (+ at 20) (layout-bind-size l) 4)
    (incf at 48)
    ;; LC_SYMTAB
    (put-le buf at #x2 4) (put-le buf (+ at 4) 24 4)
    (put-le buf (+ at 8) (layout-sym-off l) 4) (put-le buf (+ at 12) (layout-nsyms l) 4)
    (put-le buf (+ at 16) (layout-str-off l) 4) (put-le buf (+ at 20) (layout-str-size l) 4)
    (incf at 24)
    ;; LC_DYSYMTAB
    (put-le buf at #xB 4) (put-le buf (+ at 4) 80 4)
    (put-le buf (+ at 8) 0 4) (put-le buf (+ at 12) (layout-nlocal l) 4)
    (put-le buf (+ at 16) (layout-nlocal l) 4) (put-le buf (+ at 20) (layout-nextdef l) 4)
    (put-le buf (+ at 24) (+ (layout-nlocal l) (layout-nextdef l)) 4) (put-le buf (+ at 28) (layout-nundef l) 4)
    (put-le buf (+ at 56) (layout-ind-off l) 4) (put-le buf (+ at 60) (layout-nind l) 4)
    (incf at 80)
    ;; LC_LOAD_DYLINKER
    (put-le buf at #xE 4) (put-le buf (+ at 4) 32 4) (put-le buf (+ at 8) 12 4)
    (put-name16 buf (+ at 12) "/usr/lib/dyld")
    (incf at 32)
    ;; LC_UUID (filled in below)
    (let ((uuid-at (+ at 8)))
      (put-le buf at #x1B 4) (put-le buf (+ at 4) 24 4)
      (incf at 24)
      ;; LC_BUILD_VERSION: macOS, no tools
      (put-le buf at #x32 4) (put-le buf (+ at 4) 24 4)
      (put-le buf (+ at 8) 1 4) (put-le buf (+ at 12) minos 4) (put-le buf (+ at 16) sdk 4)
      (put-le buf (+ at 20) 0 4)
      (incf at 24)
      ;; LC_MAIN
      (put-le buf at #x80000028 4) (put-le buf (+ at 4) 24 4)
      (put-le buf (+ at 8) (+ (layout-text-off l) (cdr entry)) 8) (put-le buf (+ at 16) 0 8)
      (incf at 24)
      ;; LC_LOAD_DYLIB libSystem: timestamp 2, current 1351.0.0, compatibility 1.0.0
      (put-le buf at #xC 4) (put-le buf (+ at 4) 56 4) (put-le buf (+ at 8) 24 4)
      (put-le buf (+ at 12) 2 4) (put-le buf (+ at 16) #x05470000 4) (put-le buf (+ at 20) #x00010000 4)
      (let ((p "/usr/lib/libSystem.B.dylib"))
        (dotimes (i (length p)) (setf (aref buf (+ at 24 i)) (char-code (char p i)))))
      (incf at 56)
      ;; LC_CODE_SIGNATURE
      (put-le buf at #x1D 4) (put-le buf (+ at 4) 16 4)
      (put-le buf (+ at 8) (layout-sig-off l) 4) (put-le buf (+ at 12) (layout-sig-size l) 4)
      (incf at 16)
      (assert (= at (layout-hdr-size l)))
      ;; contents
      (replace buf (link-text obj l) :start1 (layout-text-off l))
      (replace buf (shim-object-cstring obj) :start1 (layout-cstring-off l))
      (replace buf elf :start1 (layout-modus-off l))
      (replace buf (coerce (bind-opcodes l) 'list) :start1 (layout-bind-off l))
      ;; symbols and strings
      (let ((stro 1) (strs (make-hash-table :test 'equal)))
        (loop for s in (layout-symbols l) for i from 0
              do (destructuring-bind (name type sect desc value) s
                   (let ((at (+ (layout-sym-off l) (* 16 i))))
                     (setf (gethash name strs) stro)
                     (put-le buf at stro 4) (setf (aref buf (+ at 4)) type (aref buf (+ at 5)) sect)
                     (put-le buf (+ at 6) desc 2)
                     (put-le buf (+ at 8) (cond ((eq value :mh-header) +text-vmaddr+)
                                                ((symbolp value) (label-vma obj l value))
                                                (t value))
                             8)
                     (dotimes (k (length name))
                       (setf (aref buf (+ (layout-str-off l) stro k)) (char-code (char name k))))
                     (incf stro (1+ (length name))))))
        ;; the indirect symbol table: GOT slot I -> the undefined symbol for import I
        (loop for imp in (layout-imports l) for i from 0
              do (put-le buf (+ (layout-ind-off l) (* 4 i))
                         (position (second imp) (layout-symbols l) :key #'first :test #'string=) 4)))
      ;; UUID: a digest of the unsigned file, with the RFC 4122 variant bits
      (let ((d (funcall sha256 buf 0 (layout-sig-off l))))
        (dotimes (i 16) (setf (aref buf (+ uuid-at i)) (aref d i)))
        (setf (aref buf (+ uuid-at 6)) (logior #x50 (logand (aref buf (+ uuid-at 6)) #x0F))
              (aref buf (+ uuid-at 8)) (logior #x80 (logand (aref buf (+ uuid-at 8)) #x3F))))
      (write-signature buf (layout-sig-off l) ident (layout-text-seg-size l) sha256)
      (values buf l))))

(defun write-signature (buf sig-off ident text-limit sha256)
  "Write the ad-hoc, linker-signed embedded signature for BUF[0, SIG-OFF) at SIG-OFF."
  (let* ((nslots (ceiling sig-off (ash 1 +cs-page-bits+)))
         (cd-len (+ 88 (1+ (length ident)) (* 32 nslots)))
         (total (+ 20 cd-len))
         (cd (+ sig-off 20))
         (hash-off (+ 88 (1+ (length ident)))))
    ;; SuperBlob: CSMAGIC_EMBEDDED_SIGNATURE, length, one index entry (CSSLOT_CODEDIRECTORY @20)
    (put-be buf sig-off #xFADE0CC0 4) (put-be buf (+ sig-off 4) total 4) (put-be buf (+ sig-off 8) 1 4)
    (put-be buf (+ sig-off 12) 0 4) (put-be buf (+ sig-off 16) 20 4)
    ;; CodeDirectory
    (put-be buf cd #xFADE0C02 4) (put-be buf (+ cd 4) cd-len 4)
    (put-be buf (+ cd 8) #x20400 4)                    ; version: CS_SUPPORTSEXECSEG
    (put-be buf (+ cd 12) #x20002 4)                   ; CS_ADHOC | CS_LINKER_SIGNED
    (put-be buf (+ cd 16) hash-off 4) (put-be buf (+ cd 20) 88 4)    ; hashOffset, identOffset
    (put-be buf (+ cd 24) 0 4) (put-be buf (+ cd 28) nslots 4)       ; nSpecialSlots, nCodeSlots
    (put-be buf (+ cd 32) sig-off 4)                   ; codeLimit
    (setf (aref buf (+ cd 36)) 32 (aref buf (+ cd 37)) 2              ; hashSize, CS_HASHTYPE_SHA256
          (aref buf (+ cd 38)) 0 (aref buf (+ cd 39)) +cs-page-bits+) ; platform, pageSize
    (put-be buf (+ cd 40) 0 4) (put-be buf (+ cd 44) 0 4) (put-be buf (+ cd 48) 0 4)  ; spare2, scatter, team
    (put-be buf (+ cd 52) 0 4) (put-be buf (+ cd 56) 0 8)            ; spare3, codeLimit64
    (put-be buf (+ cd 64) 0 8) (put-be buf (+ cd 72) text-limit 8)   ; execSegBase, execSegLimit
    (put-be buf (+ cd 80) 1 8)                                       ; CS_EXECSEG_MAIN_BINARY
    (dotimes (i (length ident)) (setf (aref buf (+ cd 88 i)) (char-code (char ident i))))
    (setf (aref buf (+ cd 88 (length ident))) 0)
    (dotimes (k nslots)
      (let ((d (funcall sha256 buf (* k 4096) (min sig-off (* (1+ k) 4096)))))
        (replace buf d :start1 (+ cd hash-off (* 32 k)))))
    buf))

;;; ---------------------------------------------------------------- the check ----
;;; Re-read the finished bytes from scratch -- not from the layout that wrote them -- and
;;; check what dyld and the kernel check (dyld mach_o/UnsafeHeader.cpp and Image.cpp,
;;; xnu bsd/kern/mach_loader.c), then recompute every code-directory hash.  The build fails
;;; on any mismatch.  host/macos/check-macho.py is the independent (Python) checker.

(defun verify-macho (buf sha256 &key elf)
  (flet ((u32 (at) (get-le buf at 4)) (u64 (at) (get-le buf at 8))
         (fail (fmt &rest args) (error "verify-macho: ~?" fmt args))
         (name16 (at) (let ((e (or (position 0 buf :start at :end (+ at 16)) (+ at 16))))
                        (map 'string #'code-char (subseq buf at e)))))
    (unless (= (u32 0) #xFEEDFACF) (fail "bad magic"))
    (unless (= (u32 4) #x0100000C) (fail "not arm64"))
    (unless (= (u32 12) 2) (fail "not MH_EXECUTE"))
    (unless (= (logand (u32 24) #x200085) #x200085) (fail "flags ~X lack NOUNDEFS|DYLDLINK|TWOLEVEL|PIE" (u32 24)))
    (let ((ncmds (u32 16)) (at 32) (segs nil) (seen nil) (sig nil) (main nil) (dyld-info nil)
          (end (+ 32 (u32 20))))
      (dotimes (i ncmds)
        (let ((cmd (u32 at)) (cs (u32 (+ at 4))))
          (when (or (< cs 8) (/= 0 (mod cs 8)) (> (+ at cs) end)) (fail "load command ~D size ~D" i cs))
          (push cmd seen)
          (case cmd
            (#x19 (let ((nsects (u32 (+ at 64))))
                    (unless (= cs (+ 72 (* 80 nsects))) (fail "segment ~D size" i))
                    (push (list (name16 (+ at 8)) (u64 (+ at 24)) (u64 (+ at 32)) (u64 (+ at 40))
                                (u64 (+ at 48)) (u32 (+ at 56)) (u32 (+ at 60)) (u32 (+ at 68))
                                (loop for k below nsects
                                      collect (let ((s (+ at 72 (* 80 k))))
                                                (list (name16 s) (u64 (+ s 32)) (u64 (+ s 40))
                                                      (u32 (+ s 48)) (u32 (+ s 52)) (u32 (+ s 64))))))
                          segs)))
            (#x1D (setq sig (list (u32 (+ at 8)) (u32 (+ at 12)))))
            (#x80000028 (setq main (u64 (+ at 8))))
            (#x80000022 (setq dyld-info (list (u32 (+ at 16)) (u32 (+ at 20))))))
          (incf at cs)))
      (unless (= at end) (fail "load commands end at ~D, not ~D" at end))
      (setq segs (nreverse segs))
      (dolist (need '(#x0E #x0C #x1B #x32 #x80000028 #x1D #x2 #xB #x80000022))
        (unless (member need seen) (fail "missing load command #x~X" need)))
      (let ((prev nil))
        (dolist (s segs)
          (destructuring-bind (name vm vmsize off fsize maxp initp flags sects) s
            (declare (ignore maxp))
            (unless (or (string= name "__PAGEZERO") (zerop (mod vm +page+))) (fail "~A not page aligned" name))
            (unless (zerop (mod off +page+)) (fail "~A file offset not page aligned" name))
            (when (> fsize vmsize) (fail "~A filesize > vmsize" name))
            (when (> (+ off fsize) (length buf)) (fail "~A runs past the file" name))
            (cond ((string= name "__PAGEZERO")
                   (unless (and (= vm 0) (= vmsize #x100000000) (= initp 0)) (fail "bad __PAGEZERO")))
                  ((string= name "__TEXT")
                   (unless (and (= off 0) (= initp 5)) (fail "__TEXT must start the file, r-x")))
                  ((string= name "__LINKEDIT") (unless (= initp 1) (fail "__LINKEDIT must be r--")))
                  ((string= name "__DATA_CONST")
                   (unless (and (= initp 3) (logtest flags #x10)) (fail "__DATA_CONST must be rw-, SG_READ_ONLY")))
                  ((string= name "__DATA") (unless (= initp 3) (fail "__DATA must be rw-"))))
            (when (and prev (string/= name "__PAGEZERO"))
              (unless (>= vm (+ (second prev) (third prev))) (fail "~A overlaps or precedes ~A" name (first prev)))
              (unless (or (string= (first prev) "__PAGEZERO") (>= off (+ (fourth prev) (fifth prev))))
                (fail "~A file range overlaps ~A" name (first prev))))
            (dolist (sc sects)
              (destructuring-bind (sname addr size soff align sflags) sc
                (declare (ignore sflags))
                (unless (and (>= addr vm) (<= (+ addr size) (+ vm vmsize))) (fail "section ~A outside ~A" sname name))
                (unless (zerop (mod addr (ash 1 align))) (fail "section ~A misaligned" sname))
                (unless (= (- soff off) (- addr vm)) (fail "section ~A file/vm offsets disagree" sname))))
            (setq prev s))))
      (let* ((text (find "__TEXT" segs :key #'first :test #'string=))
             (le (find "__LINKEDIT" segs :key #'first :test #'string=))
             (text-sects (ninth text))
             (code (find "__text" text-sects :key #'first :test #'string=))
             (modus (find "__modus" text-sects :key #'first :test #'string=)))
        (unless (and main (>= main (fourth code)) (< main (+ (fourth code) (third code))))
          (fail "LC_MAIN entry ~X is not in __text" main))
        (unless (zerop (mod (fourth modus) +page+)) (fail "__modus not 16 KB aligned"))
        (when elf
          (unless (and (= (third modus) (length elf))
                       (not (mismatch elf buf :start2 (fourth modus) :end2 (+ (fourth modus) (length elf)))))
            (fail "__modus is not the image")))
        (destructuring-bind (sig-off sig-size) sig
          (unless (= (+ sig-off sig-size) (length buf) (+ (fourth le) (fifth le)))
            (fail "the signature must end __LINKEDIT and the file"))
          (unless (zerop (mod sig-off 16)) (fail "signature misaligned"))
          (unless (and (= (get-be buf sig-off 4) #xFADE0CC0) (= (get-be buf (+ sig-off 8) 4) 1)
                       (= (get-be buf (+ sig-off 12) 4) 0))
            (fail "bad SuperBlob"))
          (let* ((cd (+ sig-off (get-be buf (+ sig-off 16) 4)))
                 (nslots (get-be buf (+ cd 28) 4)) (limit (get-be buf (+ cd 32) 4))
                 (hash-off (get-be buf (+ cd 16) 4)))
            (unless (and (= (get-be buf cd 4) #xFADE0C02) (= (get-be buf (+ cd 8) 4) #x20400)
                         (= (get-be buf (+ cd 12) 4) #x20002) (= (get-be buf (+ cd 24) 4) 0)
                         (= (aref buf (+ cd 36)) 32) (= (aref buf (+ cd 37)) 2) (= (aref buf (+ cd 39)) 12)
                         (= limit sig-off) (= nslots (ceiling limit 4096))
                         (= (get-be buf (+ cd 64) 8) 0) (= (get-be buf (+ cd 72) 8) (fifth text))
                         (= (get-be buf (+ cd 80) 8) 1))
              (fail "bad CodeDirectory fields"))
            (dotimes (k nslots)
              (let ((d (funcall sha256 buf (* k 4096) (min limit (* (1+ k) 4096)))))
                (when (mismatch d buf :start2 (+ cd hash-off (* 32 k)) :end2 (+ cd hash-off (* 32 (1+ k))))
                  (fail "page ~D hash mismatch" k))))))
        (destructuring-bind (bo bs) dyld-info
          (unless (and (>= bo (fourth le)) (<= (+ bo bs) (+ (fourth le) (fifth le))) (zerop (mod bo 8)))
            (fail "bind opcodes outside __LINKEDIT or misaligned"))))
      (length segs))))
