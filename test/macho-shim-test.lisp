;;;; test/macho-shim-test.lisp -- build the macOS host code (host/macos/shim.lisp) plus
;;;; test/macho-shim-cases.lisp into a static Linux/aarch64 ELF whose libSystem imports are
;;;; test doubles, for test/run-macho-shim-test.sh to run under qemu-aarch64.
;;;;
;;;;   MODUS_MACHO_TEST_OUT=/tmp/x.elf sbcl --script test/macho-shim-test.lisp
;;;;
;;;; The code is linked exactly as mvm/macho-writer.lisp links it for macOS (LINK-TEXT, the
;;;; same relocations, the same 0x100000000 base), only the container differs: an ELF with one
;;;; RWX PT_LOAD, whose GOT slots hold the doubles' addresses instead of dyld's binds.

(defparameter *tree*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (truename *load-truename*))))
(load (merge-pathnames "mvm/macho-cc.lisp" *tree*))
(load (merge-pathnames "mvm/macho-writer.lisp" *tree*))
(in-package :modus.macho)

(defun getenv* (n) (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") n))) (and v (plusp (length v)) v)))
(defun read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-octets (file-length s)))) (read-sequence b s) b)))

;;; MODUS_MACHO_E2E_IMAGE: instead of the unit cases, link the Linux-backed libSystem doubles
;;; (test/macho-e2e-doubles.lisp) and embed that image, 16 KB aligned, as __modus would be.
(let* ((e2e (getenv* "MODUS_MACHO_E2E_IMAGE"))
       (image (and e2e (read-octets e2e)))
       (forms (append (read-shim-file (merge-pathnames "host/macos/shim.lisp" cl-user::*tree*))
                      (read-shim-file (merge-pathnames (if e2e "test/macho-e2e-doubles.lisp"
                                                           "test/macho-shim-cases.lisp")
                                                       cl-user::*tree*))))
       (obj (compile-shim forms :entry (if e2e 'e2e-start 'test-start)))
       (l (make-layout))
       (text-off #x1000)
       (text-size (* 4 (length (shim-object-text obj))))
       (cs-off (+ text-off text-size))
       (modus-off (align-to (+ cs-off (length (shim-object-cstring obj))) #x4000))
       (data-off (align-to (+ (if image (+ modus-off (length image)) cs-off)
                              (length (shim-object-cstring obj)))
                           16))
       (got-off (align-to (+ data-off (shim-object-data-size obj)) 16))
       (size (+ got-off (* 8 (length (shim-object-imports obj)))))
       (buf (make-octets size))
       (out (or (getenv* "MODUS_MACHO_TEST_OUT") "macho-shim-test.elf")))
  (setf (layout-text-off l) text-off (layout-cstring-off l) cs-off (layout-data-off l) data-off
        (layout-dc-off l) got-off (layout-modus-off l) modus-off (layout-imports l) (shim-object-imports obj))
  (when image (replace buf image :start1 modus-off))
  (replace buf (link-text obj l) :start1 text-off)
  (replace buf (shim-object-cstring obj) :start1 cs-off)
  (let ((missing (label-vma obj l 'impl-missing)) (bound 0))
    (loop for imp in (shim-object-imports obj) for i from 0
          do (let* ((impl (intern (format nil "IMPL-~A" (symbol-name (first imp))) :modus.macho))
                    (have (gethash impl (shim-object-labels obj))))
               (when have (incf bound))
               (put-le buf (+ got-off (* 8 i)) (if have (label-vma obj l impl) missing) 8)))
    (format t "~&macho-shim-test: ~D of ~D imports have test doubles~%" bound (length (shim-object-imports obj))))
  ;; ELF64 header + one PT_LOAD (RWX) at 0x100000000
  (replace buf #(#x7F #x45 #x4C #x46 2 1 1 0))
  (put-le buf 16 2 2) (put-le buf 18 183 2) (put-le buf 20 1 4)
  (put-le buf 24 (label-vma obj l (if e2e 'e2e-start 'test-start)) 8)
  (put-le buf 32 64 8) (put-le buf 52 64 2) (put-le buf 54 56 2) (put-le buf 56 1 2)
  (put-le buf 64 1 4) (put-le buf 68 7 4) (put-le buf 72 0 8)
  (put-le buf 80 +text-vmaddr+ 8) (put-le buf 88 +text-vmaddr+ 8)
  (put-le buf 96 size 8) (put-le buf 104 size 8) (put-le buf 112 #x10000 8)
  (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence buf o))
  (format t "macho-shim-test: wrote ~A (~D bytes of code)~%" out text-size))
