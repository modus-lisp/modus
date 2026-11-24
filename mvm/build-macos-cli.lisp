;;;; mvm/build-macos-cli.lisp -- the native macOS/arm64 modus CLI, with no Apple toolchain.
;;;;
;;;;   MODUS_MACOS_IMAGE=modus-darwin.elf MODUS_MACOS_OUT=modus \
;;;;     sbcl --dynamic-space-size 4096 --script mvm/build-macos-cli.lisp
;;;;   (or a modus builder CLI running this script: the same bytes)
;;;;
;;;; What host/macos/build-macos.sh IMAGE OUT cli makes with Apple clang + ld + codesign, made
;;;; here from source: host/macos/shim.lisp (the port of modus-shim.c and syscall-stub.S)
;;;; compiled by mvm/macho-cc.lisp, linked with the image into a Mach-O and signed ad hoc by
;;;; mvm/macho-writer.lisp.  The image is the Darwin-layout aarch64 ELF that build-macos.sh's
;;;; header builds:
;;;;   MODUS_DARWIN=1 MODUS_CODE_BASE=7000010000 MODUS_CONV_DELTA=7040000000 \
;;;;   MODUS_HEAP_BASE=7080000000 MODUS_JIT_ARENA_BASE=7100000000 MODUS_CLI_OUT=modus-darwin.elf \
;;;;   sbcl --dynamic-space-size 16384 --script mvm/build-aarch64-cli.lisp
;;;;
;;;; Knobs:
;;;;   MODUS_MACOS_IMAGE   the image (required)
;;;;   MODUS_MACOS_OUT     the executable (default "modus"); its leaf name is the signing identifier
;;;;   MODUS_NATRIUM       natrium's checkout (SHA-256 for the code signature; the workspace's
;;;;                       canonical home).  Default: the nearest natrium/ beside an ancestor
;;;;                       of this tree.
;;;;   MODUS_MACOS_SHIM_MAP  write the host code's symbol map (name address) here
;;;;
;;;; THE BUILD CHECKS ITSELF: after writing, VERIFY-MACHO re-reads the bytes and checks the
;;;; header, every load command, segment and section placement, the image's bytes, the
;;;; CodeDirectory's fields and every page hash.  host/macos/check-macho.py is a second,
;;;; independent checker.
;;;;
;;;; It uses nothing of the modus compiler or translator, so a modus builder needs no
;;;; emission flags pinned (mvm/builder.lisp *BUILDER-HOST-JIT-FLAG-NAMES*): the bytes depend
;;;; only on the shim source, the image and this code.

(defun macos-env (name &optional default)
  (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") name)))
    (if (and v (plusp (length v))) v default)))

(defparameter *macos-tree*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (truename *load-truename*))))

(defun macos-find-natrium ()
  (let ((explicit (macos-env "MODUS_NATRIUM")))
    (if explicit
        (let ((d (if (char= (char explicit (1- (length explicit))) #\/) explicit
                     (concatenate 'string explicit "/"))))
          (unless (probe-file (concatenate 'string d "src/sha256.lisp"))
            (error "build-macos-cli: MODUS_NATRIUM=~A has no src/sha256.lisp" explicit))
          d)
        (let ((dir (truename *macos-tree*)))
          (dotimes (i 8 (error "build-macos-cli: no natrium checkout found; set MODUS_NATRIUM"))
            (let ((cand (merge-pathnames "natrium/src/sha256.lisp" dir)))
              (when (probe-file cand)
                (return (namestring (merge-pathnames "natrium/" dir)))))
            (setq dir (merge-pathnames "../" dir)))))))

;;; SHA-256 is natrium's (the canonical home: workspace CLAUDE.md), loaded from source.
(let ((nat (macos-find-natrium)))
  (unless (find-package "NATRIUM")
    (dolist (f '("src/gossamer.lisp" "src/packages.lisp" "src/util.lisp" "src/sha256.lisp"))
      (load (concatenate 'string nat f))))
  (format t "~&build-macos-cli: SHA-256 from ~A~%" nat))

(load (merge-pathnames "mvm/macho-cc.lisp" *macos-tree*))
(load (merge-pathnames "mvm/macho-writer.lisp" *macos-tree*))

(defun macos-sha256 (octets start end)
  (let ((st (funcall (find-symbol "MAKE-SHA256-STATE" "NATRIUM"))))
    (funcall (find-symbol "SHA256-UPDATE" "NATRIUM") st octets :start start :end end)
    (funcall (find-symbol "SHA256-FINAL-INTO" "NATRIUM") st
             (make-array 32 :element-type '(unsigned-byte 8)))))

(defun macos-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s)
      b)))

(let* ((image-path (or (macos-env "MODUS_MACOS_IMAGE") (error "build-macos-cli: set MODUS_MACOS_IMAGE")))
       (out (macos-env "MODUS_MACOS_OUT" "modus"))
       (leaf (let ((s (position #\/ out :from-end t))) (if s (subseq out (1+ s)) out)))
       (elf (macos-read image-path))
       (obj (modus.macho:compile-shim
             (modus.macho:read-shim-file (merge-pathnames "host/macos/shim.lisp" *macos-tree*)))))
  #+modus (funcall (find-symbol "JIT-EAGER" "MODUS"))      ; the hashing loop: native first
  (multiple-value-bind (bytes layout)
      (modus.macho::build-macho obj elf :ident leaf :sha256 #'macos-sha256)
    (let ((nseg (modus.macho::verify-macho bytes #'macos-sha256 :elf elf)))
      (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence bytes o))
      (let ((map (macos-env "MODUS_MACOS_SHIM_MAP")))
        (when map
          (with-open-file (o map :direction :output :if-exists :supersede)
            (dolist (f (modus.macho::shim-object-functions obj))
              (format o "~(~16,'0X~) ~A~%"
                      (modus.macho::label-vma obj layout f) (modus.macho::mangle f))))))
      (format t "~&build-macos-cli: ~A, ~D bytes (host code ~D bytes, ~D imports, ~D segments, ~
                 signature ~D bytes); verified~%"
              out (length bytes) (* 4 (length (modus.macho::shim-object-text obj)))
              (length (modus.macho::shim-object-imports obj)) nseg
              (modus.macho::layout-sig-size layout)))))
