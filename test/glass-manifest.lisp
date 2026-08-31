;;;; glass-manifest.lisp — WHICH FILES THE `:glass` SYSTEM IS, READ OUT OF THE .asd.
;;;;
;;;;   sbcl --script test/glass-manifest.lisp GLASS-DIR/ CRAM-DIR/ OUT.lisp
;;;;
;;;; Writes a `(defvar *glass-files* '((path package witness) ...))' form in
;;;; load order, for a modus-side test to load ahead of itself.
;;;;
;;;; IT IS A SEPARATE FILE BECAUSE SEVERAL RUNNERS WANT IT — loading :glass is
;;;; the preamble to every later rung (serving a client, serving two) and a
;;;; second copy of the list is a second thing to forget to update.
;;;;
;;;; THE LIST IS NOT WRITTEN OUT HERE.  It is read from `glass.asd' and
;;;; `cram.asd' with the real reader, so a component added to either system
;;;; appears in every runner at once.  What IS written out here is the WITNESS
;;;; for each file — the last thing that file defines — because a .asd does not
;;;; record what a file defines, and "LOAD did not signal" is not "it loaded"
;;;; on an implementation whose LOAD swallows a form that dies.
;;;;
;;;; A system whose file count and witness count disagree is a HARD ERROR here,
;;;; not a silently shorter test.

;; ASDF is not present in `sbcl --script' by default, and `glass.asd' spells its
;; systems `asdf:defsystem' — so the package has to exist before the file is
;; READ.  Nothing is loaded through ASDF here; only its package is wanted.
(require :asdf)

(defun asd-system-files (asd system)
  "The SRC-relative file names the ASDF system SYSTEM declares, in order.
   Reads the .asd with the real reader and walks the component tree; no
   assumption that the components are on one line or in any particular shape."
  (let ((forms '()))
    (with-open-file (in asd :external-format :utf-8)
      (let ((*package* (or (find-package :asdf-user) *package*)))
        (loop (let ((f (handler-case (read in nil :eof) (error () :eof))))
                (when (eq f :eof) (return))
                (push f forms)))))
    (dolist (f (nreverse forms))
      (when (and (consp f) (eq (first f) 'asdf:defsystem)
                 (string-equal (string (second f)) system))
        (let ((comp (getf (cddr f) :components)))
          (return-from asd-system-files
            (loop for m in comp
                  when (and (consp m) (eq (first m) :module))
                  append (loop for c in (getf (cddr m) :components)
                               when (and (consp c) (eq (first c) :file))
                               collect (format nil "~a/~a.lisp"
                                               (second m) (second c))))))))
    (error "no system ~a in ~a" system asd)))

(let* ((glass (second sb-ext:*posix-argv*))
       (cram  (third  sb-ext:*posix-argv*))
       (out   (fourth sb-ext:*posix-argv*))
       ;; cl-transport sits beside glass; the runner may also pass it as argv 5.
       (ctrans (or (fifth sb-ext:*posix-argv*)
                   (merge-pathnames "../cl-transport/" glass)))
       ;; (system  root  asd  witnesses-in-file-order)
       ;;
       ;; A WITNESS IS THE LAST THING ITS FILE DEFINES, so a toplevel form
       ;; swallowed anywhere in the file still fails the check.  NIL means the
       ;; file defines no function at all (a packages.lisp) and the witness is
       ;; that the PACKAGE now exists.
       (plan `(("cram"            ,cram  ,(merge-pathnames "cram.asd" cram)
                (("CRAM" nil)                  ; packages.lisp
                 ("CRAM" "RESET")              ; deflate.lisp
                 ("CRAM" "GZIP-DECOMPRESS")    ; inflate.lisp
                 ("CRAM" "RECENT-CACHE-REF")   ; prefix.lisp
                 ("CRAM" "LZW-DECODE")))       ; lzw.lisp
               ("glass/fb"        ,glass ,(merge-pathnames "glass.asd" glass)
                (("GLASS" nil)                 ; packages.lisp
                 ("GLASS" "WORD-NAME")         ; wordlist.lisp
                 ("GLASS" "DEFINE-RECORD")     ; record.lisp
                 ("GLASS" "FB-BLIT")))         ; framebuffer.lisp
               ("glass/clipboard" ,glass ,(merge-pathnames "glass.asd" glass)
                (("GLASS" "CLIPBOARD-PASTE"))) ; clipboard.lisp
               ;; cl-transport/listeners, BETWEEN clipboard and :glass.  glass
               ;; stopped owning its sockets (glass 3ac9844) — src/transports.lisp
               ;; IMPORTS ~35 names out of CL-TRANSPORT.LISTENERS and re-exports
               ;; them, and SIGNALS if the package does not have them.  So this
               ;; system must be loaded first or :glass cannot load at all, and
               ;; the witness below is a symbol that only exists because the
               ;; import found it.
               ("cl-transport/listeners" ,ctrans
                ,(merge-pathnames "cl-transport.asd" ctrans)
                (("CL-TRANSPORT.LISTENERS" "SOCKET-UNSENT-BYTES"))) ; listeners.lisp
               ("glass"           ,glass ,(merge-pathnames "glass.asd" glass)
                (("GLASS" "PERF-REPORT")       ; perf.lisp
                 ;; transports.lisp defines no function of its own — its whole
                 ;; body is the IMPORT above, so the witness is that a borrowed
                 ;; name is now a GLASS symbol.  Same name as the line above on
                 ;; purpose: it must be present in BOTH packages.
                 ("GLASS" "SOCKET-UNSENT-BYTES") ; transports.lisp
                 ("GLASS" "SERVE-ONE")         ; rfb.lisp
                 ("GLASS" "WRITE-RECT-TRLE"))))))  ; zrle.lisp
  (with-open-file (o out :direction :output :if-exists :supersede)
    (format o "(defvar *glass-files* '(~%")
    (dolist (p plan)
      (destructuring-bind (system root asd witnesses) p
        (let ((files (asd-system-files asd system)))
          (unless (= (length files) (length witnesses))
            (format *error-output*
                    "~&HARNESS OUT OF DATE: system ~a declares ~d files, the ~
                     witness list has ~d.  Add a witness (a function defined ~
                     near the end of the new file) rather than dropping the ~
                     check.~%" system (length files) (length witnesses))
            (sb-ext:exit :code 3))
          (loop for f in files for w in witnesses
                do (format o "  (~s ~s ~s)~%"
                           (namestring (merge-pathnames f root))
                           (first w) (second w))))))
    (format o "))~%")))
