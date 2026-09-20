;;;; glass-load.lisp — THE EIGHT FILES OF THE `:glass` SYSTEM, LOADED ON MODUS.
;;;;
;;;;   ./modus --script test/glass-load.lisp            (but really: test/run-glass-load.sh,
;;;;                                                     which supplies the paths and the SBCL oracle)
;;;;
;;;; ============================================================
;;;; WHAT THIS IS AND WHY IT IS A FILE IN THE TREE
;;;; ============================================================
;;;;
;;;; "all eight files of :glass load" was a headline with no test behind it.  A
;;;; claim nobody else can re-run is not a result, it is a memory.  This is the
;;;; claim, executable.
;;;;
;;;; It loads exactly the component list `glass.asd' declares for the system
;;;; `:glass' — cram's five files first, because `:glass' depends on `cram',
;;;; then glass/fb's three, glass/clipboard's one, and :glass's own four — and
;;;; then asks the image whether the things that load was supposed to define
;;;; are actually there.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE, and what stops it
;;;; ============================================================
;;;;
;;;;   "IT LOADED" IS NOT "LOAD DID NOT SIGNAL".  modus's LOAD swallows a
;;;;   toplevel form that dies (the documented `load-toplevel-form-swallowed'
;;;;   escape), so a file can "load" while defining nothing.  So the test is not
;;;;   that LOAD returned: it is that a NAMED FUNCTION FROM EACH FILE IS FBOUND
;;;;   afterwards, one per file, chosen to be defined near the END of that file
;;;;   so a form swallowed in the middle is still caught.
;;;;
;;;;   THE FILE LIST IS NOT WRITTEN OUT BY HAND HERE.  It is passed in by the
;;;;   runner, which reads it out of `glass.asd' and `cram.asd' with a parser
;;;;   that has no glass knowledge in it, so a component added to those systems
;;;;   appears here rather than being silently not tested.
;;;;
;;;;   THE COUNT IS EXACT AND PRINTED.  `8 of 8' with the eight names, not `ok'.
;;;;
;;;; glass and cram are READ-ONLY here: this loads their sources where they sit
;;;; and writes nothing into either tree.

;;; THE ORACLE HAS TO BE GIVEN WHAT MODUS IS GIVEN.  `src/socket.lisp' names
;;; SB-BSD-SOCKETS and SB-POSIX at READ time, and a bare `sbcl --script' has
;;; neither module loaded, so the oracle would fail to READ a file modus reads
;;; fine — and would report it as glass being broken.  On modus these packages
;;; come from the baked shim and there is nothing to require, which is the whole
;;; point of the comparison.
#+sbcl (progn (require :sb-bsd-sockets) (require :sb-posix))

(defvar *fail* 0)
(defvar *checks* 0)

;;; THE SAME PROGRAM HAS TO RUN UNDER BOTH, so the one thing that is genuinely
;;; not portable — leaving with a status — is spelled once, here.  modus is not
;;; SBCL and does not claim to be (it pushes :SB-THREAD and :SB-BSD-SOCKETS and
;;; NOT :SBCL), so #+sbcl is a real discriminator and not a guess.
(defun bail (code)
  (finish-output)
  #+sbcl (sb-ext:exit :code code)
  #-sbcl (sys-exit code))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

;;; *GLASS-FILES* — a list of (absolute-path package-name witness-function-name)
;;; — is bound by the MANIFEST the runner generates from the .asd files and
;;; loads BEFORE this one.  It is not written out here, so that a component
;;; added to :glass or to cram shows up as an untested file rather than as
;;; nothing at all.  Running this file directly, with no manifest, says so.
(unless (boundp '*glass-files*)
  (format t "~&test/glass-load.lisp needs a manifest; run test/run-glass-load.sh~%")
  (bail 2))

(defun witness-present-p (pkgname symname)
  "Did the file that was just loaded actually define its witness?

   SYMNAME NIL means the file is a packages.lisp and defines no function at all;
   the witness is then that PKGNAME EXISTS.  Otherwise the witness is that
   SYMNAME is fbound in it.  A missing package answers NIL either way, which is
   the right answer when the file that was to create it died."
  (let ((p (find-package pkgname)))
    (cond ((null p) nil)
          ((null symname) t)
          (t (let ((s (find-symbol symname p)))
               (if (null s) nil (and (fboundp s) t)))))))

(let ((loaded 0)
      (n (length *glass-files*)))
  (format t "~&=== the ~d files of :glass (and its cram dependency), on modus ===~%" n)
  (dolist (entry *glass-files*)
    (let ((path (first entry))
          (pkg (second entry))
          (witness (third entry)))
      (format t "~&>> ~a~%" path)
      (force-output)
      (load path)
      (let ((ok (witness-present-p pkg witness)))
        (when ok (setq loaded (+ loaded 1)))
        (chk (if witness
                 (format nil "~a defines ~a::~a" path pkg witness)
                 (format nil "~a creates the ~a package" path pkg))
             ok t))))
  (format t "~&~%LOADED ~d of ~d~%" loaded n))

;;; The system's own entry points, by name.  A file can define its witness and
;;; still have lost the form that matters; these are the four names the campaign
;;; is actually about.
(chk "GLASS:SERVE fbound"       (and (fboundp (find-symbol "SERVE" "GLASS")) t) t)
(chk "GLASS:SERVE-ONE fbound"   (and (fboundp (find-symbol "SERVE-ONE" "GLASS")) t) t)
(chk "GLASS:MAKE-FRAMEBUFFER fbound" (and (fboundp (find-symbol "MAKE-FRAMEBUFFER" "GLASS")) t) t)
(chk "GLASS:TCP-LISTEN fbound"  (and (fboundp (find-symbol "TCP-LISTEN" "GLASS")) t) t)

;;; And it has to be able to MAKE one, not merely name the constructor: a
;;; framebuffer is the thing every later rung draws into.
(let* ((make-fb (find-symbol "MAKE-FRAMEBUFFER" "GLASS"))
       (fb (funcall make-fb 64 48))
       (w (funcall (find-symbol "FB-WIDTH" "GLASS") fb))
       (h (funcall (find-symbol "FB-HEIGHT" "GLASS") fb)))
  (chk "make-fb 64x48 -> (w h)" (list w h) (list 64 48)))

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (zerop *fail*)
    (format t "PASS~%")
    (format t "FAIL~%"))
(bail (if (zerop *fail*) 0 1))
