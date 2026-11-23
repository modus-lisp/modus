;;;; ppcmac-glass-payload.lisp -- the boot payload list that runs glass.wm on a PowerPC Mac.
;;;;
;;;;   sbcl --script scripts/ppcmac-glass-payload.lisp OUT.list
;;;;   sbcl --script scripts/ppcmac-disk.lisp IMAGE.elf DISK --payload OUT.list
;;;;
;;;; OUT.list is scripts/ppcmac-disk.lisp's --payload list (NAME<tab>PATH a line):
;;;; glass/wm-core's sources and their dependencies' in the order ASDF loads them
;;;; (measured: the hosted ppc32 image's (asdf:load-system :glass/wm-core) log),
;;;; glass/desk's applications, Liberation Sans regular and bold from scribe,
;;;; then scripts/ppcmac/ppcmac-input.lisp (CUDA/ADB keyboard and mouse) and
;;;; ppcmac-desk.lisp (the desk on the screen), which the image evaluates in turn
;;;; before its REPL -- natively, with MODUS_PPC_JIT=1.  Each repository is found
;;;; as scripts/mac68k-tools.lisp finds fs-apple: $NAME (GLASS, SCRIBE, CRAM,
;;;; BROTLI_PURE, CL_TRANSPORT), else a sibling checkout.

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(defpackage #:ppcmac-glass-payload (:use #:cl #:mac68k-tools))
(in-package #:ppcmac-glass-payload)

(defparameter *files*
  '(
    "glass/src/packages.lisp"
    "glass/src/wordlist.lisp"
    "glass/src/record.lisp"
    "glass/src/framebuffer.lisp"
    "glass/src/clipboard.lisp"
    "cl-transport/src/listeners.lisp"
    "cram/src/packages.lisp"
    "cram/src/deflate.lisp"
    "cram/src/inflate.lisp"
    "cram/src/inflate-stream.lisp"
    "cram/src/prefix.lisp"
    "cram/src/lzw.lisp"
    "cram/src/tar.lisp"
    "cram/src/lzma.lisp"
    "cram/src/crc16.lisp"
    "cram/src/adc.lisp"
    "glass/src/perf.lisp"
    "glass/src/transports.lisp"
    "glass/src/rfb.lisp"
    "glass/src/zrle.lisp"
    "brotli-pure/src/packages.lisp"
    "brotli-pure/src/bitio.lisp"
    "brotli-pure/src/tables.lisp"
    "brotli-pure/src/huffman.lisp"
    "brotli-pure/src/dict.lisp"
    "brotli-pure/src/decompress.lisp"
    "brotli-pure/src/brotli.lisp"
    "brotli-pure/src/encode.lisp"
    "scribe/src/packages.lisp"
    "scribe/src/blend.lisp"
    "scribe/src/font.lisp"
    "scribe/src/woff.lisp"
    "scribe/src/tables/hmtx.lisp"
    "scribe/src/tables/os_2.lisp"
    "scribe/src/tables/post.lisp"
    "scribe/src/tables/name.lisp"
    "scribe/src/cmap.lisp"
    "scribe/src/tables/cmap-0.lisp"
    "scribe/src/tables/cmap-4.lisp"
    "scribe/src/tables/cmap-12.lisp"
    "scribe/src/cff.lisp"
    "scribe/src/var.lisp"
    "scribe/src/glyf.lisp"
    "scribe/src/raster.lisp"
    "scribe/src/shape.lisp"
    "scribe/src/otl.lisp"
    "scribe/src/hint.lisp"
    "scribe/src/match.lisp"
    "scribe/src/color.lisp"
    "scribe/src/threads.lisp"
    "glass/src/text.lisp"
    "glass/src/desk.lisp"
    "glass/src/desk-apps.lisp"
    "glass/src/media-radio.lisp"
    "glass/src/wm/package.lisp"
    "glass/src/wm/chrome.lisp"
    "glass/src/wm/wm.lisp"
    "glass/src/wm/rack.lisp")
  "REPO/PATH, in load order.")

(defparameter *repos*
  '(("glass" . "GLASS") ("scribe" . "SCRIBE") ("cram" . "CRAM")
    ("brotli-pure" . "BROTLI_PURE") ("cl-transport" . "CL_TRANSPORT")))

(defun repo-path (rel)
  (let* ((slash (position #\/ rel))
         (repo (subseq rel 0 slash)))
    (namestring (merge-pathnames (subseq rel (1+ slash))
                                 (mac68k-tools::sibling-dir repo (cdr (assoc repo *repos* :test #'string=)))))))

(let ((out (first (argv))))
  (with-open-file (o out :direction :output :if-exists :supersede)
    (dolist (f *files*)
      (format o "~A~C~A~%" f #\Tab (repo-path f)))
    (dolist (font '("LiberationSans-Regular.ttf" "LiberationSans-Bold.ttf"))
      (format o "~A~C~A~%" font #\Tab (repo-path (concatenate 'string "scribe/fonts/" font))))
    (dolist (f '("ppcmac-input.lisp" "ppcmac-desk.lisp"))
      (format o "~A~C~A~%" f #\Tab (namestring (merge-pathnames (concatenate 'string "ppcmac/" f) *scripts*)))))
  (format t "ppcmac-glass-payload: ~A: ~D entries~%" out (+ (length *files*) 4)))
