;;;; 68k-gc-static.lisp -- the static heap's runtime half (translate-68k.lisp
;;;; *68K-GC-STATIC*): post the freeze, and build the card tables the write
;;;; barrier and the card walk use.  Image source, read by mvm/build-68k-cli.lisp
;;;; (hosted) and mvm/build-cl-repl-common.lisp (the bare Mac); GRAN-1,
;;;; MINUS-GRAN, MAX-SHIFT and MINUS-KSHIFT are the heap granule's constants,
;;;; filled in by the build (cl-user::%68k-granule-subst / its Mac twin).

(defun %gc-static-freeze ()
  "Post the collector's freeze request (translate-68k.lisp *68K-GC-STATIC*)
   and allocate until a collection has happened: what boot left live is then
   static.  Called once, between boot and the toplevel."
  (setf (mem-ref #x10006000 :u32) 1)
  (let ((before (mem-ref #x10000060 :u32)) (junk nil))
    (loop
      (unless (eql (mem-ref #x10000060 :u32) before) (return nil))
      (setq junk (make-list 32))))
  (%gc-cards-init))

(defun %gc-cards-init ()
  "The static heap's CARDS (translate-68k.lisp M68K-EMIT-GC-CARD-WALK): the
   smallest card (512 bytes up, at most 2 KB) that fits the span in the mark
   table at #x10006100, the crossing table at #x1001E000 -- each card's first
   object start, in 16-byte granules -- and, LAST, the span, which arms the
   barrier and the card walk.  Reads headers a byte at a time: no bignum."
  (let* ((olo (mem-ref #x10006004 :u32)) (ohi (mem-ref #x10006008 :u32))
         (chi (mem-ref #x10006010 :u32))
         (lo (logand olo -512))
         (shift 9) (ncards 0) (a olo))
    (loop
      (setq ncards (ash (+ (- chi lo) (ash 1 shift) -1) (- shift)))
      (when (or (<= ncards 7680) (>= shift MAX-SHIFT)) (return nil))
      (setq shift (+ shift 1)))
    (when (<= ncards 7680)
      (dotimes (i ncards)
        (setf (mem-ref (+ #x1001E000 i) :u8) 255)
        (setf (mem-ref (+ #x10006100 i) :u8) 0))
      (loop
        (when (>= a ohi) (return nil))
        (let* ((st (mem-ref (+ a 3) :u8))
               (n (+ (* (mem-ref a :u8) 65536) (* (mem-ref (+ a 1) :u8) 256)
                     (mem-ref (+ a 2) :u8)))
               (size (logand (+ (if (or (= st 17) (= st 19)) (+ n 4) (* (+ n 1) 4)) GRAN-1) MINUS-GRAN))
               (c (ash (- a lo) (- shift))))
          (when (= (mem-ref (+ #x1001E000 c) :u8) 255)
            (setf (mem-ref (+ #x1001E000 c) :u8)
                  (ash (- (- a lo) (ash c shift)) MINUS-KSHIFT)))
          (setq a (+ a size))))
      (setf (mem-ref #x10006018 :u32) lo)
      (setf (mem-ref #x10006020 :u32) shift)
      (setf (mem-ref #x1000601C :u32) (ash ncards shift)))))
