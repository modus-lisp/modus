;;;; unlzma-core.lisp -- a raw LZMA1 decoder (LzmaSpec.cpp) in fixnum-only Lisp
;;;; with no runtime: the part the 68k floppy loaders share.
;;;;
;;;; A loader is one head file plus this one (`mvm/build-hosted.lisp ARCH HEAD
;;;; mvm/unlzma-core.lisp`).  The head defines where things are -- +OUT+ the
;;;; output, +ST+ the coder state (RH RL CH CL IPH IPL, u16 each), +PR+ the
;;;; 7991 u16 probabilities -- and KERNEL-MAIN, which calls LZ-INIT on the
;;;; stream and LZ-DECODE with the unpacked size.  Heads: mvm/68k-unlzma.lisp
;;;; (Linux/m68k, an ELF), mvm/mac68k-unlzma.lisp (a bare Mac, the boot disk).
;;;;
;;;; Everything is a fixnum: 68k fixnums are 30 bits and there is no runtime to
;;;; promote to, so the range coder's 32-bit RANGE and CODE are kept as 16-bit
;;;; halves (RH RL CH CL), and the arithmetic is the raw %FIXNUM ops on
;;;; declared fixnums -- no overflow checks, no generic fallbacks to link.
;;;; The algorithm is LzmaSpec.cpp's (LZMA SDK).
;; Probability blocks, in u16 units from +PR+.
(defconstant +is-match+ 0)
(defconstant +is-rep+ 192)
(defconstant +is-rep-g0+ 204)
(defconstant +is-rep-g1+ 216)
(defconstant +is-rep-g2+ 228)
(defconstant +is-rep0-long+ 240)
(defconstant +pos-slot+ 432)
(defconstant +spec-pos+ 688)
(defconstant +align+ 803)
(defconstant +len-dec+ 819)
(defconstant +rep-len-dec+ 1333)
(defconstant +literal+ 1847)
(defconstant +n-probs+ 7991)

(defmacro f+ (a b) `(%fixnum-+ ,a ,b))
(defmacro f- (a b) `(%fixnum-- ,a ,b))
(defmacro f* (a b) `(%fixnum-* ,a ,b))
(defmacro u8 (a) `(the fixnum (mem-ref ,a :u8)))
(defmacro u16 (a) `(the fixnum (mem-ref ,a :u16)))
(defmacro set8 (a v) `(setf (mem-ref ,a :u8) ,v))
(defmacro set16 (a v) `(setf (mem-ref ,a :u16) ,v))
(defmacro pa (i) `(f+ +pr+ (f* 2 ,i)))

(defun lz-next-byte ()
  (let* ((p (f+ (f* (u16 (f+ +st+ 8)) 65536) (u16 (f+ +st+ 10))))
         (q (f+ p 1)))
    (declare (fixnum p q))
    (set16 (f+ +st+ 8) (ash q -16))
    (set16 (f+ +st+ 10) (logand q #xFFFF))
    (u8 p)))

(defun rc-norm ()
  ;; range < 2^24: shift a byte into RANGE and CODE.
  (let ((rh (u16 +st+)))
    (declare (fixnum rh))
    (when (< rh 256)
      (let ((rl (u16 (f+ +st+ 2))) (ch (u16 (f+ +st+ 4))) (cl (u16 (f+ +st+ 6)))
            (b (the fixnum (lz-next-byte))))
        (declare (fixnum rl ch cl b))
        (set16 +st+ (logior (f* rh 256) (ash rl -8)))
        (set16 (f+ +st+ 2) (f* (logand rl 255) 256))
        (set16 (f+ +st+ 4) (logior (f* (logand ch 255) 256) (ash cl -8)))
        (set16 (f+ +st+ 6) (logior (f* (logand cl 255) 256) b))))))

(defun rc-bit (a)
  "Decode one bit with the probability at address A."
  (declare (fixnum a))
  (let* ((p (u16 a)) (rh (u16 +st+)) (rl (u16 (f+ +st+ 2)))
         (ch (u16 (f+ +st+ 4))) (cl (u16 (f+ +st+ 6)))
         ;; bound = (range >> 11) * p, as halves: range >> 11 = hi*2^16 + lo
         (hi (ash rh -11))
         (lo (logior (f* (logand rh #x7FF) 32) (ash rl -11)))
         (lp (f* lo p))
         (bh (f+ (f* hi p) (ash lp -16)))
         (bl (logand lp #xFFFF)))
    (declare (fixnum p rh rl ch cl hi lo lp bh bl))
    (if (or (< ch bh) (and (= ch bh) (< cl bl)))
        (progn
          (set16 +st+ bh) (set16 (f+ +st+ 2) bl)
          (set16 a (f+ p (ash (f- 2048 p) -5)))
          (rc-norm)
          0)
        (let ((nrl (f- rl bl)) (nrh (f- rh bh)) (ncl (f- cl bl)) (nch (f- ch bh)))
          (declare (fixnum nrl nrh ncl nch))
          (when (< nrl 0) (setq nrl (f+ nrl 65536)) (setq nrh (f- nrh 1)))
          (when (< ncl 0) (setq ncl (f+ ncl 65536)) (setq nch (f- nch 1)))
          (set16 +st+ nrh) (set16 (f+ +st+ 2) nrl)
          (set16 (f+ +st+ 4) nch) (set16 (f+ +st+ 6) ncl)
          (set16 a (f- p (ash p -5)))
          (rc-norm)
          1))))

(defun rc-direct ()
  "One direct (equiprobable) bit: RANGE >>= 1; CODE >= RANGE gives 1."
  (let* ((rh (u16 +st+)) (rl (u16 (f+ +st+ 2)))
         (nrl (logior (ash rl -1) (f* (logand rh 1) 32768))) (nrh (ash rh -1))
         (ch (u16 (f+ +st+ 4))) (cl (u16 (f+ +st+ 6))))
    (declare (fixnum rh rl nrl nrh ch cl))
    (set16 +st+ nrh) (set16 (f+ +st+ 2) nrl)
    (if (or (> ch nrh) (and (= ch nrh) (>= cl nrl)))
        (let ((ncl (f- cl nrl)) (nch (f- ch nrh)))
          (declare (fixnum ncl nch))
          (when (< ncl 0) (setq ncl (f+ ncl 65536)) (setq nch (f- nch 1)))
          (set16 (f+ +st+ 4) nch) (set16 (f+ +st+ 6) ncl)
          (rc-norm)
          1)
        (progn (rc-norm) 0))))

(defun bittree (base nbits)
  "BASE is a u16 index; NBITS bits, most significant first."
  (declare (fixnum base nbits))
  (let ((m 1) (i 0) (top 1))
    (declare (fixnum m i top))
    (loop
      (when (>= i nbits) (return (f- m top)))
      (setq m (f+ (f* m 2) (the fixnum (rc-bit (pa (f+ base m))))))
      (setq top (f* top 2))
      (setq i (f+ i 1)))))

(defun bittree-rev (base nbits)
  "NBITS bits, least significant first."
  (declare (fixnum base nbits))
  (let ((m 1) (i 0) (sym 0) (w 1))
    (declare (fixnum m i sym w))
    (loop
      (when (>= i nbits) (return sym))
      (let ((b (rc-bit (pa (f+ base m)))))
        (declare (fixnum b))
        (setq m (f+ (f* m 2) b))
        (when (= b 1) (setq sym (f+ sym w))))
      (setq w (f* w 2))
      (setq i (f+ i 1)))))

(defun len-decode (base pos-state)
  (declare (fixnum base pos-state))
  (if (= 0 (the fixnum (rc-bit (pa base))))
      (bittree (f+ (f+ base 2) (f* pos-state 8)) 3)
      (if (= 0 (the fixnum (rc-bit (pa (f+ base 1)))))
          (f+ 8 (the fixnum (bittree (f+ (f+ base 130) (f* pos-state 8)) 3)))
          (f+ 16 (the fixnum (bittree (f+ base 258) 8))))))

(defun dist-decode (len)
  (declare (fixnum len))
  (let* ((ls (if (> len 3) 3 len))
         (slot (the fixnum (bittree (f+ +pos-slot+ (f* ls 64)) 6))))
    (declare (fixnum ls slot))
    (if (< slot 4)
        slot
        (let ((nd (f- (ash slot -1) 1)) (d (f+ 2 (logand slot 1))) (k 0))
          (declare (fixnum nd d k))
          (loop                         ; d = (2 | slot&1) << nd
            (when (>= k nd) (return nil))
            (setq d (f* d 2))
            (setq k (f+ k 1)))
          (if (< slot 14)
              (f+ d (the fixnum (bittree-rev (f- (f+ +spec-pos+ d) slot) nd)))
              (let ((j (f- nd 4)) (x 0))
                (declare (fixnum j x))
                (loop
                  (when (<= j 0) (return nil))
                  (setq x (f+ (f* x 2) (the fixnum (rc-direct))))
                  (setq j (f- j 1)))
                (f+ (f+ d (f* x 16)) (the fixnum (bittree-rev +align+ 4)))))))))

(defun lz-init (stream)
  (declare (fixnum stream))
  (let ((i 0))
    (declare (fixnum i))
    (loop
      (when (>= i +n-probs+) (return nil))
      (set16 (pa i) 1024)
      (setq i (f+ i 1))))
  (set16 (f+ +st+ 8) (ash stream -16))
  (set16 (f+ +st+ 10) (logand stream #xFFFF))
  (set16 +st+ #xFFFF) (set16 (f+ +st+ 2) #xFFFF)
  (lz-next-byte)                        ; the stream's first byte is 0
  (let* ((a (the fixnum (lz-next-byte))) (b (the fixnum (lz-next-byte)))
         (c (the fixnum (lz-next-byte))) (d (the fixnum (lz-next-byte))))
    (declare (fixnum a b c d))
    (set16 (f+ +st+ 4) (logior (f* a 256) b))
    (set16 (f+ +st+ 6) (logior (f* c 256) d))))

(defun lz-literal (pos state rep0)
  (declare (fixnum pos state rep0))
  (let* ((prev (if (= pos 0) 0 (u8 (f- (f+ +out+ pos) 1))))
         (ls (f+ (f* (logand pos 1) 4) (ash prev -6)))
         (base (f+ +literal+ (f* ls #x300)))
         (sym 1))
    (declare (fixnum prev ls base sym))
    (when (>= state 7)
      (let ((mb (u8 (f- (f- (f+ +out+ pos) rep0) 1))))
        (declare (fixnum mb))
        (loop
          (when (>= sym 256) (return nil))
          (let* ((mbit (logand (ash mb -7) 1))
                 (b (the fixnum (rc-bit (pa (f+ (f+ base (f* (f+ 1 mbit) 256)) sym))))))
            (declare (fixnum mbit b))
            (setq mb (logand (f* mb 2) 255))
            (setq sym (f+ (f* sym 2) b))
            (when (not (= mbit b)) (return nil))))))
    (loop
      (when (>= sym 256) (return nil))
      (setq sym (f+ (f* sym 2) (the fixnum (rc-bit (pa (f+ base sym)))))))
    (set8 (f+ +out+ pos) (f- sym 256))))

(defun lz-decode (size)
  (declare (fixnum size))
  (let ((pos 0) (state 0) (rep0 0) (rep1 0) (rep2 0) (rep3 0))
    (declare (fixnum pos state rep0 rep1 rep2 rep3))
    (loop
      (when (>= pos size) (return pos))
      (let ((ps (logand pos 3)))
        (declare (fixnum ps))
        (if (= 0 (the fixnum (rc-bit (pa (f+ (f+ +is-match+ (f* state 16)) ps)))))
            (progn
              (lz-literal pos state rep0)
              (setq state (if (< state 4) 0 (if (< state 10) (f- state 3) (f- state 6))))
              (setq pos (f+ pos 1)))
            (let ((len 0) (short 0))
              (declare (fixnum len short))
              (if (= 1 (the fixnum (rc-bit (pa (f+ +is-rep+ state)))))
                  (progn
                    (if (= 0 (the fixnum (rc-bit (pa (f+ +is-rep-g0+ state)))))
                        (when (= 0 (the fixnum (rc-bit (pa (f+ (f+ +is-rep0-long+ (f* state 16)) ps)))))
                          (setq short 1))
                        (let ((dist 0))
                          (declare (fixnum dist))
                          (if (= 0 (the fixnum (rc-bit (pa (f+ +is-rep-g1+ state)))))
                              (setq dist rep1)
                              (progn
                                (if (= 0 (the fixnum (rc-bit (pa (f+ +is-rep-g2+ state)))))
                                    (setq dist rep2)
                                    (progn (setq dist rep3) (setq rep3 rep2)))
                                (setq rep2 rep1)))
                          (setq rep1 rep0)
                          (setq rep0 dist)))
                    (if (= short 1)
                        (progn
                          (setq state (if (< state 7) 9 11))
                          (set8 (f+ +out+ pos) (u8 (f- (f- (f+ +out+ pos) rep0) 1)))
                          (setq pos (f+ pos 1)))
                        (progn
                          (setq len (the fixnum (len-decode +rep-len-dec+ ps)))
                          (setq state (if (< state 7) 8 11)))))
                  (progn
                    (setq rep3 rep2) (setq rep2 rep1) (setq rep1 rep0)
                    (setq len (the fixnum (len-decode +len-dec+ ps)))
                    (setq state (if (< state 7) 7 10))
                    (setq rep0 (the fixnum (dist-decode len)))))
              (when (= short 0)
                (let* ((n (f+ len 2)) (dst (f+ +out+ pos)) (src (f- (f- dst rep0) 1)))
                  (declare (fixnum n dst src))
                  (loop
                    (when (or (<= n 0) (>= pos size)) (return nil))
                    (set8 dst (u8 src))
                    (setq dst (f+ dst 1)) (setq src (f+ src 1))
                    (setq pos (f+ pos 1)) (setq n (f- n 1)))))))))))
