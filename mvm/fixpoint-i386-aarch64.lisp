;;; ================================================================
;;; i386-safe AArch64 translator (byte-level emission)
;;; All instruction encodings use byte3/lo24 split to avoid
;;; 30-bit fixnum overflow on i386 (where ash 1 31 = 0).
;;; ================================================================

;;; Pre-generated AArch64 boot preamble (4096 bytes)
;;; Generated at SBCL build time from emit-aarch64-fixpoint-entry
(defvar *a64-boot-preamble-size* 4096)
(defvar *a64-boot-preamble-packed* nil)
(defvar *a64-preamble-vals* '(11012112 139218 37120 13795328 11010080 498 184960 13795392 33823 279288 12767488 11927551 8519680 8402 193192 13795392 33823 279288 12767488 11927551 8388608 8402 6419112 13795840 11010081 498 2226432 13795552 11010049 262642 63744 13795584 11010080 14721522 119424 15908872 1 249 2151040 15902720 8519680 8402 2224808 13795552 11010049 2097906 250496 13795328 10486787 8651250 2226176 9110272 1090 16753361 46591 13795968 11010080 14721522 119424 15901184 8400898 8651218 2226176 9110272 1090 16753361 46591 13796096 11010080 14721522 119424 15900960 1 2089209 53888 13965474 8823584 16594 4256448 13965344 8388608 8402 62120 13965344 211871 4186069 54531 13973520 8519841 210 43521 13965328 212959 4309 1102464 15900676 543 4497 1168000 15901696 8413216 3285202 47360 13795342 11808 106681 2151040 12124198 8388640 2760914 2144512 13795424 12832 6329 1626752 15900960 8388633 6610 1766050 13795328 8388624 446674 9499296 13965520 8454160 12587218 14669080 13959999 928 2105108 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 54531 1310720 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 213 2036736 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 0 2105108 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 54531 1310720 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 213 2036736 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 12519392 8361 119457 12140556 4097 516281 14723265 14065411 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 54531 1310720 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 213 2036736 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 0 2105108 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 54531 1310720 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 213 2036736 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 0 2105108 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 54531 1310720 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 213 2036736 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 0 2105108 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 54531 1310720 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 2105301 2086147 13959968 204831 213 ))
(defun a64i-init-boot-preamble ()
  (let ((p (make-array 1366))
        (vals *a64-preamble-vals*)
        (i 0))
    (loop
      (when (null vals) (return nil))
      (let ((v (car vals)))
        (let ((dummy (aset p i v)))
          dummy))
      (setq vals (cdr vals))
      (setq i (+ i 1)))
    (setq *a64-boot-preamble-packed* p)
    (setq *a64-boot-preamble-size* 4096)))


;;; i386-safe AArch64 code buffer (byte-based)
;;; buf = (bytes . (pos . (labels . fixups)))
(defun make-a64i-buffer ()
  (let ((bytes (make-array 2097152)))
    (cons bytes (cons 0 (cons (make-hash-table) nil)))))

(defun a64i-buf-bytes (buf) (car buf))
(defun a64i-buf-pos (buf) (car (cdr buf)))
(defun a64i-buf-labels (buf) (car (cdr (cdr buf))))
(defun a64i-buf-fixups (buf) (cdr (cdr (cdr buf))))
(defun a64i-set-pos (buf p) (set-car (cdr buf) p))
(defun a64i-set-fixups (buf f) (set-cdr (cdr (cdr buf)) f))

(defun a64i-emit (buf b3 lo24)
  (let ((bytes (a64i-buf-bytes buf))
        (pos (a64i-buf-pos buf)))
    (aset bytes pos (logand lo24 255))
    (aset bytes (+ pos 1) (logand (ash lo24 -8) 255))
    (aset bytes (+ pos 2) (logand (ash lo24 -16) 255))
    (aset bytes (+ pos 3) b3)
    (a64i-set-pos buf (+ pos 4))))

(defun a64i-emit-raw (buf b0 b1 b2 b3)
  (let ((bytes (a64i-buf-bytes buf))
        (pos (a64i-buf-pos buf)))
    (aset bytes pos b0)
    (aset bytes (+ pos 1) b1)
    (aset bytes (+ pos 2) b2)
    (aset bytes (+ pos 3) b3)
    (a64i-set-pos buf (+ pos 4))))

(defun a64i-current-index (buf) (ash (a64i-buf-pos buf) -2))

(defun a64i-set-label (buf label-id)
  (puthash label-id (a64i-buf-labels buf) (a64i-current-index buf)))

(defun a64i-add-fixup (buf index label-id type)
  (a64i-set-fixups buf (cons (cons index (cons label-id type)) (a64i-buf-fixups buf))))

;;; AArch64 encoder functions (i386-safe byte3/lo24 split)

(defun a64i-add-reg (buf rd rn rm shift amount)
  (a64i-emit buf #x8B (logior (ash shift 22) (ash rm 16) (ash amount 10) (ash rn 5) rd)))

(defun a64i-sub-reg (buf rd rn rm shift amount)
  (a64i-emit buf #xCB (logior (ash shift 22) (ash rm 16) (ash amount 10) (ash rn 5) rd)))

(defun a64i-subs-reg (buf rd rn rm shift amount)
  (a64i-emit buf #xEB (logior (ash shift 22) (ash rm 16) (ash amount 10) (ash rn 5) rd)))

(defun a64i-cmp-reg (buf rn rm)
  (a64i-subs-reg buf 31 rn rm 0 0))

(defun a64i-add-imm (buf rd rn imm12 shift)
  (let ((sh (if (= shift 12) 1 0)))
    (a64i-emit buf #x91 (logior (ash sh 22) (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64i-sub-imm (buf rd rn imm12 shift)
  (let ((sh (if (= shift 12) 1 0)))
    (a64i-emit buf #xD1 (logior (ash sh 22) (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64i-adds-imm (buf rd rn imm12 shift)
  (let ((sh (if (= shift 12) 1 0)))
    (a64i-emit buf #xB1 (logior (ash sh 22) (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64i-subs-imm (buf rd rn imm12 shift)
  (let ((sh (if (= shift 12) 1 0)))
    (a64i-emit buf #xF1 (logior (ash sh 22) (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64i-cmp-imm (buf rn imm12)
  (a64i-subs-imm buf 31 rn imm12 0))

(defun a64i-and-reg (buf rd rn rm)
  (a64i-emit buf #x8A (logior (ash rm 16) (ash rn 5) rd)))

(defun a64i-orr-reg (buf rd rn rm)
  (a64i-emit buf #xAA (logior (ash rm 16) (ash rn 5) rd)))

(defun a64i-eor-reg (buf rd rn rm)
  (a64i-emit buf #xCA (logior (ash rm 16) (ash rn 5) rd)))

(defun a64i-ands-reg (buf rd rn rm)
  (a64i-emit buf #xEA (logior (ash rm 16) (ash rn 5) rd)))

(defun a64i-tst-reg (buf rn rm)
  (a64i-ands-reg buf 31 rn rm))

(defun a64i-mov-reg (buf rd rm)
  (a64i-emit buf #xAA (logior (ash rm 16) (ash 31 5) rd)))

(defun a64i-movz (buf rd imm16 hw)
  (a64i-emit buf #xD2 (logior (ash 1 23) (ash (logand hw 3) 21) (ash (logand imm16 65535) 5) rd)))

(defun a64i-movk (buf rd imm16 hw)
  (a64i-emit buf #xF2 (logior (ash 1 23) (ash (logand hw 3) 21) (ash (logand imm16 65535) 5) rd)))

(defun a64i-movn (buf rd imm16 hw)
  (a64i-emit buf #x92 (logior (ash 1 23) (ash (logand hw 3) 21) (ash (logand imm16 65535) 5) rd)))

(defun a64i-ubfm (buf rd rn immr imms)
  (a64i-emit buf #xD3 (logior (ash 1 22) (ash (logand immr 63) 16) (ash (logand imms 63) 10) (ash rn 5) rd)))

(defun a64i-sbfm (buf rd rn immr imms)
  (a64i-emit buf #x93 (logior (ash 1 22) (ash (logand immr 63) 16) (ash (logand imms 63) 10) (ash rn 5) rd)))

(defun a64i-lsr-imm (buf rd rn amount) (a64i-ubfm buf rd rn amount 63))
(defun a64i-lsl-imm (buf rd rn amount) (a64i-ubfm buf rd rn (logand (- 64 amount) 63) (- 63 amount)))
(defun a64i-asr-imm (buf rd rn amount) (a64i-sbfm buf rd rn amount 63))

(defun a64i-mul (buf rd rn rm)
  (a64i-emit buf #x9B (logior (ash rm 16) (ash 31 10) (ash rn 5) rd)))

(defun a64i-sdiv (buf rd rn rm)
  (a64i-emit buf #x9A (logior (ash 6 21) (ash rm 16) (ash 3 10) (ash rn 5) rd)))

(defun a64i-lslv (buf rd rn rm)
  (a64i-emit buf #x9A (logior (ash 6 21) (ash rm 16) (ash 8 10) (ash rn 5) rd)))

(defun a64i-asrv (buf rd rn rm)
  (a64i-emit buf #x9A (logior (ash 6 21) (ash rm 16) (ash 10 10) (ash rn 5) rd)))

(defun a64i-neg (buf rd rm) (a64i-sub-reg buf rd 31 rm 0 0))

(defun a64i-ldur (buf rt rn simm9)
  (a64i-emit buf #xF8 (logior (ash 1 22) (ash (logand simm9 511) 12) (ash rn 5) rt)))

(defun a64i-stur (buf rt rn simm9)
  (a64i-emit buf #xF8 (logior (ash (logand simm9 511) 12) (ash rn 5) rt)))

(defun a64i-ldr-width (buf rt rn offset width)
  (let ((b3 (cond ((= width 0) #x38) ((= width 1) #x78) ((= width 2) #xB8) (t #xF8))))
    (a64i-emit buf b3 (logior (ash 1 22) (ash (logand offset 511) 12) (ash rn 5) rt))))

(defun a64i-str-width (buf rt rn offset width)
  (let ((b3 (cond ((= width 0) #x38) ((= width 1) #x78) ((= width 2) #xB8) (t #xF8))))
    (a64i-emit buf b3 (logior (ash (logand offset 511) 12) (ash rn 5) rt))))

(defun a64i-ldr-unsigned (buf rt rn imm12)
  (let ((scaled (ash imm12 -3)))
    (a64i-emit buf #xF9 (logior (ash 1 22) (ash (logand scaled 4095) 10) (ash rn 5) rt))))

(defun a64i-str-unsigned (buf rt rn imm12)
  (let ((scaled (ash imm12 -3)))
    (a64i-emit buf #xF9 (logior (ash (logand scaled 4095) 10) (ash rn 5) rt))))

(defun a64i-stp-offset (buf rt1 rt2 rn simm7)
  (let ((scaled (ash simm7 -3)))
    (a64i-emit buf #xA9 (logior (ash (logand scaled 127) 15) (ash rt2 10) (ash rn 5) rt1))))

(defun a64i-ldp-offset (buf rt1 rt2 rn simm7)
  (let ((scaled (ash simm7 -3)))
    (a64i-emit buf #xA9 (logior (ash 1 22) (ash (logand scaled 127) 15) (ash rt2 10) (ash rn 5) rt1))))

(defun a64i-stp-pre (buf rt1 rt2 rn simm7)
  (let ((scaled (ash simm7 -3)))
    (a64i-emit buf #xA9 (logior (ash 1 23) (ash (logand scaled 127) 15) (ash rt2 10) (ash rn 5) rt1))))

(defun a64i-ldp-post (buf rt1 rt2 rn simm7)
  (let ((scaled (ash simm7 -3)))
    (a64i-emit buf #xA8 (logior (ash 3 22) (ash (logand scaled 127) 15) (ash rt2 10) (ash rn 5) rt1))))

(defun a64i-str-pre (buf rt rn simm9)
  (a64i-emit buf #xF8 (logior (ash (logand simm9 511) 12) (ash 3 10) (ash rn 5) rt)))

(defun a64i-ldr-post (buf rt rn simm9)
  (a64i-emit buf #xF8 (logior (ash 1 22) (ash (logand simm9 511) 12) (ash 1 10) (ash rn 5) rt)))

(defun a64i-b (buf imm26)
  (let ((masked (logand imm26 67108863)))
    (a64i-emit buf (logior 20 (logand (ash masked -24) 3))
                (logand masked 16777215))))

(defun a64i-bl (buf imm26)
  (let ((masked (logand imm26 67108863)))
    (a64i-emit buf (logior 148 (logand (ash masked -24) 3))
                (logand masked 16777215))))

(defun a64i-bcond (buf cond imm19)
  (a64i-emit buf #x54 (logior (ash (logand imm19 524287) 5) (logand cond 15))))

(defun a64i-ret (buf rn) (a64i-emit buf #xD6 (logior (ash 95 16) (ash rn 5))))
(defun a64i-br (buf rn) (a64i-emit buf #xD6 (logior (ash 31 16) (ash rn 5))))
(defun a64i-blr (buf rn) (a64i-emit buf #xD6 (logior (ash 63 16) (ash rn 5))))

(defun a64i-nop (buf) (a64i-emit buf #xD5 #x03201F))
(defun a64i-sev (buf) (a64i-emit buf #xD5 #x03209F))
(defun a64i-wfe (buf) (a64i-emit buf #xD5 #x03205F))
(defun a64i-wfi (buf) (a64i-emit buf #xD5 #x03207F))
(defun a64i-isb (buf) (a64i-emit buf #xD5 #x033FDF))

(defun a64i-brk (buf imm16) (a64i-emit buf #xD4 (logior (ash 1 21) (ash (logand imm16 65535) 5))))
(defun a64i-svc (buf imm16) (a64i-emit buf #xD4 (logior 1 (ash (logand imm16 65535) 5))))

(defun a64i-dmb (buf option)
  (a64i-emit buf #xD5 (logior (ash 3 16) (ash 3 12) (ash (logand option 15) 8) (ash 5 5) 31)))

(defun a64i-dsb (buf option)
  (a64i-emit buf #xD5 (logior (ash 3 16) (ash 3 12) (ash (logand option 15) 8) (ash 4 5) 31)))

(defun a64i-mrs (buf rt sysreg) (a64i-emit buf #xD5 (logior (ash 3 20) (ash sysreg 5) rt)))
(defun a64i-msr-sysreg (buf sysreg rt) (a64i-emit buf #xD5 (logior (ash 1 20) (ash sysreg 5) rt)))

(defun a64i-msr-daifset (buf imm4) (a64i-emit buf #xD5 (logior (ash 3 16) (ash 4 12) (ash (logand imm4 15) 8) (ash 6 5) 31)))
(defun a64i-msr-daifclr (buf imm4) (a64i-emit buf #xD5 (logior (ash 3 16) (ash 4 12) (ash (logand imm4 15) 8) (ash 7 5) 31)))

(defun a64i-ldxr (buf rt rn) (a64i-emit buf #xC8 (logior (ash 95 16) (ash 31 10) (ash rn 5) rt)))
(defun a64i-stxr (buf rs rt rn) (a64i-emit buf #xC8 (logior (ash rs 16) (ash 31 10) (ash rn 5) rt)))

(defun a64i-cset (buf rd cond)
  (let ((inv (logxor cond 1)))
    (a64i-emit buf #x9A (logior (ash 1 23) (ash 31 16) (ash inv 12) (ash 1 10) (ash 31 5) rd))))

(defun a64i-cbnz-w (buf rt offset19)
  (a64i-emit buf #x35 (logior (ash (logand offset19 524287) 5) rt)))

(defun a64i-tbnz (buf rt bit-num imm14)
  (a64i-emit buf #x37 (logior (ash (logand bit-num 31) 19) (ash (logand imm14 16383) 5) rt)))

(defun a64i-cbnz (buf rt offset19)
  (a64i-emit buf #xB5 (logior (ash (logand offset19 524287) 5) rt)))

;;; Load 64-bit immediate from bytecodes (reads raw 16-bit half-words)
;;; Must match a64-load-imm64 optimization exactly:
;;;   1. Zero -> MOVZ rd, 0
;;;   2. Single non-zero halfword -> MOVZ at that position
;;;   3. MOVN: if inverted value has single non-zero halfword
;;;   4. General: MOVZ first non-zero, MOVK rest
(defun a64i-load-imm64-raw (buf rd bytecode pos)
  (let ((hw0 (+ (aref bytecode pos) (ash (aref bytecode (+ pos 1)) 8))))
    (let ((hw1 (+ (aref bytecode (+ pos 2)) (ash (aref bytecode (+ pos 3)) 8))))
      (let ((hw2 (+ (aref bytecode (+ pos 4)) (ash (aref bytecode (+ pos 5)) 8))))
        (let ((hw3 (+ (aref bytecode (+ pos 6)) (ash (aref bytecode (+ pos 7)) 8))))
          ;; Count non-zero halfwords
          (let ((nz (+ (if (zerop hw0) 0 1)
                       (+ (if (zerop hw1) 0 1)
                          (+ (if (zerop hw2) 0 1)
                             (if (zerop hw3) 0 1))))))
            (cond
              ;; All zero
              ((= nz 0) (a64i-movz buf rd 0 0))
              ;; Single non-zero halfword
              ((= nz 1)
               (cond
                 ((not (zerop hw0)) (a64i-movz buf rd hw0 0))
                 ((not (zerop hw1)) (a64i-movz buf rd hw1 1))
                 ((not (zerop hw2)) (a64i-movz buf rd hw2 2))
                 (t (a64i-movz buf rd hw3 3))))
              ;; General: MOVZ first non-zero, MOVK rest (no MOVN - must match bare-metal a64-load-imm64)
              (t
               (cond
                 ((not (zerop hw0))
                  (a64i-movz buf rd hw0 0)
                  (when (not (zerop hw1)) (a64i-movk buf rd hw1 1))
                  (when (not (zerop hw2)) (a64i-movk buf rd hw2 2))
                  (when (not (zerop hw3)) (a64i-movk buf rd hw3 3)))
                 ((not (zerop hw1))
                  (a64i-movz buf rd hw1 1)
                  (when (not (zerop hw2)) (a64i-movk buf rd hw2 2))
                  (when (not (zerop hw3)) (a64i-movk buf rd hw3 3)))
                 ((not (zerop hw2))
                  (a64i-movz buf rd hw2 2)
                  (when (not (zerop hw3)) (a64i-movk buf rd hw3 3)))
                 (t
                  (a64i-movz buf rd hw3 3)))))))))))

(defun a64i-load-imm64 (buf rd value)
  ;; Optimized for 32-bit values (up to 2 halfwords on i386)
  (let ((hw0 (logand value 65535)))
    (let ((hw1 (logand (ash value -16) 65535)))
      (let ((nz (+ (if (zerop hw0) 0 1) (if (zerop hw1) 0 1))))
        (cond
          ((= nz 0) (a64i-movz buf rd 0 0))
          ((= nz 1)
           (if (not (zerop hw0)) (a64i-movz buf rd hw0 0) (a64i-movz buf rd hw1 1)))
          (t (a64i-movz buf rd hw0 0) (a64i-movk buf rd hw1 1)))))))

;;; Spill slot helpers
(defun a64i-spill-offset (vreg) (+ -8 (* (- vreg 9) -8)))

;;; On bare-metal x64/aarch64, *a64-vreg-to-phys* slots 9-15 contain fixnum 0
;;; (not nil) because make-array ignores :initial-element on bare metal.
;;; So a64-phys-reg(V9) returns 0 (X0, truthy) and the translator treats V9-V15
;;; as mapped to X0 without spilling. To match, never report any VR as spilled.
(defun a64i-vreg-spilled-p (vreg) nil)

(defun a64i-emit-load-vreg (buf phys-dest vreg)
  (if (a64i-vreg-spilled-p vreg)
      (a64i-ldur buf phys-dest 29 (a64i-spill-offset vreg))
      (let ((phys (aref *a64-vreg-to-phys* vreg)))
        (when (not (= phys phys-dest)) (a64i-mov-reg buf phys-dest phys)))))

(defun a64i-emit-store-vreg (buf phys-src vreg)
  (if (a64i-vreg-spilled-p vreg)
      (a64i-stur buf phys-src 29 (a64i-spill-offset vreg))
      (let ((phys (aref *a64-vreg-to-phys* vreg)))
        (when (not (= phys phys-src)) (a64i-mov-reg buf phys phys-src)))))

(defun a64i-emit-prologue (buf)
  (a64i-stp-pre buf 29 30 31 -80)
  (a64i-add-imm buf 29 31 0 0)
  (a64i-stp-offset buf 19 20 31 16)
  (a64i-stp-offset buf 21 22 31 32)
  (a64i-stp-offset buf 23 31 31 48)
  (a64i-sub-imm buf 31 31 1024 0))

(defun a64i-emit-epilogue (buf)
  (a64i-add-imm buf 31 31 1024 0)
  (a64i-ldp-offset buf 23 31 31 48)
  (a64i-ldp-offset buf 21 22 31 32)
  (a64i-ldp-offset buf 19 20 31 16)
  (a64i-ldp-post buf 29 30 31 80)
  (a64i-ret buf 30))

;;; Fixup resolution (byte-based buffer)
(defun a64i-read-u32-at (buf idx)
  (let ((bytes (a64i-buf-bytes buf))
        (off (* idx 4)))
    (+ (aref bytes off) (ash (aref bytes (+ off 1)) 8)
       (ash (aref bytes (+ off 2)) 16))))

(defun a64i-write-bytes-at (buf idx b0 b1 b2 b3)
  (let ((bytes (a64i-buf-bytes buf))
        (off (* idx 4)))
    (aset bytes off b0)
    (aset bytes (+ off 1) b1)
    (aset bytes (+ off 2) b2)
    (aset bytes (+ off 3) b3)))

(defun a64i-resolve-fixups (buf)
  (let ((rest-fixups (a64i-buf-fixups buf))
        (labels (a64i-buf-labels buf)))
    (loop
      (when (null rest-fixups) (return nil))
      (let ((fixup (car rest-fixups)))
        (let ((index (car fixup))
              (label-id (car (cdr fixup)))
              (type (cdr (cdr fixup))))
          (let ((target (gethash label-id labels)))
            (when target
              (let ((offset (- target index)))
                (let ((masked (logand offset 67108863)))
                  (cond
                    ((eql type 518081061)
                     (a64i-write-bytes-at buf index
                       (logand masked 255) (logand (ash masked -8) 255)
                       (logand (ash masked -16) 255) (logior 20 (logand (ash masked -24) 3))))
                    ((eql type 391831169)
                     (a64i-write-bytes-at buf index
                       (logand masked 255) (logand (ash masked -8) 255)
                       (logand (ash masked -16) 255) (logior 148 (logand (ash masked -24) 3))))
                    ((eql type 248172622495451147)
                     (let ((cond-bits (logand (aref (a64i-buf-bytes buf) (* index 4)) 15)))
                       (let ((lo24 (logior (ash (logand offset 524287) 5) cond-bits)))
                         (a64i-write-bytes-at buf index
                           (logand lo24 255) (logand (ash lo24 -8) 255)
                           (logand (ash lo24 -16) 255) #x54))))
                    ((eql type 253724600)
                     (let ((byte-off (* offset 4)))
                       (let ((immlo (logand byte-off 3)))
                         (let ((immhi (logand (ash byte-off -2) 524287)))
                           (let ((rd (logand (aref (a64i-buf-bytes buf) (* index 4)) 31)))
                             (let ((b3 (logior (ash immlo 5) 16)))
                               (let ((lo24 (logior (ash immhi 5) rd)))
                                 (a64i-write-bytes-at buf index
                                   (logand lo24 255) (logand (ash lo24 -8) 255)
                                   (logand (ash lo24 -16) 255) b3)))))))))))))))
      (setq rest-fixups (cdr rest-fixups)))))

;;; End of i386-safe AArch64 encoder functions


;;; ================================================================
;;; i386-safe AArch64 translation loop + pipeline
;;; ================================================================

;;; Store buf pointer for ensure-src-i386a64 / store-dst-i386a64
(defvar *a64i-current-buf* nil)
(defun a64i-set-current-buf (buf) (setq *a64i-current-buf* buf))
(defun a64i-get-current-buf () *a64i-current-buf*)

;;; ensure-src for i386 AArch64 translation
;;; Note: cannot use (if (a64-phys-reg vreg) ...) because nil=0=x0 on i386
(defun ensure-src-i386a64 (vreg scratch)
  (if (a64i-vreg-spilled-p vreg)
      (let ((buf (a64i-get-current-buf)))
        (a64i-emit-load-vreg buf scratch vreg)
        scratch)
      (aref *a64-vreg-to-phys* vreg)))

;;; store-dst for i386 AArch64 translation
(defun store-dst-i386a64 (phys-src vreg)
  (let ((buf (a64i-get-current-buf)))
    (a64i-emit-store-vreg buf phys-src vreg)))

;;; Translate a single MVM instruction to AArch64 using i386-safe encoders.
;;; opcode/operands come from decode-instruction.
;;; buf is a64i-buffer (byte-based).
;;; mvm-to-native-label maps MVM byte offsets to label IDs.
;;; insn-offset is the MVM byte offset of this instruction.
;;; insn-size is the byte size of this MVM instruction.
(defun a64i-translate-insn (buf op operands mvm-to-native-label insn-offset insn-size)
  (a64i-set-current-buf buf)
  (cond
    ;; NOP
    ((= op 0) (a64i-nop buf))
    ;; BREAK
    ((= op 1) (a64i-brk buf 0))
    ;; TRAP
    ((= op 2)
     (let ((code (car operands)))
       (a64i-translate-trap buf code)))
    ;; MOV Vd Vs
    ((= op 16)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (store-dst-i386a64 ps vd))))
    ;; LI: handled separately in td-a64i-translate-fn-body (never reaches here)
    ;; PUSH Vs
    ((= op 18)
     (let ((ps (ensure-src-i386a64 (car operands) 16)))
       (a64i-str-pre buf ps 31 -8)))
    ;; POP Vd
    ((= op 19)
     (if (a64i-vreg-spilled-p (car operands))
         (progn
           (a64i-ldr-post buf 16 31 8)
           (store-dst-i386a64 16 (car operands)))
         (a64i-ldr-post buf (aref *a64-vreg-to-phys* (car operands)) 31 8)))
    ;; ADD Vd Va Vb
    ((= op 32)
     (a64i-translate-binop-reg buf operands 0)) ; 0=ADD
    ;; SUB Vd Va Vb
    ((= op 33)
     (a64i-translate-binop-reg buf operands 1)) ; 1=SUB
    ;; MUL Vd Va Vb
    ((= op 34)
     (let ((vd (car operands))
           (va (car (cdr operands)))
           (vb (car (cdr (cdr operands)))))
       (let ((pa (ensure-src-i386a64 va 16))
             (pb (ensure-src-i386a64 vb 17)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-asr-imm buf 16 pa 1)
           (a64i-mul buf pd 16 pb)
           (a64i-maybe-store pd vd)))))
    ;; DIV Vd Va Vb
    ((= op 35)
     (let ((vd (car operands))
           (va (car (cdr operands)))
           (vb (car (cdr (cdr operands)))))
       (let ((pa (ensure-src-i386a64 va 16))
             (pb (ensure-src-i386a64 vb 17)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-sdiv buf 16 pa pb)
           (a64i-lsl-imm buf pd 16 1)
           (a64i-maybe-store pd vd)))))
    ;; MOD Vd Va Vb
    ((= op 36)
     (let ((vd (car operands))
           (va (car (cdr operands)))
           (vb (car (cdr (cdr operands)))))
       (let ((pa (ensure-src-i386a64 va 16))
             (pb (ensure-src-i386a64 vb 17)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-sdiv buf 16 pa pb)
           (a64i-mul buf 16 16 pb)
           (a64i-sub-reg buf pd pa 16 0 0)
           (a64i-maybe-store pd vd)))))
    ;; NEG Vd Vs
    ((= op 37)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-neg buf pd ps)
           (a64i-maybe-store pd vd)))))
    ;; INC Vd
    ((= op 38)
     (let ((vd (car operands)))
       (let ((pd (ensure-src-i386a64 vd 16)))
         (a64i-add-imm buf pd pd 2 0)
         (a64i-maybe-store pd vd))))
    ;; DEC Vd
    ((= op 39)
     (let ((vd (car operands)))
       (let ((pd (ensure-src-i386a64 vd 16)))
         (a64i-sub-imm buf pd pd 2 0)
         (a64i-maybe-store pd vd))))
    ;; AND Vd Va Vb
    ((= op 40)
     (a64i-translate-binop-reg buf operands 2)) ; 2=AND
    ;; OR Vd Va Vb
    ((= op 41)
     (a64i-translate-binop-reg buf operands 3)) ; 3=OR
    ;; XOR Vd Va Vb
    ((= op 42)
     (a64i-translate-binop-reg buf operands 4)) ; 4=XOR
    ;; SHL Vd Vs imm8
    ((= op 43)
     (let ((vd (car operands))
           (vs (car (cdr operands)))
           (amt (car (cdr (cdr operands)))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-lsl-imm buf pd ps amt)
           (a64i-maybe-store pd vd)))))
    ;; SHR Vd Vs imm8
    ((= op 44)
     (let ((vd (car operands))
           (vs (car (cdr operands)))
           (amt (car (cdr (cdr operands)))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-lsr-imm buf pd ps amt)
           (a64i-maybe-store pd vd)))))
    ;; SAR Vd Vs imm8
    ((= op 45)
     (let ((vd (car operands))
           (vs (car (cdr operands)))
           (amt (car (cdr (cdr operands)))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-asr-imm buf pd ps amt)
           (a64i-maybe-store pd vd)))))
    ;; LDB Vd Vs pos size
    ((= op 46)
     (let ((vd (car operands))
           (vs (car (cdr operands)))
           (pos (car (cdr (cdr operands))))
           (sz (car (cdr (cdr (cdr operands))))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-ubfm buf pd ps pos (+ pos (- sz 1)))
           (a64i-maybe-store pd vd)))))
    ;; SHLV Vd Vs Vc
    ((= op 47)
     (a64i-translate-shift-var buf operands 0)) ; 0=LSLV
    ;; CMP Va Vb
    ((= op 48)
     (let ((pa (ensure-src-i386a64 (car operands) 16))
           (pb (ensure-src-i386a64 (car (cdr operands)) 17)))
       (a64i-cmp-reg buf pa pb)))
    ;; TEST Va Vb
    ((= op 49)
     (let ((pa (ensure-src-i386a64 (car operands) 16))
           (pb (ensure-src-i386a64 (car (cdr operands)) 17)))
       (a64i-tst-reg buf pa pb)))
    ;; SARV Vd Vs Vc
    ((= op 50)
     (a64i-translate-shift-var buf operands 1)) ; 1=ASRV
    ;; BR off16 (#x40 = 64)
    ((= op 64)
     (a64i-translate-branch buf operands insn-offset insn-size mvm-to-native-label 0 0))
    ;; BEQ off16 (#x41 = 65)
    ((= op 65)
     (a64i-translate-bcond buf operands insn-offset insn-size mvm-to-native-label 0))
    ;; BNE off16 (#x42 = 66)
    ((= op 66)
     (a64i-translate-bcond buf operands insn-offset insn-size mvm-to-native-label 1))
    ;; BLT off16 (#x43 = 67)
    ((= op 67)
     (a64i-translate-bcond buf operands insn-offset insn-size mvm-to-native-label 11))
    ;; BGE off16 (#x44 = 68)
    ((= op 68)
     (a64i-translate-bcond buf operands insn-offset insn-size mvm-to-native-label 10))
    ;; BLE off16 (#x45 = 69)
    ((= op 69)
     (a64i-translate-bcond buf operands insn-offset insn-size mvm-to-native-label 13))
    ;; BGT off16 (#x46 = 70)
    ((= op 70)
     (a64i-translate-bcond buf operands insn-offset insn-size mvm-to-native-label 12))
    ;; BNULL Vs off16 (#x47 = 71)
    ((= op 71)
     (let ((ps (ensure-src-i386a64 (car operands) 16))
           (mvm-offset (car (cdr operands))))
       (a64i-cmp-reg buf ps 26) ; VN=x26
       (let ((target-byte (+ insn-offset insn-size mvm-offset)))
         (let ((label (a64i-get-or-make-label target-byte mvm-to-native-label)))
           (let ((idx (a64i-current-index buf)))
             (a64i-bcond buf 0 0) ; EQ
             (a64i-add-fixup buf idx label 248172622495451147)))))) ; :bcond hash
    ;; BNNULL Vs off16 (#x48 = 72)
    ((= op 72)
     (let ((ps (ensure-src-i386a64 (car operands) 16))
           (mvm-offset (car (cdr operands))))
       (a64i-cmp-reg buf ps 26)
       (let ((target-byte (+ insn-offset insn-size mvm-offset)))
         (let ((label (a64i-get-or-make-label target-byte mvm-to-native-label)))
           (let ((idx (a64i-current-index buf)))
             (a64i-bcond buf 1 0) ; NE
             (a64i-add-fixup buf idx label 248172622495451147))))))
    ;; CAR Vd Vs (#x50 = 80)
    ((= op 80)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-ldur buf pd ps (logand -1 511)) ; -1 = 0x1FF (9 bits)
           (a64i-maybe-store pd vd)))))
    ;; CDR Vd Vs (#x51 = 81)
    ((= op 81)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-ldur buf pd ps 7)
           (a64i-maybe-store pd vd)))))
    ;; CONS Vd Va Vb (#x52 = 82)
    ((= op 82)
     (let ((vd (car operands))
           (va (car (cdr operands)))
           (vb (car (cdr (cdr operands)))))
       (let ((pa (ensure-src-i386a64 va 16))
             (pb (ensure-src-i386a64 vb 17)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-stp-offset buf pa pb 24 0) ; x24=VA
           (a64i-add-imm buf pd 24 1 0)
           (a64i-add-imm buf 24 24 16 0)
           (a64i-maybe-store pd vd)))))
    ;; SETCAR Vd Vs (#x53 = 83)
    ((= op 83)
     (let ((pd (ensure-src-i386a64 (car operands) 16))
           (ps (ensure-src-i386a64 (car (cdr operands)) 17)))
       (a64i-stur buf ps pd (logand -1 511))))
    ;; SETCDR Vd Vs (#x54 = 84)
    ((= op 84)
     (let ((pd (ensure-src-i386a64 (car operands) 16))
           (ps (ensure-src-i386a64 (car (cdr operands)) 17)))
       (a64i-stur buf ps pd 7)))
    ;; CONSP Vd Vs (#x55 = 85)
    ((= op 85)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch2 vd)))
           (a64i-movz buf 17 15 0) ; mask=0xF
           (a64i-and-reg buf 16 ps 17)
           (a64i-cmp-imm buf 16 1)
           (a64i-cset buf pd 0) ; EQ
           (a64i-maybe-store pd vd)))))
    ;; ATOM Vd Vs (#x56 = 86)
    ((= op 86)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch2 vd)))
           (a64i-movz buf 17 15 0)
           (a64i-and-reg buf 16 ps 17)
           (a64i-cmp-imm buf 16 1)
           (a64i-cset buf pd 1) ; NE
           (a64i-maybe-store pd vd)))))
    ;; ALLOC-OBJ Vd count subtag (#x60 = 96)
    ((= op 96)
     (a64i-translate-alloc-obj buf operands))
    ;; OBJ-REF Vd Vobj idx (#x61 = 97)
    ((= op 97)
     (a64i-translate-obj-ref buf operands))
    ;; OBJ-SET Vobj idx Vs (#x62 = 98)
    ((= op 98)
     (a64i-translate-obj-set buf operands))
    ;; OBJ-TAG Vd Vs (#x63 = 99)
    ((= op 99)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-movz buf 17 15 0)
           (a64i-and-reg buf pd ps 17)
           (a64i-maybe-store pd vd)))))
    ;; OBJ-SUBTAG Vd Vs (#x64 = 100)
    ((= op 100)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-ldur buf 17 ps (logand -10 511))
           (a64i-movz buf 16 255 0)
           (a64i-and-reg buf pd 17 16)
           (a64i-maybe-store pd vd)))))
    ;; AREF Vd Vobj Vidx (#x65 = 101)
    ((= op 101)
     (a64i-translate-aref buf operands))
    ;; ASET Vobj Vidx Vs (#x66 = 102)
    ((= op 102)
     (a64i-translate-aset buf operands))
    ;; ARRAY-LEN Vd Vobj (#x67 = 103)
    ((= op 103)
     (let ((vd (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 16)))
         (let ((pd (a64i-dest-or-scratch vd)))
           (a64i-ldur buf 17 ps (logand -10 511))
           (a64i-lsr-imm buf 17 17 16)
           (a64i-lsl-imm buf pd 17 1)
           (a64i-maybe-store pd vd)))))
    ;; ALLOC-ARRAY Vd Vcount (#x68 = 104)
    ((= op 104)
     (a64i-translate-alloc-array buf operands))
    ;; LOAD Vd Vaddr width (#x70 = 112)
    ((= op 112)
     (let ((vd (car operands))
           (va (car (cdr operands)))
           (width (car (cdr (cdr operands)))))
       (let ((pa (ensure-src-i386a64 va 16)))
         (let ((pd (a64i-dest-or-scratch2 vd)))
           (a64i-ldr-width buf pd pa 0 width)
           (a64i-maybe-store pd vd)))))
    ;; STORE Vaddr Vs width (#x71 = 113)
    ((= op 113)
     (let ((va (car operands))
           (vs (car (cdr operands)))
           (width (car (cdr (cdr operands)))))
       (let ((pa (ensure-src-i386a64 va 16))
             (ps (ensure-src-i386a64 vs 17)))
         (a64i-str-width buf ps pa 0 width))))
    ;; FENCE (#x72 = 114)
    ((= op 114)
     (a64i-dmb buf 11)) ; ISH
    ;; CALL target (#x80 = 128)
    ((= op 128)
     (let ((target-offset (car operands)))
       (let ((label (gethash target-offset mvm-to-native-label)))
         (when label
           (let ((idx (a64i-current-index buf)))
             (a64i-bl buf 0)
             (a64i-add-fixup buf idx label 391831169)))))) ; :bl hash
    ;; CALL-IND Vs (#x81 = 129)
    ((= op 129)
     (let ((ps (ensure-src-i386a64 (car operands) 16)))
       (a64i-blr buf ps)))
    ;; RET (#x82 = 130)
    ((= op 130)
     (a64i-emit-epilogue buf))
    ;; TAILCALL target (#x83 = 131)
    ((= op 131)
     (let ((target-offset (car operands)))
       (let ((label (gethash target-offset mvm-to-native-label)))
         (when label
           (a64i-add-imm buf 31 31 1024 0) ; ADD SP, SP, #1024
           (a64i-ldp-offset buf 23 31 31 48)
           (a64i-ldp-offset buf 21 22 31 32)
           (a64i-ldp-offset buf 19 20 31 16)
           (a64i-ldp-post buf 29 30 31 80)
           (let ((idx (a64i-current-index buf)))
             (a64i-b buf 0)
             (a64i-add-fixup buf idx label 518081061)))))) ; :b hash
    ;; ALLOC-CONS Vd (#x88 = 136)
    ((= op 136)
     (let ((vd (car operands)))
       (let ((pd (a64i-dest-or-scratch vd)))
         (a64i-mov-reg buf pd 24) ; MOV pd, x24
         (a64i-add-imm buf 24 24 16 0)
         (a64i-maybe-store pd vd))))
    ;; GC-CHECK (#x89 = 137)
    ((= op 137)
     (a64i-cmp-reg buf 24 25)
     (a64i-bcond buf 11 2) ; B.LT +2 (skip BRK)
     (a64i-brk buf 1))
    ;; WRITE-BARRIER (#x8A = 138)
    ((= op 138)
     (a64i-dmb buf 11))
    ;; SAVE-CTX (#x90 = 144)
    ((= op 144)
     (a64i-translate-save-ctx buf operands))
    ;; RESTORE-CTX (#x91 = 145)
    ((= op 145)
     (a64i-translate-restore-ctx buf operands))
    ;; YIELD (#x92 = 146)
    ((= op 146)
     (a64i-sev buf)
     (a64i-wfe buf))
    ;; ATOMIC-XCHG Vd Vaddr Vs (#x93 = 147)
    ((= op 147)
     (a64i-translate-atomic-xchg buf operands))
    ;; IO-READ Vd port width (#xA0 = 160)
    ((= op 160)
     (let ((vd (car operands))
           (port (car (cdr operands)))
           (width (car (cdr (cdr operands)))))
       (let ((pd (a64i-dest-or-scratch vd)))
         (a64i-load-imm64 buf 17 port)
         (a64i-ldr-width buf pd 17 0 width)
         (a64i-maybe-store pd vd))))
    ;; IO-WRITE port Vs width (#xA1 = 161)
    ((= op 161)
     (let ((port (car operands))
           (vs (car (cdr operands)))
           (width (car (cdr (cdr operands)))))
       (let ((ps (ensure-src-i386a64 vs 17)))
         (a64i-load-imm64 buf 16 port)
         (a64i-str-width buf ps 16 0 width))))
    ;; HALT (#xA2 = 162)
    ((= op 162)
     (a64i-wfi buf))
    ;; CLI (#xA3 = 163)
    ((= op 163)
     (a64i-msr-daifset buf 3))
    ;; STI (#xA4 = 164)
    ((= op 164)
     (a64i-msr-daifclr buf 3))
    ;; PERCPU-REF Vd offset (#xA5 = 165)
    ((= op 165)
     (let ((vd (car operands))
           (offset (car (cdr operands))))
       (let ((pd (a64i-dest-or-scratch vd)))
         (a64i-mrs buf 17 50820) ; TPIDR_EL1 = 0xC684
         (if (zerop (mod offset 8))
             (a64i-ldr-unsigned buf pd 17 offset)
             (progn
               (a64i-load-imm64 buf 16 offset)
               (a64i-add-reg buf 17 17 16 0 0)
               (a64i-ldur buf pd 17 0)))
         (a64i-maybe-store pd vd))))
    ;; PERCPU-SET offset Vs (#xA6 = 166)
    ((= op 166)
     (let ((offset (car operands))
           (vs (car (cdr operands))))
       (let ((ps (ensure-src-i386a64 vs 17)))
         (a64i-mrs buf 16 50820)
         (if (zerop (mod offset 8))
             (a64i-str-unsigned buf ps 16 offset)
             (progn
               (a64i-load-imm64 buf 17 offset)
               (a64i-add-reg buf 16 16 17 0 0)
               (a64i-stur buf ps 16 0))))))
    ;; FN-ADDR Vd target (#xA7 = 167)
    ((= op 167)
     (let ((vd (car operands))
           (target-offset (car (cdr operands))))
       (let ((pd (a64i-dest-or-scratch vd)))
         (let ((label (gethash target-offset mvm-to-native-label)))
           (if label
               (let ((idx (a64i-current-index buf)))
                 ;; ADR pd: byte3=0x10, lo24=pd (placeholder)
                 (a64i-emit buf 16 pd) ; 16=0x10
                 (a64i-add-fixup buf idx label 253724600)) ; :adr hash
               (a64i-movz buf pd 0 0)))
         (a64i-maybe-store pd vd))))
    ;; Unknown opcode: BRK with opcode number
    (t (a64i-brk buf op))))

;;; Helper: dest register or x16 scratch
(defun a64i-dest-or-scratch (vd)
  (if (a64i-vreg-spilled-p vd) 16 (aref *a64-vreg-to-phys* vd)))

;;; Helper: dest register or x17 scratch (for ops that use x16 internally)
(defun a64i-dest-or-scratch2 (vd)
  (if (a64i-vreg-spilled-p vd) 17 (aref *a64-vreg-to-phys* vd)))

;;; Helper: store scratch if needed
(defun a64i-maybe-store (pd vd)
  (when (a64i-vreg-spilled-p vd)
    (store-dst-i386a64 pd vd)))

;;; Helper: get or create label for target
(defun a64i-get-or-make-label (target-byte mvm-to-native-label)
  (let ((existing (gethash target-byte mvm-to-native-label)))
    (if existing existing
        (let ((lbl *mvm-label-counter*))
          (setq *mvm-label-counter* (+ lbl 1))
          (puthash target-byte mvm-to-native-label lbl)
          lbl))))

;;; Helper: translate 3-reg binary operation (ADD/SUB/AND/OR/XOR)
(defun a64i-translate-binop-reg (buf operands type)
  (let ((vd (car operands))
        (va (car (cdr operands)))
        (vb (car (cdr (cdr operands)))))
    (let ((pa (ensure-src-i386a64 va 16))
          (pb (ensure-src-i386a64 vb 17)))
      (let ((pd (a64i-dest-or-scratch vd)))
        (cond
          ((= type 0) (a64i-add-reg buf pd pa pb 0 0))
          ((= type 1) (a64i-sub-reg buf pd pa pb 0 0))
          ((= type 2) (a64i-and-reg buf pd pa pb))
          ((= type 3) (a64i-orr-reg buf pd pa pb))
          ((= type 4) (a64i-eor-reg buf pd pa pb)))
        (a64i-maybe-store pd vd)))))

;;; Helper: translate variable shift (SHLV/SARV)
(defun a64i-translate-shift-var (buf operands type)
  (let ((vd (car operands))
        (vs (car (cdr operands)))
        (vc (car (cdr (cdr operands)))))
    (let ((ps (ensure-src-i386a64 vs 16))
          (pc (ensure-src-i386a64 vc 17)))
      (let ((pd (a64i-dest-or-scratch vd)))
        (if (= type 0)
            (a64i-lslv buf pd ps pc)
            (a64i-asrv buf pd ps pc))
        (a64i-maybe-store pd vd)))))

;;; Helper: translate unconditional/conditional branch
(defun a64i-translate-branch (buf operands insn-offset insn-size mvm-to-native-label is-cond cond-code)
  (let ((mvm-offset (car operands)))
    (let ((target-byte (+ insn-offset insn-size mvm-offset)))
      (let ((label (a64i-get-or-make-label target-byte mvm-to-native-label)))
        (let ((idx (a64i-current-index buf)))
          (a64i-b buf 0)
          (a64i-add-fixup buf idx label 518081061)))))) ; :b hash

(defun a64i-translate-bcond (buf operands insn-offset insn-size mvm-to-native-label cond-code)
  (let ((mvm-offset (car operands)))
    (let ((target-byte (+ insn-offset insn-size mvm-offset)))
      (let ((label (a64i-get-or-make-label target-byte mvm-to-native-label)))
        (let ((idx (a64i-current-index buf)))
          (a64i-bcond buf cond-code 0)
          (a64i-add-fixup buf idx label 248172622495451147)))))) ; :bcond hash

;;; TRAP handler
(defun a64i-translate-trap (buf code)
  (cond
    ;; Frame-enter (code < 256)
    ((< code 256)
     (a64i-emit-prologue buf)
     ;; Copy overflow args (>4 params) from caller's stack
     (when (> code 4)
       (a64i-translate-trap-overflow buf code 4)))
    ;; Frame-alloc/frame-free (code < 768)
    ((< code 768) nil)
    ;; Serial write (code = 768 = 0x0300)
    ((= code 768)
     (a64i-asr-imm buf 16 0 1) ; untag x0
     ;; UART base: 0x20000000 (VA mapping, matches standard translator)
     (a64i-load-imm64 buf 17 536870912) ; 0x20000000
     ;; STRB x16, [x17, #0]
     (a64i-str-width buf 16 17 0 0))
    ;; Serial read (code = 769 = 0x0301)
    ((= code 769)
     (a64i-load-imm64 buf 17 536870912) ; 0x20000000
     ;; LDRB w16, [x17, #0x18]
     (a64i-ldr-width buf 16 17 24 0) ; offset 0x18=24
     ;; TBNZ x16, #4, -1 (back to LDRB)
     (a64i-tbnz buf 16 4 (logand -1 16383)) ; imm14 = -1
     ;; LDRB w0, [x17, #0]
     (a64i-ldr-width buf 0 17 0 0)
     ;; LSL x0, x0, #1 (tag)
     (a64i-lsl-imm buf 0 0 1))
    ;; DSB SY (code = 770 = 0x0302)
    ((= code 770)
     (a64i-dsb buf 15)) ; SY=0xF
    ;; Jump to address (code = 771 = 0x0303)
    ((= code 771)
     (a64i-asr-imm buf 0 0 1)
     (a64i-br buf 0))
    ;; switch-idle-stack (code = 1024 = 0x0400)
    ((= code 1024)
     (a64i-mrs buf 16 50820) ; TPIDR_EL1
     (a64i-ldr-unsigned buf 16 16 56) ; [x16, #0x38]
     (a64i-add-imm buf 31 16 0 0)) ; MOV SP, x16
    ;; All other: SVC
    (t (a64i-svc buf code))))

;;; Copy overflow args from caller's stack to local frame slots
(defun a64i-translate-trap-overflow (buf code start)
  (let ((i start))
    (loop
      (when (>= i code) (return nil))
      (let ((src-offset (+ 80 (* (- i 4) 8)))
            (dst-offset (+ -64 (* i -8))))
        (a64i-ldur buf 16 29 src-offset) ; LDR x16, [FP, #src]
        (a64i-stur buf 16 29 (logand dst-offset 511))) ; STR x16, [FP, #dst]
      (setq i (+ i 1)))))

;;; ALLOC-OBJ Vd count subtag
(defun a64i-translate-alloc-obj (buf operands)
  (let ((vd (car operands))
        (count (car (cdr operands)))
        (subtag (car (cdr (cdr operands)))))
    (let ((data-bytes (* count 8))
          (pd (a64i-dest-or-scratch vd)))
      (let ((total-size (logand (+ 8 data-bytes 15) (lognot 15))))
        ;; Header: (count << 16) | subtag
        (a64i-movz buf 16 subtag 0)
        (a64i-movk buf 16 count 1)
        ;; Store header at alloc pointer
        (a64i-stur buf 16 24 0)
        ;; Result = alloc_ptr + 10
        (a64i-add-imm buf pd 24 10 0)
        ;; Bump alloc pointer
        (if (<= total-size 4095)
            (a64i-add-imm buf 24 24 total-size 0)
            (progn
              (a64i-load-imm64 buf 17 total-size)
              (a64i-add-reg buf 24 24 17 0 0)))
        (a64i-maybe-store pd vd)))))

;;; OBJ-REF Vd Vobj idx
(defun a64i-translate-obj-ref (buf operands)
  (let ((vd (car operands))
        (vobj (car (cdr operands)))
        (idx (car (cdr (cdr operands)))))
    (let ((pd (a64i-dest-or-scratch2 vd)))
      (if (= vobj 21) ; VFP = 21
          ;; Frame slot access
          (let ((offset (+ -64 (* idx -8))))
            (if (>= offset -256)
                (a64i-ldur buf pd 29 (logand offset 511))
                (progn
                  (a64i-sub-imm buf 16 29 (- offset) 0)
                  (a64i-ldur buf pd 16 0))))
          ;; Normal object slot access
          (let ((pobj (ensure-src-i386a64 vobj 16)))
            (let ((offset (- (* idx 8) 2)))
              (if (and (>= offset -256) (<= offset 255))
                  (a64i-ldur buf pd pobj (logand offset 511))
                  (progn
                    (a64i-load-imm64 buf 17 offset)
                    (a64i-add-reg buf 17 pobj 17 0 0)
                    (a64i-ldur buf pd 17 0))))))
      (a64i-maybe-store pd vd))))

;;; OBJ-SET Vobj idx Vs
(defun a64i-translate-obj-set (buf operands)
  (let ((vobj (car operands))
        (idx (car (cdr operands)))
        (vs (car (cdr (cdr operands)))))
    (let ((ps (ensure-src-i386a64 vs 17)))
      (if (= vobj 21) ; VFP
          (let ((offset (+ -64 (* idx -8))))
            (if (>= offset -256)
                (a64i-stur buf ps 29 (logand offset 511))
                (progn
                  (a64i-sub-imm buf 16 29 (- offset) 0)
                  (a64i-stur buf ps 16 0))))
          (let ((pobj (ensure-src-i386a64 vobj 16)))
            (let ((offset (- (* idx 8) 2)))
              (if (and (>= offset -256) (<= offset 255))
                  (a64i-stur buf ps pobj (logand offset 511))
                  (progn
                    (a64i-load-imm64 buf 16 offset)
                    (a64i-add-reg buf 16 pobj 16 0 0)
                    (a64i-stur buf ps 16 0)))))))))

;;; AREF Vd Vobj Vidx
(defun a64i-translate-aref (buf operands)
  (let ((vd (car operands))
        (vobj (car (cdr operands)))
        (vidx (car (cdr (cdr operands)))))
    (let ((pobj (ensure-src-i386a64 vobj 16))
          (pidx (ensure-src-i386a64 vidx 17)))
      (let ((pd (a64i-dest-or-scratch vd)))
        ;; x16 = (Vobj - 2) + Vidx * 4 (tagged idx: real*2, shift 2 gives real*8)
        (a64i-sub-imm buf 16 pobj 2 0)
        (a64i-add-reg buf 16 16 pidx 0 2) ; LSL #2
        (a64i-ldur buf pd 16 0)
        (a64i-maybe-store pd vd)))))

;;; ASET Vobj Vidx Vs
(defun a64i-translate-aset (buf operands)
  (let ((vobj (car operands))
        (vidx (car (cdr operands))))
    (let ((pobj (ensure-src-i386a64 vobj 16))
          (pidx (ensure-src-i386a64 vidx 17)))
      (a64i-sub-imm buf 16 pobj 2 0)
      (a64i-add-reg buf 16 16 pidx 0 2)
      (let ((ps (ensure-src-i386a64 (car (cdr (cdr operands))) 17)))
        (a64i-stur buf ps 16 0)))))

;;; ALLOC-ARRAY Vd Vcount
(defun a64i-translate-alloc-array (buf operands)
  (let ((vd (car operands))
        (vc (car (cdr operands))))
    (let ((pcount (ensure-src-i386a64 vc 17)))
      (let ((pd (a64i-dest-or-scratch vd)))
        ;; Header: (count << 16) | 0x32
        (a64i-lsl-imm buf 16 pcount 16)
        (a64i-movk buf 16 50 0) ; 0x32
        ;; Store header
        (a64i-stur buf 16 24 0)
        ;; Aligned alloc: (count+2)/2 * 16
        (a64i-add-imm buf 17 pcount 2 0)
        (a64i-lsr-imm buf 17 17 1)
        (a64i-lsl-imm buf 17 17 4)
        ;; Result = alloc_ptr + 10
        (a64i-add-imm buf pd 24 10 0)
        ;; Bump
        (a64i-add-reg buf 24 24 17 0 0)
        (a64i-maybe-store pd vd)))))

;;; ATOMIC-XCHG Vd Vaddr Vs
(defun a64i-translate-atomic-xchg (buf operands)
  (let ((vd (car operands))
        (vaddr (car (cdr operands)))
        (vs (car (cdr (cdr operands)))))
    (let ((pa (ensure-src-i386a64 vaddr 16))
          (ps (ensure-src-i386a64 vs 17)))
      (let ((pd (a64i-dest-or-scratch vd)))
        ;; Pick status reg that doesn't conflict
        (let ((status (cond ((not (= ps 17)) 17)
                            ((not (= pa 15)) 15)
                            (t 0))))
          ;; loop: LDXR pd, [pa]
          (let ((loop-idx (a64i-current-index buf)))
            (a64i-ldxr buf pd pa)
            ;; STXR status, ps, [pa]
            (a64i-stxr buf status ps pa)
            ;; CBNZ-W status, loop
            (let ((back-offset (- loop-idx (a64i-current-index buf))))
              (a64i-cbnz-w buf status (logand back-offset 524287)))))
        (a64i-maybe-store pd vd)))))

;;; SAVE-CTX Vd — simplified i386-safe version with byte-level patching
(defun a64i-translate-save-ctx (buf operands)
  (let ((vd (car operands)))
    (let ((pa (ensure-src-i386a64 vd 0)))
      ;; 1. Push callee-saved
      (a64i-stp-pre buf 20 21 31 -48)
      (a64i-stp-offset buf 22 23 31 16)
      (a64i-stp-offset buf 29 30 31 32)
      ;; 2. Save SP
      (a64i-add-imm buf 16 31 0 0) ; MOV x16, SP
      (a64i-str-unsigned buf 16 pa 0)
      ;; 3. Save regs
      (a64i-str-unsigned buf 24 pa 8)
      (a64i-str-unsigned buf 25 pa 16)
      (a64i-str-unsigned buf 19 pa 24)
      ;; 4. ADR x17, continuation — placeholder, record position
      (let ((adr-idx (a64i-current-index buf)))
        (a64i-emit buf 16 17) ; placeholder: byte3=0x10, lo24=17(x17)
        (a64i-str-unsigned buf 17 pa 40) ; [pa+0x28] = continuation
        ;; 5. Save per-CPU obj-alloc/obj-limit
        (a64i-mrs buf 16 50820) ; TPIDR_EL1
        (a64i-ldr-unsigned buf 17 16 40) ; [x16, #0x28]
        (a64i-str-unsigned buf 17 pa 104) ; [pa+0x68]
        (a64i-ldr-unsigned buf 17 16 48) ; [x16, #0x30]
        (a64i-str-unsigned buf 17 pa 112) ; [pa+0x70]
        ;; 6. Initial save: return 0
        (a64i-movz buf 0 0 0)
        ;; 7. B to pop (placeholder)
        (let ((b-idx (a64i-current-index buf)))
          (a64i-b buf 0)
          ;; 8. Continuation label
          (let ((cont-idx (a64i-current-index buf)))
            ;; Patch ADR x17 at adr-idx
            (let ((byte-off (* (- cont-idx adr-idx) 4)))
              (let ((immlo (logand byte-off 3))
                    (immhi (logand (ash byte-off -2) 524287)))
                (let ((b3 (logior (ash immlo 5) 16))
                      (lo24 (logior (ash immhi 5) 17)))
                  (a64i-write-bytes-at buf adr-idx
                    (logand lo24 255) (logand (ash lo24 -8) 255)
                    (logand (ash lo24 -16) 255) b3))))
            ;; Resume path: return 2
            (a64i-movz buf 0 2 0)
            ;; 9. Pop callee-saved (both paths)
            (let ((pop-idx (a64i-current-index buf)))
              ;; Patch B forward at b-idx
              (let ((b-offset (- pop-idx b-idx)))
                (let ((masked (logand b-offset 67108863)))
                  (a64i-write-bytes-at buf b-idx
                    (logand masked 255) (logand (ash masked -8) 255)
                    (logand (ash masked -16) 255)
                    (logior 20 (logand (ash masked -24) 3)))))
              (a64i-ldp-offset buf 29 30 31 32)
              (a64i-ldp-offset buf 22 23 31 16)
              (a64i-ldp-post buf 20 21 31 48)
              (store-dst-i386a64 0 vd))))))))

;;; RESTORE-CTX Vd
(defun a64i-translate-restore-ctx (buf operands)
  (let ((vd (car operands)))
    (let ((pa (ensure-src-i386a64 vd 0)))
      ;; Move addr to x16
      (a64i-mov-reg buf 16 pa)
      ;; Load continuation
      (a64i-ldr-unsigned buf 17 16 40) ; [x16+0x28]
      ;; Restore per-CPU
      (a64i-mrs buf 0 50820)
      (a64i-ldr-unsigned buf 1 16 104) ; [x16+0x68]
      (a64i-str-unsigned buf 1 0 40)
      (a64i-ldr-unsigned buf 1 16 112) ; [x16+0x70]
      (a64i-str-unsigned buf 1 0 48)
      ;; Restore callee-saved
      (a64i-ldr-unsigned buf 19 16 24)
      (a64i-ldr-unsigned buf 24 16 8)
      (a64i-ldr-unsigned buf 25 16 16)
      ;; Restore SP
      (a64i-ldr-unsigned buf 0 16 0)
      (a64i-add-imm buf 31 0 0 0) ; MOV SP, x0
      ;; (sched-lock-addr omitted for fixpoint — no actors)
      ;; BR to continuation
      (a64i-br buf 17))))

;;; ================================================================
;;; Translation pipeline
;;; ================================================================

;;; Branch target pre-scan (same as td-a64-scan-branches)
(defun a64i-scan-branches (bytecode offset len mvm-to-native-label)
  (let ((pos offset)
        (limit (+ offset len)))
    (loop
      (when (>= pos limit) (return nil))
      (let ((decoded (decode-instruction bytecode pos)))
        (let ((opcode (car decoded))
              (operands (car (cdr decoded)))
              (new-pos (cdr (cdr decoded))))
          (when (>= opcode 64) ; #x40
            (when (<= opcode 72) ; #x48
              (let ((off-idx 0))
                (when (>= opcode 71) ; #x47 BNULL/BNNULL
                  (setq off-idx 1))
                (let ((mvm-offset (nth off-idx operands)))
                  (let ((target-byte (+ pos (- new-pos pos) mvm-offset)))
                    (let ((existing (gethash target-byte mvm-to-native-label)))
                      (when (null existing)
                        (let ((lbl *mvm-label-counter*))
                          (setq *mvm-label-counter* (+ lbl 1))
                          (puthash target-byte mvm-to-native-label lbl)))))))))
          (setq pos new-pos))))))

;;; Per-function translation with LI interception
(defun a64i-translate-fn-body (bytecode offset len buf mvm-to-native-label)
  (a64i-set-current-buf buf)
  (let ((pos offset)
        (limit (+ offset len)))
    (loop
      (when (>= pos limit) (return nil))
      ;; Set label if exists
      (let ((label (gethash pos mvm-to-native-label)))
        (when label
          (a64i-set-label buf label)))
      ;; Check for LI (opcode 17): intercept before decode-instruction
      (if (= (aref bytecode pos) 17)
          ;; LI Vd, imm64: opcode(1) + vreg(1) + imm64(8) = 10 bytes
          (let ((vd (aref bytecode (+ pos 1))))
            (let ((pd (a64i-dest-or-scratch vd)))
              (a64i-load-imm64-raw buf pd bytecode (+ pos 2))
              (a64i-maybe-store pd vd))
            (setq pos (+ pos 10)))
          ;; Normal decode
          (let ((decoded (decode-instruction bytecode pos)))
            (let ((opcode (car decoded))
                  (operands (car (cdr decoded)))
                  (new-pos (cdr (cdr decoded))))
              (a64i-translate-insn buf opcode operands mvm-to-native-label
                                   pos (- new-pos pos))
              (setq pos new-pos)))))))

;;; Main translation entry point: i386→AArch64
(defun translate-mvm-to-aarch64-from-i386 (bytecode function-table)
  (write-char-serial 105) (write-char-serial 51) ;; i3
  (write-char-serial 56) (write-char-serial 54) ;; 86
  (write-char-serial 97) (write-char-serial 54) (write-char-serial 52) ;; a64
  (write-char-serial 10)
  (let ((buf (make-a64i-buffer)))
    (let ((n-functions (length function-table)))
      (print-dec n-functions) (write-char-serial 10)
      (let ((mvm-to-native-label (make-hash-table)))
        ;; Register labels for function entry points
        (let ((rest-ft function-table)
              (i 0))
          (loop
            (when (>= i n-functions) (return nil))
            (let ((entry (car rest-ft)))
              (let ((offset (car (cdr entry))))
                (let ((lbl *mvm-label-counter*))
                  (setq *mvm-label-counter* (+ lbl 1))
                  (puthash offset mvm-to-native-label lbl))))
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))))
        ;; Pre-scan branches
        (let ((rest-ft function-table)
              (i 0))
          (loop
            (when (>= i n-functions) (return nil))
            (let ((entry (car rest-ft)))
              (let ((offset (car (cdr entry)))
                    (len (car (cdr (cdr entry)))))
                (a64i-scan-branches bytecode offset len mvm-to-native-label)))
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))))
        ;; Translate
        (write-char-serial 84) (write-char-serial 10)
        (let ((fn-map (make-hash-table)))
          (let ((rest-ft function-table)
                (i 0))
            (loop
              (when (>= i n-functions) (return nil))
              (let ((entry (car rest-ft)))
                (let ((name (car entry))
                      (offset (car (cdr entry)))
                      (len (car (cdr (cdr entry)))))
                  ;; Set label
                  (let ((fn-label (gethash offset mvm-to-native-label)))
                    (when fn-label
                      (a64i-set-label buf fn-label)))
                  ;; Record native offset
                  (puthash name fn-map (a64i-buf-pos buf))
                  ;; Translate body
                  (a64i-translate-fn-body bytecode offset len buf mvm-to-native-label)))
              (setq rest-ft (cdr rest-ft))
              (setq i (+ i 1))
              (when (zerop (mod i 50))
                (write-char-serial 35)
                (print-dec i)
                (write-char-serial 10))))
          ;; End label
          (let ((end-label (gethash (array-length bytecode) mvm-to-native-label)))
            (when end-label
              (a64i-set-label buf end-label)))
          ;; Resolve fixups
          (write-char-serial 82) (write-char-serial 10)
          (a64i-resolve-fixups buf)
          ;; Return (bytes . (size . fn-map))
          (let ((native-size (a64i-buf-pos buf)))
            (write-char-serial 78) (write-char-serial 83) ;; NS
            (print-dec native-size) (write-char-serial 10)
            ;; Compute FNV of native code
            (let ((fnv (td-fnv-native (a64i-buf-bytes buf) native-size)))
              (write-char-serial 70) (write-char-serial 78) ;; FN
              (write-char-serial 86) (write-char-serial 58) ;; V:
              (print-dec fnv) (write-char-serial 10))
            (cons (a64i-buf-bytes buf) (cons native-size fn-map))))))))

;;; ================================================================
;;; Image assembly: i386→AArch64
;;; ================================================================

;;; Unpack boot preamble from packed array into image
(defun td-generate-aarch64-boot-i386 ()
  (when (null *a64-boot-preamble-packed*)
    (a64i-init-boot-preamble))
  ;; Diagnostic: check packed array contents
  (write-char-serial 91) ;; [
  (write-char-serial 76) ;; L
  (print-dec (array-length *a64-boot-preamble-packed*))
  (write-char-serial 44) ;; ,
  ;; Direct read of first 3 elements
  (write-char-serial 48) ;; 0
  (write-char-serial 61) ;; =
  (print-dec (aref *a64-boot-preamble-packed* 0))
  (write-char-serial 44)
  (write-char-serial 49) ;; 1
  (write-char-serial 61) ;; =
  (print-dec (aref *a64-boot-preamble-packed* 1))
  (write-char-serial 44)
  ;; Test: write then read
  (let ((test-arr (make-array 3)))
    (aset test-arr 0 42)
    (write-char-serial 84) ;; T
    (write-char-serial 61) ;; =
    (print-dec (aref test-arr 0)))
  (write-char-serial 93) ;; ]
  (write-char-serial 10)
  (let ((packed *a64-boot-preamble-packed*)
        (size *a64-boot-preamble-size*)
        (packed-count (array-length *a64-boot-preamble-packed*))
        (byte-idx 0)
        (pi 0))
    (loop
      (when (>= pi packed-count) (return size))
      (let ((val (aref packed pi)))
        (let ((b0 (logand val 255))
              (b1 (logand (ash val -8) 255))
              (b2 (logand (ash val -16) 255)))
          (when (< byte-idx size) (img-emit b0))
          (setq byte-idx (+ byte-idx 1))
          (when (< byte-idx size) (img-emit b1))
          (setq byte-idx (+ byte-idx 1))
          (when (< byte-idx size) (img-emit b2))
          (setq byte-idx (+ byte-idx 1))))
      (setq pi (+ pi 1)))))

;;; Assemble Gen1 AArch64 image from i386 host
(defun td-assemble-gen1-aarch64-i386 (result bc ft)
  ;; result = (bytes . (size . fn-map))
  (let ((native-bytes (car result))
        (native-size (car (cdr result)))
        (fn-map (cdr (cdr result))))
    ;; 1. Init
    (img-init)
    (write-char-serial 65) (write-char-serial 52) ;; A4
    (write-char-serial 58) (write-char-serial 10)
    ;; 2. Boot preamble from packed array
    (let ((boot-size (td-generate-aarch64-boot-i386)))
      (write-char-serial 80)
      (print-dec boot-size) (write-char-serial 10)
      ;; Pad to 0x1000
      (loop
        (when (>= (img-pos) 4096) (return nil))
        (img-emit 0))
      ;; 3. B instruction to kernel-main at 0x1000
      (write-char-serial 75)
      ;; Read kernel-main hash from running kernel's metadata at VA 0x500000
      (let ((km-hash (td-read-u32 #x500028)))
        (print-dec km-hash) (write-char-serial 10)
        (let ((km-native-off (gethash km-hash fn-map)))
          (write-char-serial 79)
          (if km-native-off
              (let ((dummy1 (print-dec km-native-off)))
                (write-char-serial 10)
                ;; km-native-off is in bytes (since a64i buffer is byte-based)
                (let ((km-insn-offset (ash km-native-off -2)))
                  ;; B forward: offset = km_insn_offset + 1
                  (let ((b-offset (+ km-insn-offset 1)))
                    (let ((masked (logand b-offset 67108863)))
                      ;; Emit B instruction as 4 raw bytes
                      (img-emit (logand masked 255))
                      (img-emit (logand (ash masked -8) 255))
                      (img-emit (logand (ash masked -16) 255))
                      (img-emit (logior 20 (logand (ash masked -24) 3)))))))
              ;; No kernel-main — emit NOP
              (progn
                (write-char-serial 33) (write-char-serial 10)
                (img-emit 31) (img-emit 32) (img-emit 3) (img-emit 213))))) ; NOP = D503201F
      ;; 4. Copy native code (starts at 0x1004)
      (write-char-serial 78)
      ;; Store native-code-offset in running kernel's scratch area
      (td-write-u32 #x500050 (img-pos)) ; 0x300050
      (let ((i 0))
        (loop
          (when (>= i native-size) (return nil))
          (img-emit (aref native-bytes i))
          (setq i (+ i 1))
          (when (zerop (mod i 50000))
            (write-char-serial 46))))
      (write-char-serial 10)
      ;; 5. Append bytecodes
      (write-char-serial 84)
      (let ((bc-len (array-length bc))
            (bc-img-offset (img-pos)))
        (let ((bi 0))
          (loop
            (when (>= bi bc-len) (return nil))
            (img-emit (aref bc bi))
            (setq bi (+ bi 1))))
        ;; 6. Append function table — raw byte copy from source image
        ;; (avoids u32 overflow for name hashes with byte 3 >= 0x80 on i386)
        (let ((ft-img-offset (img-pos))
              (src-ft-addr (+ (td-read-u32 #x500030) (td-read-u32 #x500014)))
              (ft-count (td-read-u32 #x500018)))
          (let ((total-ft-bytes (* ft-count 12))
                (bi 0))
            (loop
              (when (>= bi total-ft-bytes) (return nil))
              (img-emit (mem-ref (+ src-ft-addr bi) :u8))
              (setq bi (+ bi 1))))
          (write-char-serial 10)
          (print-dec ft-count) (write-char-serial 10)
          ;; 7. Write metadata at image offset 0x440000
          ;; AArch64: load at PA 0x40080000, VA = PA - 0x40000000 = 0x80000
          ;; Metadata VA = 0x500000, so image offset = 0x500000 - 0x80000 = 0x440000
          (let ((md-img-off #x480000))
            ;; magic MVMT = 0x544D564D — write as individual bytes
            ;; (0x544D564D > 2^30, overflows i386 30-bit fixnum)
            (let ((base (+ #x08000000 md-img-off)))
              (setf (mem-ref base :u8) #x4D)
              (setf (mem-ref (+ base 1) :u8) #x56)
              (setf (mem-ref (+ base 2) :u8) #x4D)
              (setf (mem-ref (+ base 3) :u8) #x54))
            ;; version = 1
            (img-patch-u32 (+ md-img-off 4) 1)
            ;; my-architecture = 1 (aarch64)
            (img-patch-u32 (+ md-img-off 8) 1)
            ;; bytecode-offset
            (img-patch-u32 (+ md-img-off 12) bc-img-offset)
            ;; bytecode-length
            (img-patch-u32 (+ md-img-off 16) bc-len)
            ;; fn-table-offset
            (img-patch-u32 (+ md-img-off 20) ft-img-offset)
            ;; fn-table-count
            (img-patch-u32 (+ md-img-off 24) ft-count)
            ;; native-code-offset
            (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x500050))
            ;; native-code-length
            (img-patch-u32 (+ md-img-off 32) native-size)
            ;; preamble-size = 0x1000
            (img-patch-u32 (+ md-img-off 36) 4096)
            ;; kernel-main-hash-lo — raw byte copy (avoids u32 overflow on i386)
            (let ((dst-base (+ #x08000000 md-img-off 40)))
              (setf (mem-ref dst-base :u8) (mem-ref #x500028 :u8))
              (setf (mem-ref (+ dst-base 1) :u8) (mem-ref #x500029 :u8))
              (setf (mem-ref (+ dst-base 2) :u8) (mem-ref #x50002A :u8))
              (setf (mem-ref (+ dst-base 3) :u8) (mem-ref #x50002B :u8)))
            ;; kernel-main native offset
            (let ((km-native-off (gethash (td-read-u32 #x500028) fn-map)))
              (if km-native-off
                  (img-patch-u32 (+ md-img-off 44) km-native-off)
                  (img-patch-u32 (+ md-img-off 44) 0)))
            ;; image-load-addr: 0x80000
            (img-patch-u32 (+ md-img-off 48) #x80000)
            ;; target-architecture (default 0=x64)
            (img-patch-u32 (+ md-img-off 52) 0)
            ;; mode (default 0=cross-compile)
            (img-patch-u32 (+ md-img-off 56) 0))
          ;; Total size must cover metadata at 0x440000
          (let ((total-size (+ #x480000 64)))
            (write-char-serial 65) (write-char-serial 52) ;; A4
            (write-char-serial 61)
            (print-dec total-size) (write-char-serial 10)
            total-size))))))

;;; Top-level: build AArch64 from i386
(defun build-aarch64-from-i386 (bc ft)
  (write-char-serial 105) (write-char-serial 51) ;; i3
  (write-char-serial 56) (write-char-serial 54) ;; 86
  (write-char-serial 62) (write-char-serial 97) ;; >a
  (write-char-serial 54) (write-char-serial 52) ;; 64
  (write-char-serial 10)
  (let ((result (translate-mvm-to-aarch64-from-i386 bc ft)))
    (let ((native-size (car (cdr result))))
      (write-char-serial 78) ;; N
      (print-dec native-size) (write-char-serial 10)
      ;; FNV hash for verification
      (let ((fnv (td-fnv-native (car result) native-size)))
        (let ((hash fnv))
          (write-char-serial 72) ;; H
          (print-dec hash) (write-char-serial 10)))
      (td-assemble-gen1-aarch64-i386 result bc ft)
      native-size)))

