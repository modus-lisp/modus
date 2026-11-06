;;;; lib/core-from-card.lisp -- a Pi reads its core from a file on the SD card, and reboots
;;;; once into a second image (the firmware's tryboot).
;;;;
;;;; WHY.  The firmware places a core with `initramfs desk.core 0x18000000' -- but only from
;;;; the boot partition, a 128 MB FAT the firmware itself reads, which holds one 38 MB
;;;; kernel and one 44 MB core and not a second pair.  A kernel that finds no core at
;;;; 0x18000000 now reads the file the CORE= bootarg names, from any FAT32 partition
;;;; (CORE=2:/boot/desk-b.core: partition 2, the card's store), and restores it exactly as
;;;; if the firmware had put it there.  The boot partition then holds kernels (6 MB
;;;; gzipped) and the cores live on the store.
;;;;
;;;; A/B.  config.txt keeps its own lines and gains a section the firmware reads only on a
;;;; tryboot:
;;;;     [tryboot]
;;;;     kernel=kernel8-b.img
;;;;     cmdline=cmdline-b.txt        <- its CORE= names B's core
;;;;     [all]
;;;; (REBOOT :TRYBOOT T) asks the firmware for that once (SET_REBOOT_FLAGS, as Linux's
;;;; `reboot "0 tryboot"') and resets through the watchdog.  The next reset or power cycle
;;;; is A again, so a B that does not come up costs a power cycle, not a card.
;;;;
;;;; WHEN IT RUNS.  %CORE-REQUESTED-P runs before INIT-SYMBOL-TABLE: no globals, no symbol
;;;; table, no condition system, and on this board every exception vector but the fault
;;;; reporter is `b .', so a fault here is a silent hang.  So: raw registers and raw memory,
;;;; no global, no ERROR, every wait bounded, every number range-checked before it is used
;;;; as an address, and every failure a NIL -- the board then boots the bare REPL, as one
;;;; with no core always has.  What it does is said on the serial console (CFC lines).
;;;;
;;;; SCRATCH.  Sectors are read into [0x17E00000, 0x17E01000), the MB below the core,
;;;; inside the heap's upper semispace, which nothing uses before the restore; the core
;;;; itself goes to %CORE-ADDR.  The property mailbox buffer is the one %RPI-ARM-RAM-TOP
;;;; uses (0x111F0000).

;;; ---- raw helpers ------------------------------------------------------------------------
(defun %cfc-scr () #x17E00000)                    ; one sector
(defun %cfc-fatbuf () #x17E00200)                 ; one FAT sector
(defun %cfc-fatlba-slot () #x17E00400)            ; which FAT sector FATBUF holds (+1; 0 none)
(defun %cfc-lfn () #x17E00800)                    ; a long name, one u16 per character

(defun %cfc-le16 (a) (+ (mem-ref a :u8) (* 256 (mem-ref (+ a 1) :u8))))
(defun %cfc-le32 (a) (+ (%cfc-le16 a) (* 65536 (%cfc-le16 (+ a 2)))))

(defun %cfc-say (s n)
  "CFC <S> <N> on the serial console (N omitted when NIL)."
  (write-string-serial "CFC ") (write-string-serial s)
  (when n (write-char-serial 32) (print-dec n))
  (write-char-serial 10)
  nil)

;;; ---- the property mailbox: one tag, two words in, two words out ----------------------------
(defun %cfc-mbox (tag a b)
  "Send property TAG with the value {A, B}; the answer's first word, or -1 when the firmware
   does not answer."
  (let ((buf #x111F0000) (mbox #x3F00B880) (i 0))
    (setf (mem-ref buf :u32) 32)
    (setf (mem-ref (+ buf 4) :u32) 0)
    (setf (mem-ref (+ buf 8) :u32) tag)
    (setf (mem-ref (+ buf 12) :u32) 8)
    (setf (mem-ref (+ buf 16) :u32) 8)
    (setf (mem-ref (+ buf 20) :u32) a)
    (setf (mem-ref (+ buf 24) :u32) b)
    (setf (mem-ref (+ buf 28) :u32) 0)
    (loop (when (or (> i 1000000) (zerop (logand (mem-ref (+ mbox #x38) :u32) #x80000000)))
            (return nil))
          (setq i (+ i 1)))
    (setf (mem-ref (+ mbox #x20) :u32) (logior #xC0000000 buf 8))
    (setq i 0)
    (loop (when (> i 1000000) (return -1))
          (when (zerop (logand (mem-ref (+ mbox #x18) :u32) #x40000000))
            (mem-ref mbox :u32)
            (return (mem-ref (+ buf 20) :u32)))
          (setq i (+ i 1)))))

;;; ---- the SD card (the Arasan SDHCI; net/emmc.lisp without its global and its ERRORs) ------
(defun %cfc-reg (o) (mem-ref (+ #x3F300000 o) :u32))
(defun %cfc-set (o v) (setf (mem-ref (+ #x3F300000 o) :u32) v))

(defun %cfc-wait (o mask want n)
  (let ((i 0))
    (loop (when (= (logand (%cfc-reg o) mask) want) (return t))
          (when (> i n) (return nil))
          (setq i (+ i 1)))))

(defun %cfc-wait-irpt (mask)
  "T once an INTERRUPT bit in MASK is set; NIL on an error bit or after the bound."
  (let ((i 0))
    (loop
      (let ((r (%cfc-reg #x30)))
        (when (logbitp 15 r) (%cfc-set #x30 #xFFFFFFFF) (return nil))
        (unless (zerop (logand r mask)) (%cfc-set #x30 mask) (return t))
        (when (> i 2000000) (return nil))
        (setq i (+ i 1))))))

(defun %cfc-cmd (cmdtm arg)
  "RESP0, or -1 when the command failed."
  (%cfc-wait #x24 1 0 200000)
  (%cfc-set #x30 #xFFFFFFFF)
  (%cfc-set #x08 arg)
  (%cfc-set #x0C cmdtm)
  (if (%cfc-wait-irpt 1) (%cfc-reg #x10) -1))

(defun %cfc-sd-init ()
  "Bring the card up as net/emmc.lisp's EMMC-INIT does.  T, or NIL with a CFC line."
  (%cfc-mbox #x00028001 0 2)                                 ; SD power off, wait
  (%cfc-mbox #x00028001 0 3)                                 ; on, wait
  (%cfc-set #x2C (ash 1 24))
  (if (not (%cfc-wait #x2C (ash 1 24) 0 200000))
      (%cfc-say "sd: reset never cleared" nil)
      (progn
        (setf (mem-ref #x3F200010 :u32) #x7F706949)          ; GPIO 48-53 to ALT3
        (setf (mem-ref #x3F200014 :u32) #x67706FFF)
        (%cfc-set #x34 #xFFFFFFFF) (%cfc-set #x38 0)
        (%cfc-set #x2C (logior (ash #xE 16) (ash 1 14) 1))
        (if (not (%cfc-wait #x2C 2 2 2000000))
            (%cfc-say "sd: clock never stable" nil)
            (progn
              (%cfc-set #x2C (logior (%cfc-reg #x2C) 4))
              (%cfc-cmd #x00000000 0)
              (if (/= (logand (%cfc-cmd #x081A0000 #x1AA) #xFFF) #x1AA)
                  (%cfc-say "sd: CMD8 echo wrong" nil)
                  (let ((tries 0) (ready nil))
                    (loop
                      (%cfc-cmd #x371A0000 0)
                      (let ((ocr (%cfc-cmd #x29020000 #x40FF8000)))
                        (when (and (> ocr 0) (logbitp 31 ocr)) (setq ready ocr) (return nil)))
                      (setq tries (+ tries 1))
                      (when (> tries 400) (return nil)))
                    (cond ((null ready) (%cfc-say "sd: ACMD41 never ready" nil))
                          ((not (logbitp 30 ready)) (%cfc-say "sd: an SDSC card" nil))
                          (t
                           (%cfc-cmd #x02090000 0)
                           (let ((rca (ash (%cfc-cmd #x031A0000 0) -16)))
                             (%cfc-cmd #x071B0000 (ash rca 16))
                             (%cfc-cmd #x371A0000 (ash rca 16))
                             (%cfc-cmd #x061A0000 2)
                             (%cfc-set #x28 (logior (%cfc-reg #x28) 2))      ; 4-bit
                             (%cfc-set #x2C (logand (%cfc-reg #x2C) (lognot 4)))
                             (%cfc-set #x2C (logior (logand (%cfc-reg #x2C) (lognot #xFFC0))
                                                    (ash 5 8)))            ; ~25 MHz
                             (if (%cfc-wait #x2C 2 2 2000000)
                                 (progn (%cfc-set #x2C (logior (%cfc-reg #x2C) 4)) t)
                                 (%cfc-say "sd: clock never stable at speed" nil))))))))))))

(defun %cfc-read-sector (lba dst)
  "Sector LBA to raw memory DST (512 bytes).  T, or NIL."
  (%cfc-set #x04 (logior (ash 1 16) 512))
  (if (< (%cfc-cmd #x113A0010 lba) 0)
      nil
      (if (not (%cfc-wait-irpt (ash 1 5)))
          nil
          (progn
            (dotimes (n 128)
              (setf (mem-ref (+ dst (* 4 n)) :u32) (%cfc-reg #x20)))
            (%cfc-wait-irpt 2)))))

;;; ---- FAT32, read only ---------------------------------------------------------------------
;;; A volume is four numbers kept in locals by the callers: the FAT's first sector, the data
;;; area's first sector, sectors per cluster, and the root directory's cluster.

(defun %cfc-next-cluster (fat c)
  "The FAT entry for cluster C, or 0 when it cannot be read."
  (let* ((byte (* c 4)) (sec (+ fat (floor byte 512))) (off (mod byte 512)))
    (if (or (= (mem-ref (%cfc-fatlba-slot) :u32) (+ sec 1))
            (and (%cfc-read-sector sec (%cfc-fatbuf))
                 (progn (setf (mem-ref (%cfc-fatlba-slot) :u32) (+ sec 1)) t)))
        (logand (%cfc-le32 (+ (%cfc-fatbuf) off)) #x0FFFFFFF)
        0)))

(defun %cfc-code-upcase (c) (if (and (>= c 97) (<= c 122)) (- c 32) c))

(defun %cfc-name-matches (args s e n)
  "Do ARGS[S, E) and the N-character name at %CFC-LFN match, ignoring ASCII case?"
  (and (= (- e s) n)
       (let ((i 0) (ok t))
         (loop (when (or (not ok) (>= i n)) (return ok))
               (unless (= (%cfc-code-upcase (%prim-aref args (+ s i)))
                          (%cfc-code-upcase (mem-ref (+ (%cfc-lfn) (* 2 i)) :u16)))
                 (setq ok nil))
               (setq i (+ i 1))))))

(defun %cfc-short-name (ent)
  "Put the 8.3 name of the entry at ENT into %CFC-LFN; its length."
  (let ((n 0) (last 0))
    (dotimes (i 8)
      (let ((c (mem-ref (+ ent i) :u8)))
        (setf (mem-ref (+ (%cfc-lfn) (* 2 i)) :u16) c)
        (unless (= c 32) (setq last (+ i 1)))))
    (setq n last)
    (unless (= (mem-ref (+ ent 8) :u8) 32)
      (setf (mem-ref (+ (%cfc-lfn) (* 2 n)) :u16) 46)       ; .
      (setq n (+ n 1))
      (dotimes (i 3)
        (let ((c (mem-ref (+ ent 8 i) :u8)))
          (unless (= c 32)
            (setf (mem-ref (+ (%cfc-lfn) (* 2 n)) :u16) c)
            (setq n (+ n 1))))))
    n))

(defun %cfc-lfn-piece (ent)
  "Copy a long-name entry's 13 characters to their place in %CFC-LFN; the name's length when
   this is its last piece (the first one met), else -1."
  (let* ((seq (mem-ref ent :u8)) (k (- (logand seq #x1F) 1)) (base (* k 13)) (len -1) (j 0))
    (when (and (>= k 0) (< k 20))
      (dotimes (jj 13)
        (let* ((off (cond ((< jj 5) (+ 1 (* 2 jj)))            ; 1 3 5 7 9
                          ((< jj 11) (+ 14 (* 2 (- jj 5))))     ; 14 .. 24
                          (t (+ 28 (* 2 (- jj 11))))))          ; 28 30
               (c (%cfc-le16 (+ ent off))))
          (when (and (< len 0) (or (= c 0) (= c #xFFFF)) (logbitp 6 seq))
            (setq len (+ base j)))
          (setf (mem-ref (+ (%cfc-lfn) (* 2 (+ base j))) :u16) c))
        (setq j (+ j 1)))
      (when (and (< len 0) (logbitp 6 seq)) (setq len (+ base 13))))
    len))

(defun %cfc-find (fat data spc dir args s e)
  "Look up ARGS[S, E) in the directory at cluster DIR: (values cluster size dirp), or NIL."
  (let ((c dir) (hops 0) (lfn-len -1) (found nil) (fcl 0) (fsz 0) (fdir nil) (done nil))
    (loop
      (when (or done found (< c 2) (>= c #x0FFFFFF8) (> hops 65536)) (return nil))
      (dotimes (si spc)
        (unless (or done found)
          (if (not (%cfc-read-sector (+ data (* (- c 2) spc) si) (%cfc-scr)))
              (setq done t)
              (dotimes (k 16)
                (unless (or done found)
                  (let* ((ent (+ (%cfc-scr) (* 32 k))) (b0 (mem-ref ent :u8))
                         (attr (mem-ref (+ ent 11) :u8)))
                    (cond ((= b0 0) (setq done t))
                          ((= b0 #xE5) (setq lfn-len -1))
                          ((= attr #x0F)
                           (let ((l (%cfc-lfn-piece ent))) (when (>= l 0) (setq lfn-len l))))
                          ((logtest attr 8) (setq lfn-len -1))          ; volume label
                          (t
                           (let ((n (if (> lfn-len 0) lfn-len (%cfc-short-name ent))))
                             (when (or (%cfc-name-matches args s e n)
                                       ;; a long name that differs: its 8.3 alias may match
                                       (and (> lfn-len 0)
                                            (%cfc-name-matches args s e (%cfc-short-name ent))))
                               (setq found t
                                     fcl (+ (%cfc-le16 (+ ent 26)) (* 65536 (%cfc-le16 (+ ent 20))))
                                     fsz (%cfc-le32 (+ ent 28))
                                     fdir (logtest attr #x10))))
                           (setq lfn-len -1)))))))))
      (setq c (%cfc-next-cluster fat c) hops (+ hops 1)))
    (if found (values fcl fsz fdir) nil)))

;; READ_MULTIPLE_BLOCK (CMD18) with the controller's own CMD12 at the end: index 18,
;; 48-bit response with CRC and index checks, data, read, multi-block, block count, auto
;; CMD12.  One command per run of contiguous clusters instead of one per sector: CMD17 a
;; sector at a time paid the card's access latency 85,000 times for a 44 MB core (41 s on a
;; Zero).
(defun %cfc-read-blocks (lba n dst)
  "N sectors from LBA to raw memory DST with one CMD18.  T, or NIL."
  (%cfc-set #x04 (logior (ash n 16) 512))
  (if (< (%cfc-cmd #x123A0036 lba) 0)
      nil
      (let ((ok t) (p dst))
        (dotimes (b n)
          (when ok
            (if (%cfc-wait-irpt (ash 1 5))
                (progn
                  (dotimes (k 128) (setf (mem-ref (+ p (* 4 k)) :u32) (%cfc-reg #x20)))
                  (setq p (+ p 512)))
                (setq ok nil))))
        (and ok (%cfc-wait-irpt 2)))))

(defun %cfc-read-singly (lba n dst)
  "N sectors from LBA to DST one CMD17 at a time, after resetting the data line a failed
   CMD18 may have left busy.  T, or NIL."
  (%cfc-set #x2C (logior (%cfc-reg #x2C) (ash 1 26)))          ; SRST_DATA
  (%cfc-wait #x2C (ash 1 26) 0 200000)
  (%cfc-set #x30 #xFFFFFFFF)
  (%cfc-say "core: a multi-block read failed, reading sector by sector from" lba)
  (let ((ok t))
    (dotimes (i n)
      (when (and ok (not (%cfc-read-sector (+ lba i) (+ dst (* 512 i)))))
        (setq ok nil)))
    ok))

(defun %cfc-read-file (fat data spc cl size dst limit)
  "Read SIZE bytes of the chain starting at cluster CL to DST, a run of contiguous
   clusters per command.  T, or NIL."
  (if (> (+ dst size) limit)
      (%cfc-say "core: does not fit below the top of RAM, bytes" size)
      (let ((left size) (p dst) (c cl) (hops 0) (ok t))
        (loop
          (when (or (not ok) (<= left 0)) (return ok))
          (when (or (< c 2) (>= c #x0FFFFFF8) (> hops 1000000))
            (return (%cfc-say "core: the file's cluster chain ends early, bytes left" left)))
          ;; the run: C, C+1, ... while the chain is contiguous, it is still short of LEFT,
          ;; and one more cluster stays within 32768 sectors (CMD18 counts to 65535)
          (let ((first c) (k 1) (nxt (%cfc-next-cluster fat c)))
            (loop
              (when (or (/= nxt (+ first k))
                        (>= (* k spc 512) left)
                        (> (* (+ k 1) spc) 32768))
                (return nil))
              (setq k (+ k 1) nxt (%cfc-next-cluster fat nxt)))
            (let ((blocks (min (* k spc) (floor (+ left 511) 512)))
                  (lba (+ data (* (- first 2) spc))))
              (if (or (%cfc-read-blocks lba blocks p) (%cfc-read-singly lba blocks p))
                  (setq p (+ p (* 512 blocks)) left (- left (* 512 blocks)))
                  (setq ok (%cfc-say "core: a sector read failed at cluster" first))))
            (setq c nxt hops (+ hops k)))))))

;;; ---- CORE=P:/path --------------------------------------------------------------------------
(defun %cfc-core-arg (args)
  "The index in the bootargs string ARGS just past `CORE=', or NIL."
  (let ((n (length args)) (i 0) (at nil))
    (loop
      (when (or at (> (+ i 5) n)) (return at))
      (when (and (or (= i 0) (<= (%prim-aref args (- i 1)) 32))
                 (= (%prim-aref args i) 67) (= (%prim-aref args (+ i 1)) 79)        ; C O
                 (= (%prim-aref args (+ i 2)) 82) (= (%prim-aref args (+ i 3)) 69)  ; R E
                 (= (%prim-aref args (+ i 4)) 61))                                  ; =
        (setq at (+ i 5)))
      (setq i (+ i 1)))))

(defun %cfc-load-core ()
  "Read the core CORE= names to %CORE-ADDR.  T when a file was read (the caller still checks
   its magic), NIL with a CFC line otherwise.  No CORE= bootarg: NIL, silently."
  (let* ((base (%fdt-base))
         (args (and (%fdt-plausible-p base) (%fdt-bootargs-at base)))
         (at (and args (%cfc-core-arg args))))
    (when at
      (let* ((n (length args))
             (end (let ((j at)) (loop (when (or (>= j n) (<= (%prim-aref args j) 32)) (return j))   ; space, NUL, newline
                                      (setq j (+ j 1)))))
             (part (if (< at n) (- (%prim-aref args at) 48) -1)))
        (cond
          ((or (< part 1) (> part 4) (>= (+ at 2) end) (/= (%prim-aref args (+ at 1)) 58)
               (/= (%prim-aref args (+ at 2)) 47))
           (%cfc-say "core: CORE= wants P:/path (P a partition 1-4), length" (- end at)))
          ((not (%cfc-sd-init)) nil)
          (t
           (setf (mem-ref (%cfc-fatlba-slot) :u32) 0)
           (let ((t0 (mem-ref #x3F003004 :u32)))
             (if (not (%cfc-read-sector 0 (%cfc-scr)))
                 (%cfc-say "core: the MBR read failed" nil)
                 (let* ((pe (+ (%cfc-scr) #x1BE (* 16 (- part 1))))
                        (ptype (mem-ref (+ pe 4) :u8))
                        (plba (%cfc-le32 (+ pe 8))))
                   (cond
                     ((not (and (= (mem-ref (+ (%cfc-scr) 510) :u8) #x55)
                                (= (mem-ref (+ (%cfc-scr) 511) :u8) #xAA)))
                      (%cfc-say "core: no MBR on the card" nil))
                     ((not (or (= ptype #x0B) (= ptype #x0C)))
                      (%cfc-say "core: the partition is not FAT32, type" ptype))
                     ((not (%cfc-read-sector plba (%cfc-scr)))
                      (%cfc-say "core: the boot sector read failed" plba))
                     ((/= (%cfc-le16 (+ (%cfc-scr) 11)) 512)
                      (%cfc-say "core: sectors are not 512 bytes" nil))
                     (t
                      (let* ((spc (mem-ref (+ (%cfc-scr) 13) :u8))
                             (rsv (%cfc-le16 (+ (%cfc-scr) 14)))
                             (nfat (mem-ref (+ (%cfc-scr) 16) :u8))
                             (fsz (%cfc-le32 (+ (%cfc-scr) 36)))
                             (root (%cfc-le32 (+ (%cfc-scr) 44)))
                             (fat (+ plba rsv))
                             (data (+ fat (* nfat fsz)))
                             (top (let ((r (%rpi-arm-ram-top))) (if (> r (%core-addr)) r #x1C000000))))
                        (if (or (< spc 1) (> spc 128) (< nfat 1) (> nfat 2) (< fsz 1))
                            (%cfc-say "core: the boot sector does not look like FAT32" nil)
                            ;; walk the path one component at a time
                            (let ((c root) (s (+ at 3)) (size 0) (dirp t) (ok t))
                              (loop
                                (when (or (not ok) (>= s end)) (return nil))
                                (let ((e (let ((j s)) (loop (when (or (>= j end) (= (%prim-aref args j) 47))
                                                              (return j))
                                                            (setq j (+ j 1))))))
                                  (if (not dirp)
                                      (setq ok (%cfc-say "core: a path component is a file, at" s))
                                      (multiple-value-bind (cl sz dp) (%cfc-find fat data spc c args s e)
                                        (if (null cl)
                                            (setq ok (%cfc-say "core: not found, path offset" (- s at)))
                                            (setq c cl size sz dirp dp))))
                                  (setq s (+ e 1))))
                              (cond
                                ((not ok) nil)
                                (dirp (%cfc-say "core: the path names a directory" nil))
                                ((< size 4096) (%cfc-say "core: the file is too small, bytes" size))
                                ((%cfc-read-file fat data spc c size (%core-addr) top)
                                 (%cfc-say "core read, bytes" size)
                                 (%cfc-say "core read, ms"
                                           (floor (logand (- (mem-ref #x3F003004 :u32) t0) #xFFFFFFFF) 1000))
                                 t)
                                (t nil))))))))))))))))

;;; A core at 0x18000000 that THIS kernel wrote wins; without one, the card's.  "This kernel"
;;; matters on a tryboot: config.txt's common lines still have the firmware place slot A's
;;; core there (its filters only add settings), and A's core would be refused by the restore
;;; (a core holds addresses into the binary that saved it) -- so a foreign build id counts as
;;; no core, and CORE= is read instead.  The id is header word 16, written through the same
;;; halving :u64 pair the magic is.
(defun %cfc-own-core-p ()
  (and (eq (mem-ref (%core-addr) :u64) (%core-magic))
       (eq (mem-ref (+ (%core-addr) 128) :u64) (%core-build-id))))

(defun %core-requested-p ()
  (or (%cfc-own-core-p)
      (progn
        (when (eq (mem-ref (%core-addr) :u64) (%core-magic))
          (%cfc-say "core: the core in RAM is another kernel's" nil))
        (and (%cfc-load-core)
             (or (%cfc-own-core-p)
                 (%cfc-say "core: the file is not a core this kernel wrote" nil))))))

;;; ---- rebooting, and rebooting once into B --------------------------------------------------
(defun reboot (&key tryboot)
  "Reset the board through the watchdog.  With TRYBOOT, ask the firmware first to read the
   [tryboot] section of config.txt for this one boot (SET_REBOOT_FLAGS = 1, as Linux's
   `reboot \"0 tryboot\"'); the boot after that is the normal one again."
  (when tryboot
    (let ((r (%cfc-mbox #x00038064 1 0)))
      (when (< r 0) (%cfc-say "reboot: the firmware did not take the tryboot flag" nil))))
  (let ((pm #x3F100000) (pw #x5A000000))
    (setf (mem-ref (+ pm #x24) :u32) (logior pw 10))                       ; WDOG: 10 ticks
    (setf (mem-ref (+ pm #x1C) :u32)
          (logior pw (logand (mem-ref (+ pm #x1C) :u32) #xFFFFFFCF) #x20))   ; RSTC: full reset
    (loop)))
