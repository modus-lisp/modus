;;;; mac68k-core.lisp -- a Mac boot disk that starts from a SAVED CORE.
;;;;
;;;;   sbcl --script scripts/mac68k-core.lisp IMAGE DISK
;;;;
;;;; IMAGE is a bare-Mac image built with a static heap (MODUS_MAC68K_MACHINE
;;;; classic, floppy, quadra, ...).  A small Mac cannot boot it from scratch --
;;;; boot wants a few MB of heap and tens of MB of allocation -- so this boots it
;;;; ONCE on a big Mac (Basilisk II, 32 MB; MAC68K_BASILISK / MAC68K_ROM as the
;;;; ladder's) with the core descriptor's SAVE flag set.  That boot runs on its
;;;; :BOOT- heap, freezes, and sends its core on serial port A
;;;; (build-cl-repl-common.lisp %MAC-SEND-CORE): "MCOR", the low block's
;;;; address, the static objects' and conses' ranges, then the low block, the
;;;; objects and the conses.
;;;;
;;;; The static heap is then COLLECTED and MOVED to the layout's static area
;;;; (boot-mac68k.lisp :STATIC-LO), objects first, conses after them, as a
;;;; copying collection would move it: every word that is a pointer into it --
;;;; tag 1 (cons) or 7 (object), in a pointer-bearing object, a cons, or the low
;;;; block -- is rewritten.  Leaf objects (strings, byte vectors, numbers) are
;;;; not scanned; neither are the low block's byte tables (the cards, the
;;;; crossing table, the console's font and state).
;;;;
;;;; DISK: the boot blocks read [image, low block end) in one go -- the resident
;;;; image, the moved static heap, the low block at :REGION -- and the stream
;;;; section, the cold code and the cold name tables follow on the disk.  The
;;;; core descriptor's CORE flag is set.
;;;;
;;;; It replaced mac68k-core.py, byte for byte (2026-11-21).  Knobs, all
;;;; MAC68K_*: CORE_TIMEOUT (s, default 900), RAM_MB (32), JIT, CORE_NORELOC=1
;;;; (bisect: leave the heap where the run had it), CORE_NOGC=1 (keep every
;;;; object), CORE_CENSUS=1, CORE_KEEP_INIT=1, CORE_STATIC_LO=ADDR, CORE_RAW=FILE
;;;; (the boot read alone, for a floppy that carries it compressed), and
;;;; CORE_FROM=FILE: a core captured before (DISK.core) instead of booting.

(defvar cl-user::*mac68k-disk-library* t)
(load (merge-pathnames "mac68k-disk.lisp" *load-truename*))
(defpackage #:mac68k-core (:use #:cl #:mac68k-tools))
(in-package #:mac68k-core)

(defun machine () (string-downcase (env "MODUS_MAC68K_MACHINE" "classic")))

(defparameter *layouts*
  ;; per machine (boot/boot-mac68k.lisp's layouts): the image, the static
  ;; heap's nominal range, the runtime-data region, and the low block's size
  ;; (the layout's :ALLOC-START: the core run sends that much; its last 8 KB is
  ;; the static heap's card crossing table)
  '(("classic"   #x00040000 #x00110000 #x002B0000 #x002B0000 #x20000)
    ("classicos" #x00040000 #x00110000 #x00240000 #x002B0000 #x20000) ; JIT area above
    ("quadra"    #x00600000 #x009D0000 #x00A30000 #x00A80000 #x20000) ; cache, stack, NIL's page between
    ("floppy"    #x00040000 #x00440000 #x00500000 #x00540000 #x20000) ; 8 MB, cold code on the floppy
    ("classic2m" #x00040000 #x000D5000 #x00178000 #x00178000 #xE000)
    ("plus1m"    #x00040000 #x0008E000 #x000D1000 #x000D1000 #xE000)
    ("plus1mjit" #x0003C000 #x0008E000 #x000D1000 #x000D1000 #xE000)))

(destructuring-bind (load static-lo static-hi region low-block)
    (or (rest (assoc (machine) *layouts* :test #'string=))
        (error "mac68k-core: no layout for MODUS_MAC68K_MACHINE=~A" (machine)))
  ;; MODUS_MAC68K_SHIFT (boot-mac68k): the whole map moved; LOW-BLOCK is a size
  (let ((shift (env-hex "MODUS_MAC68K_SHIFT")))
    (defparameter *load* (+ load shift))
    (defparameter *static-lo* (+ static-lo shift))
    (defparameter *static-hi* (+ static-hi shift))
    (defparameter *region* (+ region shift))
    (defparameter *low-block* low-block)
    ;; a map that can run on top of Mac OS (boot-mac68k's :CLASSIC-OS; launched
    ;; by mac68k-chain's application): the RAM it needs.  The boot blocks'
    ;; record then names the static block's #x6050, the open file's refnum.
    (defparameter *chain-memtop*
      (cond ((string= (machine) "classicos") #x00400000)
            ((string= (machine) "quadra") #x00800000)
            (t 0)))
    ;; the layout's kind map when its young heap is last in the map
    ;; (boot-mac68k :QUADRA): the application's loader sizes the heap from it
    (defparameter *kmap* (if (string= (machine) "quadra") (+ #x00EB0000 shift) 0))
    ;; where the layout LINKS the init-only text (boot-mac68k :INIT-BASE,
    ;; cross.lisp *POOL-INIT-BASE*), when not where it lies in the image
    (defparameter *init-base*
      (and (member (machine) '("plus1m" "plus1mjit") :test #'string=) #x00400000))))

(defparameter *leaf* '(#x10 #x11 #x12 #x13 #x14 #x16 #x60 #x64 #x65 #x66))

(defparameter *low-bytes*
  ;; low-block byte tables, offsets from the region (not scanned for pointers)
  (list '(#x6000 . #x6100)          ; the static block's raw words (set by hand)
        '(#x6100 . #x7F00)          ; card marks
        '(#x9000 . #x9800)          ; serial parameter blocks, console state + font
        '(#xA000 . #xC000)          ; the console's text buffer
        (cons (- *low-block* #x2000) *low-block*)))     ; card crossing table

(defun die (fmt &rest args)
  (format *error-output* "~&mac68k-core: ~?~%" fmt args)
  (sb-ext:exit :code 1 :abort t))

(defun u32 (v i) (fs-apple:u32-ref v i))
(defun put32 (v i x) (fs-apple:set-u32 v i (logand x #xFFFFFFFF)))
(defun bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun octets (s) (map '(vector (unsigned-byte 8)) #'char-code s))
(defun find-bytes (needle v &key (start 0) end)
  (search (octets needle) v :start2 start :end2 end))
(defun cat (&rest vs)
  (let ((out (bytes (reduce #'+ vs :key #'length))) (at 0))
    (dolist (v vs out) (replace out v :start1 at) (incf at (length v)))))
(defun pad-512 (n) (mod (- n) 512))
(defun latin1 (v) (map 'string #'code-char v))

(defun set-flag (img which value)
  "The core descriptor 'modusMCR': +8 CORE, +12 SAVE."
  (let ((k (find-bytes "modusMCR" img)))
    (when (or (null k) (find-bytes "modusMCR" img :start (1+ k)))
      (die "no unique modusMCR descriptor (a Classic build?)"))
    (put32 img (+ k (ecase which (:core 8) (:save 12))) value)))

;;; ---- the core run: Basilisk II, its serial port A (mac68k-tools BASILISK-SERIAL) --

(defun capture-core (disk timeout)
  "Boot DISK under Basilisk II and read the core it sends: (values what it said
   before the core, the core)."
  (let* ((need nil)
         (got (basilisk-serial
               disk timeout
               (lambda (got)
                 (let ((k (find-bytes "MCOR" got)))
                   (when (and k (null need) (>= (length got) (+ k 24)))
                     (let ((olo (u32 got (+ k 8))) (ohi (u32 got (+ k 12)))
                           (clo (u32 got (+ k 16))) (chi (u32 got (+ k 20))))
                       (setq need (+ k 24 *low-block* (- ohi olo) (- chi clo)))
                       (format t "mac68k-core: core coming: low block #~(~x~), objects #~(~x~)..#~(~x~), conses #~(~x~)..#~(~x~)~%"
                               (u32 got (+ k 4)) olo ohi clo chi)
                       (finish-output))))
                 (and need (>= (length got) need)))
               :ram-mb (parse-integer (env "MAC68K_RAM_MB" "32")))))
    (when (or (null need) (< (length got) need))
      (write-string (latin1 (subseq got (max 0 (- (length got) 2000)))) *error-output*)
      (die "no complete core on serial (~D bytes)" (length got)))
    (let ((k (find-bytes "MCOR" got)))
      (values (subseq got 0 k) (subseq got k need)))))

;;; ---- the cold function records, as pages --------------------------------------

(defun bytes< (a b)
  (let ((m (mismatch a b)))
    (cond ((null m) nil)
          ((>= m (length a)) t)
          ((>= m (length b)) nil)
          (t (< (aref a m) (aref b m))))))

(defun cold-fn-pages (recs)
  "The cold function records (hash.l value.l flags.b len.b name, even) as index
   pages and record pages: page 0.. the count of record pages, then each's first
   hash; a run of one hash never straddles two pages.  (values the bytes, the
   record count, the page count)."
  (let ((rows nil) (i 0))
    (loop while (< i (length recs))
          do (let ((n (+ 10 (aref recs (+ i 9)))))
               (incf n (logand n 1))
               (push (cons (u32 recs i) (subseq recs i (+ i n))) rows)
               (incf i n)))
    (setq rows (coerce (sort (nreverse rows)
                             (lambda (a b) (or (< (car a) (car b))
                                               (and (= (car a) (car b)) (bytes< (cdr a) (cdr b))))))
                       'vector))
    (let ((pages nil) (firsts nil) (cur nil) (k 0))
      (flet ((close-page ()
               (let ((page (apply #'cat (reverse cur))))
                 (push (cat page (bytes (- 512 (length page)))) pages)
                 (setq cur nil))))
        (loop while (< k (length rows))
              do (let* ((h (car (aref rows k)))
                        (run (loop for j from k below (min (length rows) (+ k 64))
                                   when (= (car (aref rows j)) h) collect (aref rows j)))
                        (size (reduce #'+ run :key (lambda (r) (length (cdr r))))))
                   (when (> size 512)
                     (die "~D cold functions share hash #~(~x~)" (length run) h))
                   (when (> (+ (reduce #'+ cur :key #'length) size) 512) (close-page))
                   (unless cur (push h firsts))
                   (dolist (r run) (push (cdr r) cur))
                   (incf k (length run))))
        (when cur (close-page)))
      (let* ((pages (nreverse pages)) (firsts (nreverse firsts))
             (index (bytes (* 4 (1+ (length firsts))))))
        (put32 index 0 (length pages))
        (loop for f in firsts for j from 4 by 4 do (put32 index j f))
        (values (cat index (bytes (pad-512 (length index))) (apply #'cat pages))
                (length rows) (length pages))))))

;;; ---- the core's own collection, and the move ----------------------------------

(defvar *cold-fns* nil "The cold function records the core run left at #x6048.")

(defun relocate (core &optional static-lo)
  "(values the low block, the objects' new base, the objects, the conses' new
   base, the conses), the static heap collected and moved."
  (let* ((base (u32 core 4)) (olo (u32 core 8)) (ohi (u32 core 12))
         (clo (u32 core 16)) (chi (u32 core 20)))
    (unless (= base *region*)
      (die "the low block is at #~(~x~), the layout says #~(~x~)" base *region*))
    (let* ((pos 24)
           (low (subseq core pos (incf pos *low-block*)))
           (objs (subseq core pos (incf pos (- ohi olo))))
           (conses (subseq core pos (+ pos (- chi clo))))
           (nolo (or static-lo *static-lo*)))
      (when (equal (env "MAC68K_CORE_NORELOC") "1")
        ;; THE BISECT: the static heap left where the core run had it (a 32 MB
        ;; Mac only), so a failure is the core itself, not the move
        (format t "mac68k-core: NOT relocating (MAC68K_CORE_NORELOC=1)~%")
        (put32 low #x601C 0) (put32 low #x6014 1)
        (put32 low #x6030 (if (equal (env "MODUS_MAC_CORE_PROBES") "1") 1 0))
        (return-from relocate (values low olo objs clo conses)))
      ;; COLD FUNCTIONS (mvm/mac68k-cold.lisp %COLD-FNS-CUT): the freeze left the
      ;; function table's image entries as records in a byte vector, its raw
      ;; word at #x6048.  Take the bytes; nothing else points at the vector, so
      ;; the collection below drops it.
      (let ((w (u32 low #x6048)))
        (put32 low #x6048 0) (put32 low #x604C 0)
        (unless (zerop w)
          (let* ((a (- w 7)) (h (and (<= olo a) (< a ohi) (u32 objs (- a olo)))))
            (when (or (/= (logand w 7) 7) (null h) (/= (logand h #xFF) #x11))
              (die "#x6048 is not a byte vector (#~8,'0x)" w))
            (setq *cold-fns* (subseq objs (+ (- a olo) 4) (+ (- a olo) 4 (ash h -8)))))))
      ;; THE CORE'S OWN COLLECTION.  A core boot's only roots are the low block's
      ;; words (the stack starts fresh), so what they do not reach is garbage the
      ;; freeze kept -- a stale word on the core run's stack or in a register
      ;; (measured: a dropped 12,250-name table, 326 KB).  Mark from the low block
      ;; as the collector scans (LEAF payloads are not pointers), then copy the
      ;; marked objects and conses down in their order.  MAC68K_CORE_NOGC=1 keeps
      ;; everything (the old uniform move).
      (let ((ostarts (make-array 0 :adjustable t :fill-pointer 0))
            (osize (make-hash-table)) (otype (make-hash-table)))
        (let ((i 0))
          (loop while (< i (length objs))
                do (let* ((h (u32 objs i)) (st (logand h #xFF)) (n (ash h -8))
                          (size (logand (+ (if (member st '(#x11 #x13)) (+ n 4) (* (1+ n) 4)) 7)
                                        (lognot 7))))
                     (when (or (<= size 0) (> (+ i size) (length objs)))
                       (die "a bad object header #~8,'0x at #~(~x~)" h (+ olo i)))
                     (vector-push-extend (+ olo i) ostarts)
                     (setf (gethash (+ olo i) osize) size (gethash (+ olo i) otype) (cons st n))
                     (incf i size))))
        (labels ((skip-p (off) (some (lambda (r) (and (<= (car r) off) (< off (cdr r)))) *low-bytes*))
                 (node-of (w)
                   ;; the object start or cons cell a tagged word points into, or NIL
                   (let ((tg (logand w 7)))
                     (cond ((= tg 7)
                            (let ((a (- w 7)))
                              (when (and (<= olo a) (< a ohi))
                                ;; bisect_right - 1: the last start <= A
                                (let ((lo 0) (hi (length ostarts)))
                                  (loop while (< lo hi)
                                        do (let ((mid (floor (+ lo hi) 2)))
                                             (if (< a (aref ostarts mid)) (setq hi mid) (setq lo (1+ mid)))))
                                  (and (> lo 0) (aref ostarts (1- lo)))))))
                           ((= tg 1)
                            (let ((a (- w 1)))
                              (when (and (<= clo a) (< a chi)) (logand a (lognot 7)))))))))
          ;; MARKED in the order a Cheney collection would copy: breadth first
          ;; from the low block's words in address order, each node's slots in
          ;; order.  So the static heap's layout is a function of what is
          ;; reachable alone -- not of the core run's allocation history, which a
          ;; conservative stack scan makes vary between runs of one build.  Two
          ;; core runs a shift apart must lay out alike: mac68k-reloc-disk diffs them.
          (let ((marked (make-hash-table)) (order (make-array 0 :adjustable t :fill-pointer 0))
                (work (make-array 0 :adjustable t :fill-pointer 0)) (head 0))
            (flet ((mark-word (w)
                     (let ((nd (node-of w)))
                       (when (and nd (not (gethash nd marked)))
                         (setf (gethash nd marked) t)
                         (vector-push-extend nd work) (vector-push-extend nd order)))))
              (if (equal (env "MAC68K_CORE_NOGC") "1")
                  (progn
                    (loop for a across ostarts do (setf (gethash a marked) t) (vector-push-extend a order))
                    (loop for a from clo below chi by 8 do (setf (gethash a marked) t) (vector-push-extend a order)))
                  (progn
                    (loop for j from 0 below *low-block* by 4
                          unless (skip-p j) do (mark-word (u32 low j)))
                    (loop while (< head (length work))
                          do (let ((nd (aref work head)))
                               (incf head)
                               (if (gethash nd osize)
                                   (destructuring-bind (st . n) (gethash nd otype)
                                     (unless (or (member st *leaf*) (zerop n))
                                       (loop for j from (+ (- nd olo) 4) by 4 repeat n
                                             do (mark-word (u32 objs j)))))
                                   (progn (mark-word (u32 conses (- nd clo)))
                                          (mark-word (u32 conses (+ (- nd clo) 4))))))))))
            (when (env "MAC68K_CORE_CENSUS")
              ;; what the static heap holds, by subtag, and the largest objects
              (let ((cn (make-hash-table)) (cb (make-hash-table)))
                (loop for a across ostarts when (gethash a marked)
                      do (let ((st (car (gethash a otype))))
                           (incf (gethash st cn 0)) (incf (gethash st cb 0) (gethash a osize))))
                (dolist (e (sort (loop for st being the hash-keys of cb using (hash-value b) collect (cons st b))
                                 #'> :key #'cdr))
                  (format t "mac68k-core: census subtag #x~(~2,'0x~): ~6D objects ~8D bytes~%"
                          (car e) (gethash (car e) cn) (cdr e)))
                (dolist (e (subseq (sort (loop for a across ostarts when (gethash a marked)
                                               collect (cons (gethash a osize) a))
                                         #'> :key #'car)
                                   0 (min 25 (length ostarts))))
                  (format t "mac68k-core: census big #x~(~x~) subtag #x~(~2,'0x~) ~D bytes~%"
                          (cdr e) (car (gethash (cdr e) otype)) (car e)))))
            ;; the new places: marked objects in order from NOLO, conses from NCLO
            (let ((omap (make-hash-table)) (cmap (make-hash-table)) (nob-len 0) (ncs-len 0))
              (loop for a across order when (gethash a osize)
                    do (setf (gethash a omap) (+ nolo nob-len)) (incf nob-len (gethash a osize)))
              (let* ((nclo (logand (+ nolo nob-len 4095) (lognot 4095)))
                     (nob (bytes nob-len)) (ncs nil) (moved 0) (nobj 0))
                (loop for a across order
                      do (if (gethash a osize)
                             (replace nob objs :start1 (- (gethash a omap) nolo) :start2 (- a olo)
                                               :end2 (+ (- a olo) (gethash a osize)))
                             (progn (setf (gethash a cmap) (+ nclo ncs-len)) (incf ncs-len 8))))
                (setq ncs (bytes ncs-len))
                (loop for a across order unless (gethash a osize)
                      do (replace ncs conses :start1 (- (gethash a cmap) nclo) :start2 (- a clo) :end2 (+ (- a clo) 8)))
                (let ((nchi (+ nclo ncs-len)))
                  (when (> nchi *static-hi*)
                    (die "the static heap (~D bytes) does not fit #~(~x~)..#~(~x~)"
                         (- nchi nolo) nolo *static-hi*))
                  (flet ((rel (buf off)
                           (let* ((w (u32 buf off)) (nd (node-of w)))
                             (when nd
                               (let ((new (or (gethash nd omap) (gethash nd cmap))))
                                 (unless new (die "a pointer to an unmarked node #~(~x~)" nd))
                                 (put32 buf off (+ (- w nd) new))
                                 (incf moved))))))
                    (loop for a across order
                          do (let ((new (gethash a omap)))
                               (when new
                                 (incf nobj)
                                 (destructuring-bind (st . n) (gethash a otype)
                                   (unless (member st *leaf*)
                                     (loop for j from (+ (- new nolo) 4) by 4 repeat n do (rel nob j)))))))
                    (loop for j from 0 below ncs-len by 4 do (rel ncs j))
                    (loop for j from 0 below *low-block* by 4 unless (skip-p j) do (rel low j)))
                  (format t "mac68k-core: the core's collection kept ~D of ~D objects (~D of ~D bytes), ~D of ~D bytes of conses~%"
                          nobj (length ostarts) nob-len (length objs) ncs-len (length conses))
                  ;; the static block: the new ranges; cards off until %GC-CARDS-INIT
                  (loop for (off v) in (list (list #x6004 nolo) (list #x6008 (+ nolo nob-len))
                                             (list #x600C nclo) (list #x6010 nchi)
                                             (list #x6018 0) (list #x601C 0) (list #x6014 1)
                                             (list #x6030 (if (equal (env "MODUS_MAC_CORE_PROBES") "1") 1 0)))
                        do (put32 low off v))
                  ;; the cards' tables (the marks, the crossing table): %GC-CARDS-INIT
                  ;; rebuilds both at a core boot, and the core run's describe its
                  ;; own layout -- which two runs a shift apart do not share
                  (fill low 0 :start #x6100 :end #x7F00)
                  (fill low 0 :start (- *low-block* #x2000) :end *low-block*)
                  ;; a new collection count: anything keyed by address re-hashes.
                  ;; A fixed one, not the run's + 1: two core runs a shift apart
                  ;; need not collect equally often, and the relocating pair must agree
                  (let ((c (u32 low #x60))) (put32 low #x60 (if (< c #x4000) #x4000 (1+ c))))
                  ;; the semispaces the run ended in: every boot from the core sets
                  ;; them anew (boot-mac68k EMIT-MAC68K-HEAP / -HEAP-FROM-SLOT)
                  (fill low 0 :start #x40 :end #x58)
                  (format t "mac68k-core: ~D objects (~D bytes) -> #~(~x~), ~D bytes of conses -> #~(~x~); ~D pointers moved~%"
                          nobj nob-len nolo ncs-len nclo moved)
                  (values low nolo nob nclo ncs))))))))))

;;; ---- the disk ------------------------------------------------------------------

(defun main (image disk)
  (let* ((img0 (read-bytes image)) img resident count (cold-desc nil) (cold-n 0))
    (if (find-bytes "modusPGD" img0)
        (let ((mac68k-disk:*load-addr* *load*))
          (multiple-value-setq (img resident count) (mac68k-disk:paged img0)))
        ;; unpaged (MODUS_68K_PAGED=0): all resident
        (setq img (copy-seq img0) resident (length img0) count 0))
    ;; COLD NATIVE CODE (translate-68k M68K-EMIT-COLD-ENTER): the descriptor
    ;; 'modusCLD' (+8 BIAS, +12 the cold section's address, +16 the count); the
    ;; section is the image's tail, never read at boot -- not even by the core
    ;; run, whose disk has the image at byte 1024 as the build's BIAS assumes
    (setq cold-desc (find-bytes "modusCLD" img :end resident))
    (when cold-desc
      (let ((cold-at (u32 img (+ cold-desc 12))))
        (setq cold-n (u32 img (+ cold-desc 16)) resident (- cold-at *load*))
        ;; the boot tail ends there; the core run's scratch and I/O pages, NIL's
        ;; page and low block start 128 KB under the low block (boot-mac68k)
        (when (> cold-at (- *region* #x20000))
          (die "the boot tail (to #~(~x~)) runs into the pages under the low block (#~(~x~))"
               cold-at (- *region* #x20000)))
        (format t "mac68k-core: ~D cold functions, ~D bytes from #~(~x~), on the disk after the boot read~%"
                cold-n (- (length img) resident) cold-at)))
    ;; what must stay resident: up to the init-only text's marker, when there is
    ;; one (the static heap is laid over that text; see below)
    (let ((kept (or (find-bytes "modusINI" img :end resident) resident)))
      (when (> (+ *load* kept) *static-lo*)
        (die "the resident image (~D bytes) runs into the static heap" kept)))
    ;; 1. the core run: the image as mac68k-disk --paged writes it, SAVE set
    (let ((run (copy-seq img)) (run-disk (concatenate 'string disk ".corerun")))
      (set-flag run :save 1)
      (if *init-base*
          ;; the init-only text is linked at INIT-BASE: the run reads the
          ;; resident part up to its marker, a gap, then the text at INIT-BASE;
          ;; the streams follow on the disk, and the descriptor's disk bias says so
          (let ((k (find-bytes "modusINI" run :end resident)))
            (when (or (null k) (> (+ *load* k) *init-base*))
              (die "no init-only text to put at #~(~x~)" *init-base*))
            (let* ((body (cat (subseq run 0 k) (bytes (- *init-base* *load* k)) (subseq run k resident)))
                   (body (cat body (bytes (pad-512 (length body)))))
                   (d (find-bytes "modusPGD" body)))
              (put32 body (+ d 20) (- (u32 body (+ d 16)) (+ 1024 (length body))))
              (mac68k-disk:write-disk (cat body (subseq run resident)) run-disk
                                      :load-addr *load* :read-len (length body))
              (format t "mac68k-core: the core run reads ~D bytes, the init-only text at #~(~x~)~%"
                      (length body) *init-base*)))
          (mac68k-disk:write-disk run run-disk :load-addr *load* :read-len resident))
      (finish-output)
      (multiple-value-bind (console core)
          (let ((from (env "MAC68K_CORE_FROM")))
            (if from
                (progn (format t "mac68k-core: the core from ~A (MAC68K_CORE_FROM), no core run~%" from)
                       (values (bytes 0) (read-bytes from)))
                (let ((t0 (get-internal-real-time)))
                  (multiple-value-bind (console core)
                      (capture-core run-disk (parse-integer (env "MAC68K_CORE_TIMEOUT" "900")))
                    (format t "mac68k-core: the core run took ~D s; it said:~%~A~%"
                            (round (- (get-internal-real-time) t0) internal-time-units-per-second)
                            (let ((s (latin1 console))) (subseq s (max 0 (- (length s) 400)))))
                    (values console core)))))
        (declare (ignore console))
        (write-bytes (concatenate 'string disk ".core") core)   ; as sent, for looking into
        ;; INIT-ONLY TEXT (cross.lisp *POOL-INIT-ONLY*): the resident image ends
        ;; in literals only boot reads, after a "modusINI" marker; a core boot
        ;; never reads them, so the static heap starts there, over them
        (let ((k (find-bytes "modusINI" img :end resident)) (static-lo nil))
          (cond ((and k (equal (env "MAC68K_CORE_KEEP_INIT") "1"))
                 (format t "mac68k-core: init-only text kept (MAC68K_CORE_KEEP_INIT=1)~%"))
                (k
                 (setq static-lo (logand (+ *load* k 4095) (lognot 4095)))
                 (format t "mac68k-core: init-only text: ~D bytes at #~(~x~); the static heap starts at #~(~x~)~%"
                         (- resident k) (+ *load* k) static-lo)))
          ;; MAC68K_CORE_STATIC_LO=ADDR: the static heap at ADDR instead (Basilisk
          ;; II's LC 475 ROM uses RAM in [#xB3000, #xB9000) while the REPL runs)
          (let ((s (env "MAC68K_CORE_STATIC_LO")))
            (when s
              (setq static-lo (if (and (> (length s) 2) (string-equal (subseq s 0 2) "0x"))
                                  (parse-integer s :start 2 :radix 16) (parse-integer s)))
              (format t "mac68k-core: the static heap at #~(~x~) (MAC68K_CORE_STATIC_LO)~%" static-lo)))
          (multiple-value-bind (low nolo objs nclo conses) (relocate core static-lo)
            ;; 2. the disk: [image, low block end) in one read, the streams after it
            (let* ((top (max (+ *region* *low-block*) (+ nclo (length conses))))
                   (chunk (bytes (- top *load*))))
              (replace chunk img :end2 resident)
              ;; what the static heap does not cover of the init-only text and the
              ;; boot tail is dead once the core is saved: zeroed, so the disk
              ;; carries no stale code (and two builds a shift apart agree there)
              (when static-lo
                (let ((dead-lo (- static-lo *load*)))
                  (when (< dead-lo resident) (fill chunk 0 :start dead-lo :end resident))))
              (replace chunk objs :start1 (- nolo *load*))
              (replace chunk conses :start1 (- nclo *load*))
              (replace chunk low :start1 (- *region* *load*))
              (let* ((streams (subseq img resident))
                     (stream-disk (+ 1024 (length chunk)))   ; disk byte of the section's first
                     (k (find-bytes "modusPGD" chunk))
                     (cnt 0))
                (when k
                  (setq cnt (u32 chunk (+ k 12)))
                  (put32 chunk (+ k 20) (- (u32 chunk (+ k 16)) stream-disk)))   ; disk bias (SUB.L wraps)
                (set-flag chunk :core 1)
                (when cold-desc
                  ;; the cold section's disk byte: BIAS = its address - that
                  (put32 chunk (+ cold-desc 8) (- (+ *load* resident) stream-disk))
                  ;; what the core run loaded is gone: no cold floor, no JIT top;
                  ;; and no code area of its own (boot-mac68k :BOOT-JIT-AREA; an
                  ;; application's loader writes its partition's there at launch)
                  (fill chunk 0 :start (+ (- *region* *load*) #x6060) :end (+ (- *region* *load*) #x6078)))
                ;; 3. COLD NAMES (mvm/mac68k-cold.lisp): IMAGE.cold, when the build
                ;; wrote one, on a block boundary after the streams; the static
                ;; block's #x6044 says where
                (let ((tail streams) (cold-path (concatenate 'string image ".cold")))
                  (when (probe-file cold-path)
                    (let* ((cold (read-bytes cold-path))
                           (pad (pad-512 (+ stream-disk (length tail))))
                           (cold-disk (+ stream-disk (length tail) pad)))
                      (setq tail (cat tail (bytes pad) cold))
                      (put32 chunk (+ (- *region* *load*) #x6044) cold-disk)
                      (format t "mac68k-core: cold names: ~D bytes at disk byte ~D~%" (length cold) cold-disk)))
                  (when (and *cold-fns* (plusp (length *cold-fns*)))
                    (multiple-value-bind (region n npages) (cold-fn-pages *cold-fns*)
                      (let* ((pad (pad-512 (+ stream-disk (length tail))))
                             (fn-disk (+ stream-disk (length tail) pad)))
                        (setq tail (cat tail (bytes pad) region))
                        (put32 chunk (+ (- *region* *load*) #x604C) fn-disk)
                        (format t "mac68k-core: cold functions: ~D names in ~D pages at disk byte ~D~%"
                                n npages fn-disk))))
                  ;; MAC68K_CORE_RAW=FILE: the boot read's bytes alone, LOAD up --
                  ;; for a floppy that carries the core compressed (mac68k-floppy
                  ;; --load); only a build with no streams and no cold code.  The
                  ;; cold NAME tables come along in RAM past the static heap, with
                  ;; #x6044 / #x604C their ADDRESSES and #x6078 = 1, which
                  ;; mac68k-cold.lisp %COLD-DISK-READ reads as "copy from there".
                  (let ((raw-path (env "MAC68K_CORE_RAW")))
                    (when raw-path
                      (when (or (plusp cnt) cold-desc)
                        (die "MAC68K_CORE_RAW wants an image with no streams or cold code"))
                      (let* ((raw (copy-seq chunk)) (low-at (- *region* *load*))
                             (used (max (+ nolo (length objs)) (+ nclo (length conses))))
                             (at (logand (+ used #xFFF) (lognot #xFFF))))
                        (when (> (+ at (length tail)) *static-hi*)
                          (die "the cold tables (~D bytes) do not fit below :STATIC-HI" (length tail)))
                        (replace raw tail :start1 (- at *load*))
                        (dolist (slot '(#x6044 #x604C))
                          (let ((d (u32 raw (+ low-at slot))))
                            (put32 raw (+ low-at slot) (if (zerop d) 0 (- (+ at d) stream-disk)))))
                        (put32 raw (+ low-at #x6078) 1)
                        (write-bytes raw-path raw)
                        (format t "mac68k-core: the cold tables, ~D bytes, at #x~(~x~)~%" (length tail) at)
                        (format t "mac68k-core: raw core ~A: ~D bytes at #x~(~x~)~%" raw-path (length chunk) *load*))))
                  (mac68k-disk:write-disk (cat chunk tail) disk :load-addr *load* :read-len (length chunk)
                                          :refslot (if (plusp *chain-memtop*) (+ *region* #x6050) 0)
                                          :memtop *chain-memtop*
                                          :kmap (if (plusp *chain-memtop*) *kmap* 0))
                  (format t "mac68k-core: ~A: boot reads ~D bytes (image ~D, static ~D, low block ~D); ~D streams (~D bytes) on demand from disk byte ~D~%"
                          disk (length chunk) resident (+ (length objs) (length conses)) *low-block*
                          cnt (length streams) stream-disk))))))))))

(apply #'main (argv))
