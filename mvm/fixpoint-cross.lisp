;;; ============================================================
;;; Image buffer functions (for assembling Gen1 on bare metal)
;;; ============================================================
;;; Image buffer at 0x08000000, position counter at 0x4FF040

(defun img-init ()
  (setf (mem-ref #x4FF040 :u64) 0))

(defun img-pos ()
  (mem-ref #x4FF040 :u64))

(defun img-emit (b)
  (let ((pos (mem-ref #x4FF040 :u64)))
    (setf (mem-ref (+ #x08000000 pos) :u8) b)
    (setf (mem-ref #x4FF040 :u64) (+ pos 1))))

(defun img-emit-u32 (v)
  (img-emit (logand v 255))
  (img-emit (logand (ash v -8) 255))
  (img-emit (logand (ash v -16) 255))
  (img-emit (logand (ash v -24) 255)))

(defun img-emit-u64-raw (v)
  ;; Write tagged fixnum v as 8 LE bytes via mem-ref :u64 (raw bits)
  ;; This preserves the tagged representation for next-generation reads
  (let ((pos (mem-ref #x4FF040 :u64)))
    (setf (mem-ref (+ #x08000000 pos) :u64) v)
    (setf (mem-ref #x4FF040 :u64) (+ pos 8))))

(defun img-patch-u32 (offset v)
  (let ((base #x08000000))
    (setf (mem-ref (+ base offset) :u8) (logand v 255))
    (let ((p1 (+ offset 1)))
      (setf (mem-ref (+ base p1) :u8) (logand (ash v -8) 255)))
    (let ((p2 (+ offset 2)))
      (setf (mem-ref (+ base p2) :u8) (logand (ash v -16) 255)))
    (let ((p3 (+ offset 3)))
      (setf (mem-ref (+ base p3) :u8) (logand (ash v -24) 255)))))

(defun img-emit-boot-preamble ()
  ;; Preamble size stored at metadata +0x24 = address 0x300024
  (let ((size (td-read-u32 #x2000024))
        (i 0))
    (let ((load-addr (td-read-u32 #x2000030)))
      (loop
        (when (>= i size) (return i))
        (img-emit (mem-ref (+ load-addr i) :u8))
        (setq i (+ i 1))))))

;;; Fixpoint: compute FNV-1a of native code
(defun td-fnv-native (bytes size)
  (let ((hash 2166136261)
        (j 0))
    (loop
      (when (>= j size) (return hash))
      (let ((b (aref bytes j)))
        (setq hash (logand #xFFFFFFFF
                           (* (logxor hash b) 16777619))))
      (setq j (+ j 1)))))

;;; Fixpoint: assemble Gen1 x64 image from translated native code
;;; Writes a bootable multiboot1 image to 0x08000000
(defun td-assemble-gen1 (result bc ft)
  ;; result = (cons code-buffer fn-map)
  ;; bc = bytecode array, ft = function table list
  (let ((buf (car result))
        (fn-map (cdr result)))
    (let ((native-bytes (code-buffer-bytes buf))
          (native-size (code-buffer-position buf)))
      ;; 1. Init image buffer
      (img-init)
      (write-char-serial 71) (write-char-serial 49) ;; G1
      (write-char-serial 58) (write-char-serial 10) ;; :NL
      ;; 2. Copy boot preamble from running kernel
      (img-emit-boot-preamble)
      (write-char-serial 80) ;; P
      (print-dec (img-pos))
      (write-char-serial 10)
      ;; 3. Emit JMP rel32 to kernel-main
      ;; Kernel-main native offset from metadata +0x2C
      (let ((km-offset (td-read-u32 #x200002C)))
        (let ((jmp-pos (img-pos)))
          (img-emit #xE9)
          (img-emit-u32 km-offset)
          ;; 4. Copy native code from code-buffer
          (write-char-serial 78) ;; N
          ;; Save code-start position in diagnostic slot for metadata
          (td-write-u32 #x2000050 (img-pos))
          (let ((i 0))
            (loop
              (when (>= i native-size) (return nil))
              (img-emit (aref native-bytes i))
              (setq i (+ i 1))
              (when (zerop (mod i 50000))
                (write-char-serial 46))))
          (write-char-serial 10)
          (print-dec (img-pos))
          (write-char-serial 10)
          ;; 5. Append fixpoint data: MVM bytecode
          (write-char-serial 84) ;; T
          (let ((bc-len (array-length bc))
                (bc-img-offset (img-pos)))
            (let ((bi 0))
              (loop
                (when (>= bi bc-len) (return nil))
                (img-emit (aref bc bi))
                (setq bi (+ bi 1))))
            ;; 6. Append function table entries (12 bytes each, 3 x u32 LE)
            (let ((ft-img-offset (img-pos))
                  (rest-ft ft)
                  (ft-count 0))
              (loop
                (when (null rest-ft) (return nil))
                (let ((entry (car rest-ft)))
                  (let ((name (car entry))
                        (offset (car (cdr entry)))
                        (len (car (cdr (cdr entry)))))
                    (img-emit-u32 name)
                    (img-emit-u32 offset)
                    (img-emit-u32 len)))
                (setq rest-ft (cdr rest-ft))
                (setq ft-count (+ ft-count 1)))
              (write-char-serial 10)
              (print-dec ft-count)
              (write-char-serial 10)
              ;; 7. Write fixpoint metadata at offset 0x400000 (= VA 0x500000 for x64)
              ;; All values as u32 LE
              (let ((md-img-off #x1F00000))
                ;; magic MVMT = 0x544D564D
                (img-patch-u32 md-img-off #x544D564D)
                ;; version = 1
                (img-patch-u32 (+ md-img-off 4) 1)
                ;; my-architecture = 0 (x64)
                (img-patch-u32 (+ md-img-off 8) 0)
                ;; bytecode-offset
                (img-patch-u32 (+ md-img-off 12) bc-img-offset)
                ;; bytecode-length
                (img-patch-u32 (+ md-img-off 16) bc-len)
                ;; fn-table-offset
                (img-patch-u32 (+ md-img-off 20) ft-img-offset)
                ;; fn-table-count
                (img-patch-u32 (+ md-img-off 24) ft-count)
                ;; native-code-offset (saved in diagnostic slot)
                (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x2000050))
                ;; native-code-length
                (img-patch-u32 (+ md-img-off 32) native-size)
                ;; preamble-size (same as running kernel)
                (let ((preamble (td-read-u32 #x2000024)))
                  (img-patch-u32 (+ md-img-off 36) preamble))
                ;; kernel-main-hash-lo (copy from running kernel)
                (let ((km-hash (td-read-u32 #x2000028)))
                  (img-patch-u32 (+ md-img-off 40) km-hash))
                ;; kernel-main-native-offset
                (img-patch-u32 (+ md-img-off 44) km-offset)
                ;; image-load-addr = 0x100000 (x64)
                (img-patch-u32 (+ md-img-off 48) #x100000)
                ;; target-architecture (default: 1=aarch64, overridden by host script)
                (img-patch-u32 (+ md-img-off 52) 1)
                ;; mode (default: 0=cross-compile, overridden by host script)
                (img-patch-u32 (+ md-img-off 56) 0))
              ;; 8. Patch multiboot header: load_end_addr, bss_end_addr
              (let ((total-size (+ #x1F00000 64)))
                (let ((load-end (+ #x100000 total-size)))
                  (img-patch-u32 20 load-end)
                  (img-patch-u32 24 load-end))
                ;; Print Gen1 info
                (write-char-serial 71) (write-char-serial 49) ;; G1
                (write-char-serial 61) ;; =
                (print-dec total-size)
                (write-char-serial 10)
                total-size))))))))

;;; Fixpoint: build-image-cross entry point
;;; Build x64 target from x64 host (same-arch, existing path)
(defun td-print-bytes-at (bytes pos offset count)
  ;; Print COUNT bytes at OFFSET for comparison
  (write-char-serial 66) (write-char-serial 64) ;; B@
  (print-dec offset) (write-char-serial 58) ;; :
  (let ((i 0))
    (loop
      (when (>= i count) (return nil))
      (let ((idx (+ offset i)))
        (when (< idx pos)
          (write-char-serial 32)
          (print-dec (aref bytes idx))))
      (setq i (+ i 1))))
  (write-char-serial 10))

(defun build-x64-from-x64 (bc ft)
  (let ((result (translate-mvm-to-x64 bc ft)))
    ;; result is (cons code-buffer fn-map)
    (let ((buf (car result)))
      (let ((pos (code-buffer-position buf)))
        (print-dec pos) (write-char-serial 10)
        ;; FNV-1a of native code
        (let ((native-bytes (code-buffer-bytes buf)))
          (let ((hash (td-fnv-native native-bytes pos)))
            (write-char-serial 70) (write-char-serial 78) ;; FN
            (write-char-serial 86) (write-char-serial 58) ;; V:
            (print-dec hash) (write-char-serial 10))
          ;; Print bytes at key positions for comparison with SBCL reference
          (td-print-bytes-at native-bytes pos 0 16)
          (td-print-bytes-at native-bytes pos 1000 16)
          (td-print-bytes-at native-bytes pos 100000 16)
          (td-print-bytes-at native-bytes pos 200000 16)
          ;; XOR checksum
          (let ((xsum 0) (xi 0))
            (loop
              (when (>= xi pos) (return nil))
              (setq xsum (logxor xsum (aref native-bytes xi)))
              (setq xi (+ xi 1)))
            (write-char-serial 88) (write-char-serial 79) ;; XO
            (write-char-serial 82) (write-char-serial 58) ;; R:
            (print-dec xsum) (write-char-serial 10))
          ;; Assemble Gen1 x64 image
          (td-assemble-gen1 result bc ft))
        pos))))

;;; Build AArch64 target from x64 host (cross-arch)
(defun build-aarch64-from-x64 (bc ft)
  ;; Set AArch64 serial config for fixpoint (UART at VA 0x20000000, PL011 byte-width)
  (setq *aarch64-serial-base* #x20000000)
  (setq *aarch64-serial-width* 0)
  (setq *aarch64-serial-tx-poll* nil)
  (setq *aarch64-sched-lock-addr* nil)
  (let ((result (translate-mvm-to-aarch64 bc ft)))
    ;; result is (cons native-bytes (cons native-size fn-map))
    (let ((native-bytes (car result))
          (native-size (car (cdr result))))
      (print-dec native-size) (write-char-serial 10)
      ;; FNV-1a of AArch64 native code
      (let ((hash (td-fnv-native native-bytes native-size)))
        (write-char-serial 70) (write-char-serial 78) ;; FN
        (write-char-serial 86) (write-char-serial 58) ;; V:
        (print-dec hash) (write-char-serial 10))
      ;; Assemble Gen1 AArch64 image
      (td-assemble-gen1-aarch64 result bc ft)
      native-size)))

;;; Build x64 target from AArch64 host (cross-arch return trip)
(defun build-x64-from-aarch64 (bc ft)
  (let ((result (translate-mvm-to-x64 bc ft)))
    (let ((buf (car result)))
      (let ((pos (code-buffer-position buf)))
        (print-dec pos) (write-char-serial 10)
        (let ((native-bytes (code-buffer-bytes buf)))
          (let ((hash (td-fnv-native native-bytes pos)))
            (write-char-serial 70) (write-char-serial 78)
            (write-char-serial 86) (write-char-serial 58)
            (print-dec hash) (write-char-serial 10)))
        (td-assemble-gen1-x64 result bc ft)
        pos))))

(defun build-image-cross (target)
  ;; target: 0=x64, 1=aarch64
  ;; Step 1: Read embedded bytecode
  (write-char-serial 83) (write-char-serial 49) (write-char-serial 58)
  (let ((bc (td-read-bytecode)))
    (print-dec (array-length bc))
    (write-char-serial 10)
    ;; XOR checksum of bytecode
    (write-char-serial 88) ;; X
    (let ((xsum 0) (xi 0))
      (loop
        (when (>= xi (array-length bc)) (return nil))
        (setq xsum (logxor xsum (aref bc xi)))
        (setq xi (+ xi 1)))
      (print-dec xsum))
    (write-char-serial 10)
    ;; Step 2: Read function table
    (write-char-serial 83) (write-char-serial 50) (write-char-serial 58)
    (let ((ft (td-read-fn-table-list)))
      (print-dec (length ft))
      (write-char-serial 10)
      ;; Step 3: Translate and assemble based on target
      (write-char-serial 83) (write-char-serial 51) (write-char-serial 58)
      (write-char-serial 10)
      (let ((my-arch (td-read-u32 #x2000008)))
        ;; Dispatch: target 0=x64, 1=aarch64, 2=i386, 3=arm32
        (cond
          ((= my-arch 3)
           ;; Running on arm32 (uses i386-safe translators)
           (cond
             ((= target 3) (build-arm32-from-arm32 bc ft))
             ((= target 2) (build-i386-from-arm32 bc ft))
             ((= target 1) (build-aarch64-from-arm32 bc ft))
             (t (build-x64-from-arm32 bc ft))))
          ((= my-arch 2)
           ;; Running on i386
           (cond
             ((= target 3) (build-arm32-from-i386 bc ft))
             ((= target 2) (build-i386-from-i386 bc ft))
             ((zerop target) (build-x64-from-i386 bc ft))
             (t (build-aarch64-from-i386 bc ft))))
          ((= target 3)
           ;; arm32 target (from x64 or aarch64 host)
           (if (zerop my-arch)
               (build-arm32-from-x64 bc ft)
               (build-arm32-from-aarch64 bc ft)))
          ((= target 2)
           ;; i386 target (from x64 or aarch64 host)
           (if (zerop my-arch)
               (build-i386-from-x64 bc ft)
               (build-i386-from-aarch64 bc ft)))
          ((zerop my-arch)
           ;; Running on x64
           (if (zerop target)
               (build-x64-from-x64 bc ft)
               (build-aarch64-from-x64 bc ft)))
          (t
           ;; Running on AArch64
           (if (zerop target)
               (build-x64-from-aarch64 bc ft)
               (build-x64-from-x64 bc ft)))))
      ;; Print DONE marker for host script
      (write-char-serial 68) (write-char-serial 79) ;; DO
      (write-char-serial 78) (write-char-serial 69) ;; NE
      (write-char-serial 10))))

