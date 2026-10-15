(defvar *ss* #x14F00000)

(defvar *sp* #x14F00800)

(defun smp-poke (page words) (let ((i 0)) (dolist (w words) (%jit-emit-word32 (+ page (* i 4)) w) (setq i (+ i 1)))) (%jit-icache-flush page 4096) page)

(defun smp-snapshot () (smp-poke #x14F02000 (list (logior #xD2800000 (ash (logand *sp* #xFFFF) 5)) (logior #xF2A00000 (ash (logand (ash *sp* -16) #xFFFF) 5)) #xd53ca201 #xf9000001 #xd53c2041 #xf9000401 #xd53c2001 #xf9000801 #xd53c1001 #xf9000c01 #xd53cc001 #xf9001001 #xf900141c #xd5384241 #xf9003401 #xd65f03c0)) (%jit-call #x14F02000) (loop for o from 0 to 104 by 8 collect (format nil "~X" (%gc-read64 (+ *sp* o)))))

(defun core-spin () (let ((a (+ #x111F1F80 (* 8 (percpu-ref 16))))) (loop (setf (mem-ref a :u32) (+ 1 (mem-ref a :u32))))))

(defun smp-counters () (loop for c from 0 below 4 collect (mem-ref (+ #x111F1F80 (* c 8)) :u32)))

(defun smp-start (n fn &optional r24 r25) (let ((w (+ #x10400000 (* n #x20000))) (pc (+ #x10080000 (* n #x4000))) (top (+ #x05400000 (* (- n 1) #x200000))) (x24 (or r24 (+ #x05B00000 (* n #x100000))))) (dotimes (i 8704) (setf (mem-ref (+ w (* i 8)) :u64) 0)) (dotimes (i 2048) (setf (mem-ref (+ pc (* i 8)) :u64) 0)) (%gc-write64 (+ w #xFFE8) pc) (setf (mem-ref (+ pc 16) :u64) n) (setf (mem-ref (+ #x111F1F80 (* 8 n)) :u32) 0) (smp-poke *ss* (list #x10004000 #xf9400001 #xd51ca201 #xf9400401 #xd51c2041 #xf9400801 #xd51c2001 #xd50c871f #xd5033f9f #xd5033fdf #xd539f221 #xb27a0021 #xd519f221 #xf9401001 #xd51cc001 #xd518c001 #xd53c1141 #x9275f821 #xd51c1141 #xd2a00601 #xd5181041 #xd5033fdf #xf9400c01 #xd51c1001 #xd5033fdf #xf9401801 #x9100003f #xf9401c01 #xd518d081 #xf9402001 #xd51bd041 #xf940141c #xf9402418 #xf9402819 #xd2a20012 #xd280003a #xf2bbd5ba #xd2800021 #xf9003001 #xf9402c09 #xd280001d #xd280001e #xd63f0120 #x10003aa0 #xd2800041 #xf9003001 #xd503205f #x17ffffff )) (%gc-write64 (+ *sp* 48) top) (%gc-write64 (+ *sp* 56) pc) (%gc-write64 (+ *sp* 64) (- w #x10000000)) (%gc-write64 (+ *sp* 72) x24) (%gc-write64 (+ *sp* 80) (or r25 (+ x24 #xF0000))) (%gc-write64 (+ *sp* 88) (- (%val->word (symbol-function fn)) 3)) (%gc-write64 (+ *sp* 96) 0) (smp-poke #x14F03000 (list (logior #xD2800000 (ash (logand *ss* #xFFFF) 5)) (logior #xF2A00000 (ash (logand (ash *ss* -16) #xFFFF) 5)) #x91240002 #xd50b7e20 #x91010000 #xeb02001f #x54ffffa3 #xd2801800 #xd50b7e20 #xd5033f9f #xd503209f #xd65f03c0)) (%gc-write64 (+ #xD8 (* 8 n)) *ss*) (%jit-call #x14F03000) (let ((st 0)) (dotimes (i 2000000) (setq st (mem-ref (+ *sp* 96) :u32)) (when (> st 0) (return))) (list :core n :status st :tag (logand (%val->word (symbol-function fn)) 15)))))

(defun ap () (get-alloc-ptr))

(defun smp-stack-top (n) (+ #x05400000 (* (- n 1) #x200000)))

(defun smp-carve (n size) (let* ((k 2) (r0 (%gc-region-0)) (from (%gc-meta-read r0 k)) (to (%gc-meta-read (+ r0 8) k)) (sz (%gc-meta-read (+ r0 #x10) k)) (new (- sz size)) (rcb (+ #x10090000 (* n 64)))) (if (>= (ap) (- (+ from new) #x800000)) (list :live-data-too-high (ap) from new) (progn (%gc-region-shrink r0 new k) (set-alloc-limit (- (+ from new) #x800000)) (%gc-region-init rcb (+ from new) (+ to new) size (smp-stack-top n) k) (%gc-write64 (+ #x10000F08 (* 8 n)) rcb) (setf (mem-ref #x10080010 :u64) 0) (setf (mem-ref #x10000FF8 :u32) 1) (list :rcb rcb :x24 (+ from new) :x25 (- (+ from new size) #x800000) :r0-from from :r0-new-size new)))))

(defun core-alloc () (let ((a (+ #x111F1F80 (* 8 (percpu-ref 16)))) (n 0)) (loop (let ((l nil) (s 0)) (dotimes (i 1000) (setq l (cons i l))) (dolist (x l) (setq s (+ s x))) (when (= s 499500) (setq n (+ n 1)) (setf (mem-ref a :u32) n))))))

(defun smp-region-gcs (n) (%gc-meta-read (+ #x10090000 (* n 64) #x20) 2))
