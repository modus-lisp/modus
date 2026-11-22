;;;; net/win32-entropy.lisp -- the operating system's random bytes, for the Windows image.
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, *WIN64-PLATFORM-FILES*).
;;;; natrium seeds its DRBG from /dev/urandom, or from %MODUS-HARDWARE-ENTROPY when the image
;;;; has one (natrium's entropy.lisp; MODUS exports the name so CL-USER finds it); Windows has
;;;; no /dev/urandom, so without this every key natrium made there -- chirp's identity, an SSH
;;;; host key -- failed for want of a seed.  This is BCryptGenRandom with the system's preferred
;;;; generator (BCRYPT_USE_SYSTEM_PREFERRED_RNG), Windows' CSPRNG since Vista.
;;;;
;;;; Nothing is kept between calls: on modus a worker thread may not store into a global, and
;;;; natrium asks once per thread.  Each call maps a scratch page, has Windows fill it, copies
;;;; the bytes out and unmaps it; bcrypt.dll is loaded at the first call (and only counted up
;;;; after that), so a modus.exe that never asks never maps it.

(defun %modus-hardware-entropy (n)
  "N bytes from Windows' system random number generator, as an octet vector."
  (let* ((out (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
         (page #x10000)
         (block (syscall6 9 0 page 3 34 -1 0)))
    (when (or (not (integerp block)) (<= block 0))
      (error "modus: no memory for the random bytes"))
    (unwind-protect
         (let* ((lib (syscall6 +w32-loadlibrarya+ (%ww-put-cstr (+ block #x100) "bcrypt.dll") 0 0 0 0 0))
                (fn (if (= lib 0) 0
                        (syscall6 +w32-getprocaddress+ lib (%ww-put-cstr (+ block #x100) "BCryptGenRandom")
                                  0 0 0 0)))
                (args (+ block #x200)) (buf (+ block #x1000)) (room (- page #x1000)) (i 0))
           (when (= fn 0) (error "modus: bcrypt.dll's BCryptGenRandom is not there"))
           (loop
             (when (>= i n) (return out))
             (let ((k (min room (- n i))))
               ;; BCryptGenRandom(NULL, buf, k, BCRYPT_USE_SYSTEM_PREFERRED_RNG): STATUS_SUCCESS is 0
               (%ww-put64 args 0) (%ww-put64 (+ args 8) buf) (%ww-put64 (+ args 16) k) (%ww-put64 (+ args 24) 2)
               (let ((st (logand (syscall3 #x3000 fn args 4) #xFFFFFFFF)))
                 (unless (= st 0) (error "modus: BCryptGenRandom answered NTSTATUS #x~8,'0X" st)))
               (dotimes (j k) (setf (aref out (+ i j)) (mem-ref (+ buf j) :u8)))
               (setq i (+ i k)))))
      (syscall6 11 block page 0 0 0 0))))
