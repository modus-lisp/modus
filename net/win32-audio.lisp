;;;; net/win32-audio.lisp -- the speaker, for the Windows image (modus.exe).
;;;;
;;;; Compiled into the Windows image only, beside net/win32-window.lisp (WIN64-CONFIGURE-BUILD).
;;;; The shape of kiln's speaker (open / write / queued, pseudo-syscalls 1010-1012 on the Mac
;;;; and the phone), on the classic waveOut API:
;;;;
;;;;   (w32-audio-open RATE CHANNELS)      -> a speaker on the default output, or NIL and why
;;;;   (w32-audio-write SPK PCM)           queue PCM (a vector of signed 16-bit samples,
;;;;                                       interleaved when CHANNELS > 1); T, or NIL when every
;;;;                                       buffer is still playing (the frame is dropped)
;;;;   (w32-audio-queued SPK)              samples queued and not yet played, per channel
;;;;   (w32-audio-close SPK)
;;;;
;;;; THE DEFAULT OUTPUT (WAVE_MAPPER) is whatever Windows plays to -- speakers, HDMI, and
;;;; Bluetooth headphones or speakers alike: Windows presents those as ordinary outputs.
;;;; WINMM is loaded at the first open (LoadLibraryA), so a console modus.exe never maps it.
;;;;
;;;; BUFFERS: +#x10000 holds 16 WAVEHDRs, each over its own 16 KB of the block from +#x20000;
;;;; a header is free when Windows has not got it queued (WHDR_INQUEUE clear).  The samples are
;;;; copied in (a 20 ms frame is 960 of them), so nothing of Lisp's is handed to Windows.
;;;; A speaker is a vector: 0 block  1 hwaveout  2 channels  3 next header to try.
;;;; Block: +0 the HWAVEOUT, +#x100 WAVEFORMATEX, +#x400 the functions (%WA-PROCS order),
;;;; +#x600 a name being looked up, +#x800 the arguments of a call.

(defconstant +wa-buffers+ 16)
(defconstant +wa-buffer-bytes+ 16384)

(defun %wa-procs ()
  '("waveOutOpen" "waveOutPrepareHeader" "waveOutWrite" "waveOutUnprepareHeader"   ; 0-3
    "waveOutReset" "waveOutClose"))                                               ; 4 5

(defun %wa-call (block i &rest args)
  (let ((a (+ block #x800)) (n 0))
    (dolist (x args) (%ww-put64 (+ a (* 8 n)) x) (setq n (+ n 1)))
    (syscall3 #x3000 (%ww-get64 (+ block #x400 (* 8 i))) a n)))

(defun w32-audio-open (rate channels)
  "A speaker playing RATE Hz, CHANNELS channels of signed 16-bit samples on the default output;
   NIL if there is none."
  (let* ((bytes (+ #x20000 (* +wa-buffers+ +wa-buffer-bytes+)))
         (block (syscall6 9 0 bytes 3 34 -1 0)))
    (when (or (not (integerp block)) (<= block 0))
      (return-from w32-audio-open nil))
    (let ((lib (syscall6 +w32-loadlibrarya+ (%ww-put-cstr (+ block #x600) "winmm.dll") 0 0 0 0 0))
          (i 0))
      (dolist (name (%wa-procs))
        (let ((f (if (= lib 0) 0
                     (syscall6 +w32-getprocaddress+ lib (%ww-put-cstr (+ block #x600) name) 0 0 0 0))))
          (when (= f 0)
            (syscall6 11 block bytes 0 0 0 0)
            (return-from w32-audio-open (values nil "winmm.dll is not there")))
          (%ww-put64 (+ block #x400 (* 8 i)) f)
          (setq i (+ i 1)))))
    ;; WAVEFORMATEX: PCM, CHANNELS, RATE, RATE*2*CHANNELS bytes a second, 2*CHANNELS a block,
    ;; 16 bits, no extra
    (let ((wf (+ block #x100)))
      (%ww-put16 wf 1) (%ww-put16 (+ wf 2) channels) (%ww-put32 (+ wf 4) rate)
      (%ww-put32 (+ wf 8) (* rate 2 channels)) (%ww-put16 (+ wf 12) (* 2 channels))
      (%ww-put16 (+ wf 14) 16) (%ww-put16 (+ wf 16) 0)
      ;; waveOutOpen(&hwo, WAVE_MAPPER, &wf, 0, 0, CALLBACK_NULL)
      (let ((r (logand (%wa-call block 0 block #xFFFFFFFF wf 0 0 0) #xFFFFFFFF)))
        (unless (= r 0)
          (syscall6 11 block bytes 0 0 0 0)
          ;; 6 MMSYSERR_NODRIVER, 2 MMSYSERR_BADDEVICEID: no output device (Windows Audio not
          ;; running, as on a server by default)
          (return-from w32-audio-open
            (values nil (format nil "waveOutOpen answered ~d~a" r
                                (if (or (= r 2) (= r 6)) " (no output device)" "")))))))
    (let ((spk (make-array 4 :initial-element 0)))
      (setf (aref spk 0) block (aref spk 1) (%ww-get64 block) (aref spk 2) channels)
      spk)))

(defun %wa-header (block i) (+ block #x10000 (* 48 i)))

(defun w32-audio-write (spk pcm)
  "Queue the signed 16-bit samples PCM; NIL (dropped) when every buffer is still playing."
  (let* ((block (aref spk 0)) (hwo (aref spk 1))
         (n (min (length pcm) (floor +wa-buffer-bytes+ 2))))
    (dotimes (k +wa-buffers+ nil)
      (let* ((i (mod (+ (aref spk 3) k) +wa-buffers+))
             (h (%wa-header block i)))
        (when (= 0 (logand (%ww-get32 (+ h 24)) #x10))       ; dwFlags: not WHDR_INQUEUE
          (let ((data (+ block #x20000 (* i +wa-buffer-bytes+))))
            ;; a header used before is unprepared first (WHDR_PREPARED, 2)
            (when (logtest (%ww-get32 (+ h 24)) 2) (%wa-call block 3 hwo h 48))
            (dotimes (j n)
              (let ((v (logand (aref pcm j) #xFFFF)))
                (setf (mem-ref (+ data (* 2 j)) :u8) (logand v 255))
                (setf (mem-ref (+ data (* 2 j) 1) :u8) (ash v -8))))
            ;; WAVEHDR: lpData, dwBufferLength, the rest zero
            (dotimes (z 6) (%ww-put64 (+ h (* 8 z)) 0))
            (%ww-put64 h data)
            (%ww-put32 (+ h 8) (* 2 n))
            (%wa-call block 1 hwo h 48)                       ; waveOutPrepareHeader
            (%wa-call block 2 hwo h 48)                       ; waveOutWrite
            (setf (aref spk 3) (mod (+ i 1) +wa-buffers+))
            (return t)))))))

(defun w32-audio-queued (spk)
  "Samples per channel queued and not yet played (whole buffers: the one playing counts whole)."
  (let ((block (aref spk 0)) (bytes 0))
    (dotimes (i +wa-buffers+)
      (let ((h (%wa-header block i)))
        (when (logtest (%ww-get32 (+ h 24)) #x10)
          (setq bytes (+ bytes (%ww-get32 (+ h 8)))))))
    (floor bytes (* 2 (aref spk 2)))))

(defun w32-audio-close (spk)
  (let ((block (aref spk 0)) (hwo (aref spk 1)))
    (%wa-call block 4 hwo)                                    ; waveOutReset: everything done
    (dotimes (i +wa-buffers+)
      (let ((h (%wa-header block i)))
        (when (logtest (%ww-get32 (+ h 24)) 2) (%wa-call block 3 hwo h 48))))
    (%wa-call block 5 hwo)                                    ; waveOutClose
    (syscall6 11 block (+ #x20000 (* +wa-buffers+ +wa-buffer-bytes+)) 0 0 0 0)
    nil))
