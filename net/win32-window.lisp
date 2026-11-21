;;;; net/win32-window.lisp -- a native window for the Windows image (modus.exe).
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, WIN64-CONFIGURE-BUILD).
;;;; Three calls, all made on the thread that owns the window, which must be the
;;;; thread that opened it (Win32 delivers a window's messages to its creator):
;;;;
;;;;   (w32-window-open WIDTH HEIGHT TITLE)        -> a window, its client area WIDTH x HEIGHT
;;;;   (w32-window-present WIN PIXELS PW X0 Y0 X1 Y1)  show PIXELS' rectangle [X0,X1) x [Y0,Y1)
;;;;   (w32-window-events WIN TIMEOUT-MS)          -> the events since the last call
;;;;   (w32-window-close WIN)
;;;;
;;;; PIXELS is a vector of (unsigned-byte 32) X8R8G8B8, PW pixels to a row -- glass's
;;;; framebuffer, whose word order is exactly a 32-bit Windows DIB's.  The events are
;;;; RFB's, so a glass presenter hands them on unchanged: (:key DOWN KEYSYM) with an X
;;;; keysym, (:pointer BUTTON-MASK X Y) with RFB's mask (1 left, 2 middle, 4 right,
;;;; 8/16 wheel), and (:paint) and (:close).
;;;;
;;;; USER32 AND GDI32 ARE LOADED HERE, not imported: a console modus.exe never maps them
;;;; (LoadLibraryA + GetProcAddress at W32-WINDOW-OPEN; the addresses go in the window's
;;;; block, raw memory, so nothing a worker thread allocated is stored anywhere shared).
;;;; Calls go through the shim's #x3000 (function pointer, arguments in memory).
;;;;
;;;; THE WINDOW PROCEDURE IS NATIVE (boot-win64, WIN64-WNDPROC-ITEMS): Win32 calls it
;;;; inside DispatchMessageA, and it only appends the message to the window's ring.  No
;;;; Lisp runs inside a Win32 callback; W32-WINDOW-EVENTS reads the ring afterwards.
;;;;
;;;; THE PIXELS ARE COPIED into a back buffer VirtualAlloc'd with the window, and
;;;; StretchDIBits reads from there: the collector moves Lisp vectors, and a blit runs
;;;; while other threads may collect.  The wait for messages is GC-safe
;;;; (%GC-SAFE-BLOCK-6), so a window thread idling in it never holds up a collection.
;;;;
;;;; A window is a vector:
;;;;   0 hwnd  1 hdc  2 block  3 width  4 height  5 button mask  6 last x  7 last y
;;;;   8 control held
;;;; Block layout: +0 the event ring (count dword, capacity dword at +8, 24-byte entries
;;;; from +16); +#x10000 WNDCLASSEXA, +#x10100 MSG, +#x10200 BITMAPINFOHEADER,
;;;; +#x10300 RECT, +#x10400 the argument vector of a call, +#x10600 the title,
;;;; +#x10700 the class name, +#x10800 the functions (%WW-PROCS order), +#x10A00 a
;;;; name being looked up; +#x20000 the back buffer.

(defun %ww-put32 (a v) (setf (mem-ref a :u32) (logand v #xFFFFFFFF)))
(defun %ww-put16 (a v)
  (setf (mem-ref a :u8) (logand v 255))
  (setf (mem-ref (+ a 1) :u8) (logand (ash v -8) 255)))
(defun %ww-put64 (a v)
  (let ((u (if (< v 0) (+ v 18446744073709551616) v)))
    (%ww-put32 a (logand u #xFFFFFFFF))
    (%ww-put32 (+ a 4) (logand (ash u -32) #xFFFFFFFF))))
(defun %ww-get32 (a) (mem-ref a :u32))
(defun %ww-get64 (a) (+ (mem-ref a :u32) (* 4294967296 (mem-ref (+ a 4) :u32))))
(defun %ww-s16 (v) (let ((x (logand v #xFFFF))) (if (>= x 32768) (- x 65536) x)))
(defun %ww-s32 (v) (let ((x (logand v #xFFFFFFFF))) (if (>= x 2147483648) (- x 4294967296) x)))
(defun %ww-put-cstr (a s)
  (dotimes (i (length s)) (setf (mem-ref (+ a i) :u8) (logand (char-code (char s i)) 255)))
  (setf (mem-ref (+ a (length s)) :u8) 0)
  a)

(defun %ww-procs ()
  "The USER32 and GDI32 functions a window uses, in the order of their slots."
  '(("user32.dll" "RegisterClassExA") ("user32.dll" "CreateWindowExA")        ; 0 1
    ("user32.dll" "DefWindowProcA") ("user32.dll" "ShowWindow")               ; 2 3
    ("user32.dll" "UpdateWindow") ("user32.dll" "PeekMessageA")               ; 4 5
    ("user32.dll" "TranslateMessage") ("user32.dll" "DispatchMessageA")       ; 6 7
    ("user32.dll" "MsgWaitForMultipleObjectsEx") ("user32.dll" "GetDC")       ; 8 9
    ("user32.dll" "ReleaseDC") ("user32.dll" "ValidateRect")                  ; 10 11
    ("user32.dll" "AdjustWindowRectEx") ("user32.dll" "LoadCursorA")          ; 12 13
    ("user32.dll" "DestroyWindow") ("user32.dll" "SetWindowTextA")            ; 14 15
    ("user32.dll" "GetWindowLongPtrA") ("user32.dll" "SetWindowLongPtrA")     ; 16 17
    ("user32.dll" "SetForegroundWindow") ("user32.dll" "SetProcessDPIAware")  ; 18 19
    ("gdi32.dll" "StretchDIBits")))                                           ; 20

(defun %ww-load-procs (block)
  "Look every %WW-PROCS function up into BLOCK; NIL if one is missing."
  (let ((i 0) (name (+ block #x10A00)))
    (dolist (p (%ww-procs) t)
      (let* ((lib (syscall6 +w32-loadlibrarya+ (%ww-put-cstr name (first p)) 0 0 0 0 0))
             (f (if (= lib 0) 0
                    (syscall6 +w32-getprocaddress+ lib (%ww-put-cstr name (second p)) 0 0 0 0))))
        (when (= f 0) (return nil))
        (%ww-put64 (+ block #x10800 (* 8 i)) f)
        (setq i (+ i 1))))))

(defun %ww-call (block i &rest args)
  "Call %WW-PROCS function I (looked up into BLOCK) with ARGS (up to 20)."
  (let ((a (+ block #x10400)) (n 0))
    (dolist (x args) (%ww-put64 (+ a (* 8 n)) x) (setq n (+ n 1)))
    (syscall3 #x3000 (%ww-get64 (+ block #x10800 (* 8 i))) a n)))

(defconstant +w32-window-style+ #x00CA0000)   ; WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX

(defun w32-window-open (width height title)
  "Open a WIDTH x HEIGHT window titled TITLE on this thread; the window, or NIL."
  (let* ((bytes (+ #x20000 (* 4 width height)))
         (block (syscall6 9 0 bytes 3 34 -1 0)))
    (when (or (not (integerp block)) (<= block 0))
      (return-from w32-window-open nil))
    (unless (%ww-load-procs block)
      (syscall6 11 block bytes 0 0 0 0)
      (return-from w32-window-open nil))
    ;; the window procedure's three USER32 calls (boot-win64, WIN64-WNDPROC-ITEMS)
    (%ww-put64 +w32-wp-getwlp-va+ (%ww-get64 (+ block #x10800 (* 8 16))))
    (%ww-put64 +w32-wp-defwp-va+ (%ww-get64 (+ block #x10800 (* 8 2))))
    (%ww-put64 +w32-wp-validate-va+ (%ww-get64 (+ block #x10800 (* 8 11))))
    (%ww-call block 19)                                   ; SetProcessDPIAware
    (let* ((hinst (syscall6 +w32-getmodulehandlea+ 0 0 0 0 0 0))
           (wc (+ block #x10000))
           (class (%ww-put-cstr (+ block #x10700) "ModusWindow"))
           (text (%ww-put-cstr (+ block #x10600) title))
           (rect (+ block #x10300)))
      ;; the event ring: empty, 2048 entries
      (%ww-put32 block 0)
      (%ww-put32 (+ block 8) 2048)
      ;; WNDCLASSEXA: cbSize, CS_OWNDC, the native window procedure, hInstance, an
      ;; arrow cursor, the class name.  A second window's RegisterClassExA fails
      ;; (the class exists), which is what it should do.
      (%ww-put32 wc 80)
      (%ww-put32 (+ wc 4) #x20)
      (%ww-put64 (+ wc 8) +w32-wndproc-va+)
      (%ww-put64 (+ wc 24) hinst)
      (%ww-put64 (+ wc 40) (%ww-call block 13 0 32512))   ; LoadCursorA(IDC_ARROW)
      (%ww-put64 (+ wc 64) class)
      (%ww-call block 0 wc)                               ; RegisterClassExA
      ;; the outer size whose client area is WIDTH x HEIGHT
      (%ww-put32 rect 0) (%ww-put32 (+ rect 4) 0)
      (%ww-put32 (+ rect 8) width) (%ww-put32 (+ rect 12) height)
      (%ww-call block 12 rect +w32-window-style+ 0 0)     ; AdjustWindowRectEx
      (let* ((ow (- (%ww-s32 (%ww-get32 (+ rect 8))) (%ww-s32 (%ww-get32 rect))))
             (oh (- (%ww-s32 (%ww-get32 (+ rect 12))) (%ww-s32 (%ww-get32 (+ rect 4)))))
             (hwnd (%ww-call block 1 0 class text +w32-window-style+   ; CreateWindowExA
                             #x80000000 #x80000000 ow oh 0 0 hinst 0)))
        (when (or (not (integerp hwnd)) (= hwnd 0))
          (return-from w32-window-open nil))
        ;; the ring, where the window procedure looks for it (GWLP_USERDATA)
        (%ww-call block 17 hwnd #xFFFFFFEB block)          ; SetWindowLongPtrA
        (%ww-call block 3 hwnd 5)                          ; ShowWindow(SW_SHOW)
        (%ww-call block 4 hwnd)                            ; UpdateWindow
        (%ww-call block 18 hwnd)                           ; SetForegroundWindow
        ;; BITMAPINFOHEADER: 40 bytes, 32 bits per pixel, BI_RGB; the width and the
        ;; (negative: top-down) height are set per blit.
        (let ((bmi (+ block #x10200)))
          (dotimes (i 10) (%ww-put32 (+ bmi (* 4 i)) 0))
          (%ww-put32 bmi 40)
          (%ww-put16 (+ bmi 12) 1)
          (%ww-put16 (+ bmi 14) 32))
        (let ((win (make-array 9 :initial-element 0)))
          (setf (aref win 0) hwnd
                (aref win 1) (%ww-call block 9 hwnd)              ; GetDC
                (aref win 2) block
                (aref win 3) width
                (aref win 4) height
                (aref win 8) nil)
          win)))))

(defun w32-window-present (win pixels pw x0 y0 x1 y1)
  "Show the rectangle [X0,X1) x [Y0,Y1) of PIXELS (PW pixels to a row) at the same
   place in WIN."
  (let* ((w (aref win 3)) (h (aref win 4))
         (x0 (max 0 x0)) (y0 (max 0 y0)) (x1 (min w x1)) (y1 (min h y1)))
    (when (and (< x0 x1) (< y0 y1))
      (let* ((block (aref win 2)) (buf (+ block #x20000)) (bmi (+ block #x10200))
             (rw (- x1 x0)) (rh (- y1 y0)))
        (do ((y y0 (+ y 1))) ((>= y y1))
          (let ((src (* y pw)) (dst (+ buf (* 4 (* y w)))))
            (do ((x x0 (+ x 1))) ((>= x x1))
              (setf (mem-ref (+ dst (* 4 x)) :u32) (aref pixels (+ src x))))))
        ;; The DIB is the rows Y0..Y1 of the back buffer, top-down, so the source
        ;; rectangle's Y is 0 whatever StretchDIBits makes of a top-down origin.
        (%ww-put32 (+ bmi 4) w)
        (%ww-put32 (+ bmi 8) (- 0 rh))
        (%ww-call block 20 (aref win 1) x0 y0 rw rh x0 0 rw rh   ; StretchDIBits
                  (+ buf (* 4 (* y0 w))) bmi 0 #x00CC0020)))
    t))

(defun %ww-vk-keysym (vk)
  "The X keysym of a virtual key that WM_CHAR does not carry, or NIL."
  (cond ((= vk 8) #xff08) ((= vk 9) #xff09) ((= vk 13) #xff0d) ((= vk 27) #xff1b)
        ((= vk 46) #xffff) ((= vk 37) #xff51) ((= vk 38) #xff52) ((= vk 39) #xff53)
        ((= vk 40) #xff54) ((= vk 36) #xff50) ((= vk 35) #xff57) ((= vk 33) #xff55)
        ((= vk 34) #xff56) ((= vk 45) #xff63)
        ((and (>= vk 112) (<= vk 123)) (+ #xffbe (- vk 112)))     ; F1..F12
        ((= vk 16) #xffe1) ((= vk 17) #xffe3) ((= vk 18) #xffe9)  ; Shift, Control, Alt
        (t nil)))

(defun w32-window-events (win timeout-ms)
  "Wait up to TIMEOUT-MS for input, run the window's messages, and return what they
   said, oldest first: (:key DOWN KEYSYM), (:pointer MASK X Y), (:paint), (:close)."
  (let* ((block (aref win 2)) (a (+ block #x10400)))
    ;; MsgWaitForMultipleObjectsEx(0, NULL, TIMEOUT-MS, QS_ALLINPUT, 0), GC-safe
    (%ww-put64 a 0) (%ww-put64 (+ a 8) 0) (%ww-put64 (+ a 16) timeout-ms)
    (%ww-put64 (+ a 24) #x4FF) (%ww-put64 (+ a 32) 0)
    (%gc-safe-block-6 #x3000 (%ww-get64 (+ block #x10800 (* 8 8))) a 5 0))
  (let* ((block (aref win 2)) (msg (+ block #x10100)))
    (loop
      (when (= 0 (logand (%ww-call block 5 msg 0 0 0 1) #xFFFFFFFF))   ; PeekMessageA, PM_REMOVE
        (return nil))
      (%ww-call block 6 msg)                                            ; TranslateMessage
      (%ww-call block 7 msg)))                                          ; DispatchMessageA
  (let* ((ring (aref win 2)) (n (%ww-get32 ring)) (out nil))
    (dotimes (i n)
      (let* ((e (+ ring 16 (* 24 i)))
             (m (%ww-get32 e))
             (wp (%ww-get64 (+ e 8)))
             (lp (%ww-get64 (+ e 16)))
             (x (%ww-s16 lp))
             (y (%ww-s16 (ash lp -16))))
        (cond
          ((= m #x0F) (push (list :paint) out))
          ((= m #x10) (push (list :close) out))
          ((or (= m #x200) (= m #x201) (= m #x202) (= m #x204) (= m #x205)
               (= m #x207) (= m #x208))
           (let ((mask (aref win 5)))
             (cond ((= m #x201) (setq mask (logior mask 1)))
                   ((= m #x202) (setq mask (logand mask (lognot 1))))
                   ((= m #x204) (setq mask (logior mask 4)))
                   ((= m #x205) (setq mask (logand mask (lognot 4))))
                   ((= m #x207) (setq mask (logior mask 2)))
                   ((= m #x208) (setq mask (logand mask (lognot 2)))))
             (setf (aref win 5) mask (aref win 6) x (aref win 7) y)
             (push (list :pointer mask x y) out)))
          ((= m #x20A)                                   ; WM_MOUSEWHEEL: a click of 8 or 16
           (let ((bit (if (> (%ww-s16 (ash wp -16)) 0) 8 16))
                 (mask (aref win 5)) (lx (aref win 6)) (ly (aref win 7)))
             (push (list :pointer (logior mask bit) lx ly) out)
             (push (list :pointer mask lx ly) out)))
          ((or (= m #x100) (= m #x104) (= m #x101) (= m #x105))
           (let* ((down (or (= m #x100) (= m #x104)))
                  (vk (logand wp #xFF))
                  (ks (%ww-vk-keysym vk)))
             (when (= vk 17) (setf (aref win 8) down))
             (cond (ks (push (list :key down ks) out))
                   ;; Control-letter: WM_CHAR brings a control code, so the
                   ;; letter's keysym comes from the key itself.
                   ((and (aref win 8) (>= vk 65) (<= vk 90))
                    (push (list :key down (+ vk 32)) out)))))
          ((= m #x102)                                   ; WM_CHAR: a printable character
           (let ((c (logand wp #xFFFF)))
             (when (and (>= c 32) (/= c 127) (not (aref win 8)))
               (push (list :key t c) out)
               (push (list :key nil c) out)))))))
    (%ww-put32 ring 0)
    (nreverse out)))

(defun w32-window-close (win)
  (%ww-call (aref win 2) 10 (aref win 0) (aref win 1))   ; ReleaseDC
  (%ww-call (aref win 2) 14 (aref win 0))                ; DestroyWindow
  (syscall6 11 (aref win 2) (+ #x20000 (* 4 (aref win 3) (aref win 4))) 0 0 0 0)
  nil)
