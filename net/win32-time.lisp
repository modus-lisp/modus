;;;; net/win32-time.lisp -- this machine's time zone, for the Windows image.
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, *WIN64-PLATFORM-FILES*).
;;;; modus has no time zone database: DECODE-UNIVERSAL-TIME with no zone is GMT.  Windows
;;;; knows the zone the person set, so an app that shows a clock (chirp's Bitchat window)
;;;; asks it here, by name, and adds the answer itself.
;;;;
;;;; GetTimeZoneInformation, not a sum of GetLocalTime and GetSystemTime: it answers the
;;;; bias and whether daylight time is in effect, so the offset is exact to the minute and
;;;; there is no race between two clock reads.  Nothing is kept between calls (on modus a
;;;; worker thread may not store into a global): each call maps a scratch page, asks,
;;;; and unmaps it.

(defun w32-utc-offset ()
  "Seconds east of UTC on this machine now (its zone, daylight time included), or 0 when
   Windows will not say."
  (let* ((page #x1000)
         (block (syscall6 9 0 page 3 34 -1 0)))
    (if (or (not (integerp block)) (<= block 0))
        0
        (unwind-protect
             (let* ((name (+ block #x100)) (args (+ block #x200)) (tzi (+ block #x400))
                    (lib (syscall6 +w32-loadlibrarya+ (%ww-put-cstr name "kernel32.dll") 0 0 0 0 0))
                    (fn (if (= lib 0) 0
                            (syscall6 +w32-getprocaddress+ lib (%ww-put-cstr name "GetTimeZoneInformation")
                                      0 0 0 0))))
               (if (= fn 0)
                   0
                   ;; TIME_ZONE_INFORMATION: Bias at 0, StandardBias at 84, DaylightBias at 168,
                   ;; all LONG minutes, UTC = local + bias.  The answer says which applies:
                   ;; 1 standard, 2 daylight, 0 neither (no daylight time here), else failure.
                   (progn
                     (%ww-put64 args tzi)
                     (let ((id (logand (syscall3 #x3000 fn args 1) #xFFFFFFFF))
                           (bias (%ww-s32 (%ww-get32 tzi))))
                       (case id
                         (0 (* -60 bias))
                         (1 (* -60 (+ bias (%ww-s32 (%ww-get32 (+ tzi 84))))))
                         (2 (* -60 (+ bias (%ww-s32 (%ww-get32 (+ tzi 168))))))
                         (t 0))))))
          (syscall6 11 block page 0 0 0 0)))))
