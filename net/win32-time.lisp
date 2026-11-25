;;;; net/win32-time.lisp -- this machine's time zone, for the Windows image.
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, *WIN64-PLATFORM-FILES*), after
;;;; net/win32-api.lisp.  modus has no time zone database: DECODE-UNIVERSAL-TIME with no zone is
;;;; GMT.  Windows knows the zone the person set, so an app that shows a clock (chirp's Bitchat
;;;; window) asks it here, by name, and adds the answer itself.
;;;;
;;;; GetTimeZoneInformation, not a sum of GetLocalTime and GetSystemTime: it answers the bias
;;;; and whether daylight time is in effect, so the offset is exact to the minute and there is
;;;; no race between two clock reads.

(defun w32-utc-offset ()
  "Seconds east of UTC on this machine now (its zone, daylight time included), or 0 when
   Windows will not say."
  ;; TIME_ZONE_INFORMATION (172 bytes): Bias at 0, StandardBias at 84, DaylightBias at 168, all
  ;; LONG minutes, UTC = local + bias.  The answer says which applies: 1 standard, 2 daylight,
  ;; 0 neither (no daylight time here), else failure.
  (or (ignore-errors
       (multiple-value-bind (id err tzi)
           (w32-call* :u32 "kernel32.dll!GetTimeZoneInformation" '(:buf 172))
         (declare (ignore err))
         (let ((bias (w32-octets-get tzi 0 :s32)))
           (case id
             (0 (* -60 bias))
             (1 (* -60 (+ bias (w32-octets-get tzi 84 :s32))))
             (2 (* -60 (+ bias (w32-octets-get tzi 168 :s32))))
             (t 0)))))
      0))
