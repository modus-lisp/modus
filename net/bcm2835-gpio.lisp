;;;; bcm2835-gpio.lisp - BCM2835/2837 GPIO, system timer and activity LED
;;;;
;;;; Baked into every Raspberry Pi CL image (mvm/build-cl-repl-common.lisp, the
;;;; :rpi arm).  Replaces the mini-Lisp `rpi-periph' image's net/bcm2835-periph.lisp:
;;;; its framebuffer is net/hdmi-fb.lisp's and its RNG net/hwrng-bcm2835.lisp's,
;;;; so what is left is plain register access, here.
;;;;
;;;; Peripheral base 0x3F000000 (BCM2837: Pi 3, Pi Zero 2 W).  Every register is
;;;; 32 bits and must be accessed as such.
;;;;   GPFSELn  0x3F200000 + 4*(pin/10)   3 bits per pin: 0 input, 1 output, 4..7 ALT0..3, 3/2 ALT4/5
;;;;   GPSET0/1 0x3F20001C / 0x20         write 1 = drive high
;;;;   GPCLR0/1 0x3F200028 / 0x2C         write 1 = drive low
;;;;   GPLEV0/1 0x3F200034 / 0x38         pin level
;;;;   CLO      0x3F003004                1 MHz free-running counter, low 32 bits

(defun gpio-reg (base pin) (+ base (* 4 (floor pin 32))))

(defun gpio-function (pin)
  "The 3-bit function code of PIN (0 input, 1 output, else an ALT function)."
  (let ((addr (+ #x3F200000 (* 4 (floor pin 10))))
        (shift (* 3 (mod pin 10))))
    (logand (ash (mem-ref addr :u32) (- shift)) 7)))

(defun gpio-select (pin fn)
  "Set PIN's function to FN (0 input, 1 output, else an ALT code)."
  (let* ((addr (+ #x3F200000 (* 4 (floor pin 10))))
         (shift (* 3 (mod pin 10)))
         (old (mem-ref addr :u32)))
    (setf (mem-ref addr :u32)
          (logior (logand old (logxor #xFFFFFFFF (ash 7 shift))) (ash (logand fn 7) shift)))
    fn))

(defun gpio-set (pin)
  (setf (mem-ref (gpio-reg #x3F20001C pin) :u32) (ash 1 (mod pin 32))) pin)
(defun gpio-clear (pin)
  (setf (mem-ref (gpio-reg #x3F200028 pin) :u32) (ash 1 (mod pin 32))) pin)
(defun gpio-write (pin level)
  (if level (gpio-set pin) (gpio-clear pin)) level)
(defun gpio-level (pin)
  "T when PIN reads high."
  (logbitp (mod pin 32) (mem-ref (gpio-reg #x3F200034 pin) :u32)))

(defun timer-micros ()
  "The BCM system timer's low word: microseconds, wrapping every ~71 minutes."
  (mem-ref #x3F003004 :u32))

(defun delay-us (us)
  (let ((t0 (timer-micros)))
    (loop (when (>= (logand (- (timer-micros) t0) #xFFFFFFFF) us) (return nil)))))

;;; Activity LED: GPIO 29 on the Pi 3B (active LOW).  Other boards wire it
;;; elsewhere (the Zero 2 W's is also 29); this is the old image's choice.
(defun led-pin () 29)
(defun led-init () (gpio-select (led-pin) 1))
(defun led-on () (gpio-clear (led-pin)))
(defun led-off () (gpio-set (led-pin)))
(defun led-blink (count ms)
  (dotimes (i count)
    (led-on) (delay-us (* ms 1000))
    (led-off) (delay-us (* ms 1000))))
