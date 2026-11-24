;;;; test/win64/ble-test.lisp -- the Bluetooth LE layer (net/win32-ble.lisp) on whatever machine
;;;; this runs on.  `modus.exe --script ble-test.lisp [SECONDS]`
;;;;
;;;; First the plumbing, radio or not (W32-BLE-SELFTEST: WinRT, activation, an event-handler
;;;; registration, a buffer round trip, an async operation to its end); then the radio itself for
;;;; SECONDS (default 20) with bitchat's service, every event printed as it comes.  With no adapter
;;;; the second part says so and ends; with one, any phone running bitchat nearby shows up as
;;;; found / link / data lines.  Exit 0 when the plumbing checks all pass.

(defvar *service* "F47B5E2D-4A9E-4C5A-9B3F-8E1D2C3A4B5C")   ; bitchat's mesh service
(defvar *char* "A1B2C3D4-E5F6-4A5B-8C9D-0E1F2A3B4C5D")      ; and its one characteristic

(defun say (fmt &rest args) (apply #'format t fmt args) (terpri) (finish-output))

(let ((ok t))
  (dolist (step (modus-int::w32-ble-selftest))
    (say "selftest ~(~a~): ~s" (first step) (second step))
    (when (and (member (first step) '(:winrt :watcher :adapter-statics :provider-statics))
               (not (eq (second step) t)))
      (setq ok nil))
    (when (and (eq (first step) :add-received) (not (equal (second step) "0x00000000")))
      (setq ok nil))
    (when (and (eq (first step) :buffer) (not (equal (second step) '(1 2 3 250))))
      (setq ok nil)))
  (say "plumbing: ~:[FAILED~;ok~]" ok)
  (let* ((args (cdr (member "--script" sb-ext:*posix-argv* :test #'string=)))
         (secs (or (and (second args) (parse-integer (second args) :junk-allowed t)) 20))
         (radio (modus-int::w32-ble-start *service* *char*))
         (t0 (get-internal-real-time))
         (on nil))
    (if (null radio)
        (say "radio: did not start")
        (progn
          (loop
            (dolist (e (modus-int::w32-ble-poll radio))
              (say "~,2f ~s" (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1.0) e)
              (when (eq (first e) :state) (setq on (eql (second e) 1)))
              ;; answer whatever a peer sends with its own bytes, so a link shows both directions
              (when (eq (first e) :data)
                (say "  send back: ~s" (modus-int::w32-ble-send radio (second e) (third e)))))
            (when (> (- (get-internal-real-time) t0) (* secs internal-time-units-per-second))
              (return nil))
            (when (and (not on) (> (- (get-internal-real-time) t0) (* 3 internal-time-units-per-second))
                       (not (eq (modus-int::w32-ble-state radio) :adapter)))
              (return nil))
            (sleep 0.05))
          (modus-int::w32-ble-stop radio)
          (say "radio: stopped"))))
  (sb-ext:exit :code (if ok 0 1)))
