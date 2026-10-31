;;;; usb-engine-threads.lisp - start and stop the I/O core's engine (net/usb-io-core.lisp).
;;;; Threaded board builds only: the engine is a bare thread (net/bare-threads.lisp).

(defun usbe-start (&optional (core 1))
  "From the MAIN thread: give the USB controller to an engine thread alone on CORE.
   :STARTED, :ALREADY, or why not.  The RTL8153 must be bound (NET-USB-PROBE) first."
  (cond ((usbe-running-p) :already)
        ((not (eql (usb-netdev-get) 2)) :no-rtl8153)
        ((zerop (%klog-base)) :no-log)
        (t
         (io-stubs)
         (io-map-local)
         (usbe-init-block)
         (bt-spawn (function usbe-main) core)
         (let ((k 0))
           (loop
             (when (usbe-running-p) (return :started))
             (when (> k 200000000) (return :timeout))
             (setq k (+ k 1)))))))

(defun usbe-stop ()
  "Ask the engine to stop and wait for it; the controller goes back to whoever calls
   the drivers directly.  :STOPPED or :NOT-RUNNING."
  (if (not (usbe-running-p))
      :not-running
      (progn
        (usbe-put 8 1)
        (usbe-poke)
        (let ((k 0))
          (loop
            (when (= (usbe-get 4) 3) (return :stopped))
            (when (> k 200000000) (return :timeout))
            (setq k (+ k 1)))))))
