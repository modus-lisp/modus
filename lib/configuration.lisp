;;;; configuration.lisp - (modus:image-configuration): what this image is.
;;;;
;;;; Three parts:
;;;;   IMAGE  what it runs on: machine, Lisp version, build id, features.
;;;;   BUILD  how it was built: the script, the source commit, every MODUS_*
;;;;          variable the build read and the value it got, and the emission flags
;;;;          that decide what code it contains.  Recorded by the build host
;;;;          (lib/image-config.lisp) as %IMAGE-BUILD-CONFIGURATION.
;;;;   LIVE   what it is doing now: one line per subsystem that can say, named in
;;;;          %CONFIGURATION-SECTIONS.  A subsystem this image does not have is
;;;;          skipped (its function is looked up when asked, not linked).
;;;;
;;;;   (modus:image-configuration)              ; print it
;;;;   (modus:image-configuration :data t)      ; the same as a plist

(defun %configuration-sections ()
  "(label . function name) of every live section an image may have.  Add a subsystem
   here: a function of no arguments returning a plist."
  '((:log . klog-status)
    (:io-core . io-status)))

(defun %configuration-live ()
  (let ((acc nil))
    (dolist (s (%configuration-sections))
      (let ((f (intern (symbol-name (cdr s)) "MODUS-INT")))
        (when (fboundp f)
          (push (list (car s) (funcall f)) acc))))
    (nreverse acc)))

(defun %configuration-image ()
  (list :machine-type (machine-type) :software-type (software-type)
        :lisp (lisp-implementation-version)
        :build-id (let ((f (intern "%CORE-BUILD-ID" "MODUS-INT")))
                    (and (fboundp f) (funcall f)))
        :features *features*))

(defun image-configuration (&key data (stream *standard-output*))
  "Print what this image is: what it runs on, how it was built (every MODUS_* knob
   the build read, the emission flags, the source commit) and what its subsystems
   are doing now.  DATA: return it as a plist instead."
  (let ((image (%configuration-image))
        (build (%image-build-configuration))
        (live (%configuration-live)))
    (if data
        (list :image image :build build :live live)
        (progn
          (format stream "~&;; IMAGE~%")
          (format stream "  machine     ~A (~A)~%" (getf image :machine-type) (getf image :software-type))
          (format stream "  lisp        ~A~@[, build id ~D~]~%" (getf image :lisp) (getf image :build-id))
          (format stream "  features    ~{~S~^ ~}~%" (getf image :features))
          (format stream ";; BUILD~%")
          (format stream "  script      ~A~@[  (target ~A)~]~%" (getf build :script) (getf build :target))
          (format stream "  commit      ~A~:[~; (with uncommitted changes)~]~%"
                  (getf build :commit) (getf build :uncommitted-changes))
          (let ((set nil) (unset nil))
            (dolist (k (getf build :knobs))
              (if (second k) (push k set) (push (first k) unset)))
            (format stream "  knobs set   ~D~%" (length set))
            (dolist (k (nreverse set))
              (if (eq (second k) :output-path)
                  (format stream "    ~A  (an output path)~%" (first k))
                  (format stream "    ~A = ~S~%" (first k) (second k))))
            (format stream "  knobs read, at their default (~D):~%" (length unset))
            (let ((col 4))
              (format stream "    ")
              (dolist (n (nreverse unset))
                (when (> (+ col (length n)) 78) (format stream "~%    ") (setq col 4))
                (format stream "~A " n)
                (setq col (+ col 1 (length n))))
              (terpri stream)))
          (format stream "  flags~%")
          (dolist (f (getf build :flags))
            (format stream "    ~A = ~S~%" (first f) (second f)))
          (format stream ";; LIVE~%")
          (if live
              (dolist (s live) (format stream "  ~(~A~)  ~S~%" (first s) (second s)))
              (format stream "  (no subsystem reports here)~%"))
          (values)))))
