;;;; net/win32-ble.lisp -- Bluetooth LE links for a mesh, on Windows (WinRT).
;;;;
;;;; Compiled into the Windows image only.  The Windows side of kiln's Bluetooth.swift, with its
;;;; shape and its events, so a mesh written against one (chirp: bitchat's protocol) runs on both:
;;;; this machine is both a CENTRAL (it finds peers offering a service and connects to them) and a
;;;; PERIPHERAL (it offers the service, for peers to connect to), each link a number.  One service
;;;; with one characteristic: a peer writes to it and is told of what this side sends by
;;;; notification; as a central this side writes to the other's characteristic and subscribes to it.
;;;;
;;;;   (w32-ble-start SERVICE CHARACTERISTIC)  -> a radio (UUID strings), or NIL and why
;;;;   (w32-ble-poll RADIO)                    -> the events since the last poll, oldest first:
;;;;        (:state ON)          the radio is on (1) or not (0: no adapter, off, or no LE)
;;;;        (:link LINK UP MAX)  a link opened (UP 1, MAX the most one write or notification carries)
;;;;                             or closed (UP 0)
;;;;        (:data LINK BYTES)   what the link's peer sent: one write or one notification
;;;;        (:log TEXT)          what happened, for a log
;;;;   (w32-ble-send RADIO LINK BYTES)         -> T if it went to the radio
;;;;   (w32-ble-stop RADIO)
;;;;   (w32-ble-state RADIO)                   -> :adapter (still asking), :on, :off, :stopped
;;;;   (w32-ble-selftest)                      -> each step of the plumbing and what it answered,
;;;;                                              on any machine, radio or not
;;;;
;;;; NOTHING HERE BLOCKS: every WinRT operation is started and then looked at by W32-BLE-POLL, which
;;;; a window's tick can call 60 times a second.  A connection is a chain of operations (the device,
;;;; its service, its characteristic, the subscription, the session's MTU), each step advanced when
;;;; its operation is done.  The radio is the caller's object (on modus a thread keeps its own); its
;;;; WinRT context (net/win32-winrt.lisp) holds everything Windows sees.
;;;;
;;;; Event handlers (delegates) are identified by a number: KIND + 8 * SERIAL, KIND one of
;;;; 1 advertisement received, 2 a central link's value changed, 3 a device's connection changed,
;;;; 4 the subscribers changed, 5 a peer wrote; SERIAL names the connection a handler belongs to.

(defconstant +ble-max-central-links+ 6)

(defstruct (w32-ble (:constructor %make-w32-ble))
  ctx service characteristic
  (phase :adapter) adapter-op
  (watcher 0) (watcher-token 0)
  ;; the peripheral side: its setup chain, then the provider and the offered characteristic
  (offer :start) (offer-op 0) (provider 0) (local-service 0) (local-char 0)
  (links nil)                 ; w32-ble-link, newest first
  (connecting nil)            ; w32-ble-conn, chains in progress
  (writes nil)                ; (op deferral peer-id) for write requests being fetched
  (seen nil)                  ; addresses being connected to or linked
  (next-link 1) (next-serial 1)
  (spare-delegates nil)       ; delegate objects of closed connections, for reuse
  (out nil))                  ; events, newest first

(defstruct (w32-ble-link (:constructor %make-w32-ble-link))
  id kind max
  (device 0) (char 0) (address 0) (serial 0) (value-token 0) (status-token 0)   ; to a peripheral
  (value-delegate 0) (status-delegate 0)
  (client 0) (peer ""))                                                        ; from a central

(defstruct (w32-ble-conn (:constructor %make-w32-ble-conn))
  address serial (stage :device) (op 0) (device 0) (char 0)
  (value-delegate 0) (status-delegate 0) (value-token 0) (status-token 0))

(defun %ble-event (r &rest e) (push e (w32-ble-out r)))
(defun %ble-log (r fmt &rest args) (%ble-event r :log (apply #'format nil fmt args)))

(defun %ble-hr-text (hr) (format nil "0x~8,'0X" (logand hr #xFFFFFFFF)))

;;; ---- starting --------------------------------------------------------------------------------

(defun w32-ble-start (service characteristic)
  "The radio on: finding peers that offer SERVICE (a UUID string), and offering it, its one
   CHARACTERISTIC.  A radio, or NIL and why.  Its state arrives as an event from W32-BLE-POLL."
  (multiple-value-bind (ctx why) (wrt-open)
    (unless ctx (return-from w32-ble-start (values nil (format nil "no Windows Runtime: ~a" why))))
    (let* ((r (%make-w32-ble :ctx ctx :service service :characteristic characteristic))
           (statics (wrt-factory ctx "Windows.Devices.Bluetooth.BluetoothAdapter"
                                 +wrt-iid-ibluetoothadapterstatics+)))
      (if (= statics 0)
          (progn (%ble-log r "no BluetoothAdapter class") (%ble-event r :state 0) (setf (w32-ble-phase r) :off))
          (let ((hr (wrt-call ctx statics +wrt-ibluetoothadapterstatics-getdefaultasync+ (wrt-out-clear ctx 0))))
            (wrt-release ctx statics)
            (if (< hr 0)
                (progn (%ble-log r "GetDefaultAsync: ~a" (%ble-hr-text hr)) (%ble-event r :state 0)
                       (setf (w32-ble-phase r) :off))
                (setf (w32-ble-adapter-op r) (wrt-out-ref ctx 0)))))
      r)))

(defun %ble-adapter-ready (r)
  "The adapter query is done: say whether there is a radio, and if so start both roles."
  (let* ((ctx (w32-ble-ctx r)) (adapter (wrt-op-result ctx (w32-ble-adapter-op r))))
    (setf (w32-ble-adapter-op r) 0)
    (flet ((flag (slot) (and (>= (wrt-call ctx adapter slot (wrt-out-clear ctx 1)) 0)
                             (/= 0 (logand (wrt-out-ref ctx 1) #xFF)))))
      (if (or (null adapter) (= adapter 0))
          (progn (%ble-log r "no Bluetooth adapter") (%ble-event r :state 0) (setf (w32-ble-phase r) :off))
          (let ((le (flag +wrt-ibluetoothadapter-get_islowenergysupported+))
                (central (flag +wrt-ibluetoothadapter-get_iscentralrolesupported+))
                (peripheral (flag +wrt-ibluetoothadapter-get_isperipheralrolesupported+)))
            (wrt-release ctx adapter)
            (%ble-log r "adapter: low energy ~:[no~;yes~], central ~:[no~;yes~], peripheral ~:[no~;yes~]"
                      le central peripheral)
            (if (not le)
                (progn (%ble-event r :state 0) (setf (w32-ble-phase r) :off))
                (progn
                  (%ble-event r :state 1)
                  (setf (w32-ble-phase r) :on)
                  (if central (%ble-start-watcher r) (%ble-log r "no central role: not looking for peers"))
                  (if peripheral (%ble-offer-start r) (%ble-log r "no peripheral role: not offering the service")))))))))

(defun %ble-start-watcher (r)
  "Look for advertisements of the service; each one found is a peer to connect to."
  (let* ((ctx (w32-ble-ctx r))
         (insp (wrt-activate ctx "Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementWatcher"))
         (w (wrt-qi ctx insp +wrt-iid-ibluetoothleadvertisementwatcher+)))
    (wrt-release ctx insp)
    (when (= w 0) (%ble-log r "no advertisement watcher") (return-from %ble-start-watcher nil))
    (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-put_scanningmode+ 1)   ; Active
    ;; filter: the service's UUID only
    (when (>= (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-get_advertisementfilter+ (wrt-out-clear ctx 2)) 0)
      (let ((filter (wrt-out-ref ctx 2)))
        (when (>= (wrt-call ctx filter +wrt-ibluetoothleadvertisementfilter-get_advertisement+ (wrt-out-clear ctx 3)) 0)
          (let ((adv (wrt-out-ref ctx 3)))
            (when (>= (wrt-call ctx adv +wrt-ibluetoothleadvertisement-get_serviceuuids+ (wrt-out-clear ctx 4)) 0)
              (let ((uuids (wrt-out-ref ctx 4)))
                (wrt-call ctx uuids +wrt-ivector-append+ (wrt-guid ctx 0 (w32-ble-service r)))
                (wrt-release ctx uuids)))
            (wrt-release ctx adv)))
        (wrt-release ctx filter)))
    (let* ((d (wrt-delegate ctx 1 +wrt-iid-handler-adv-received+))
           (hr (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-add_received+ d (wrt-out-clear ctx 5))))
      (if (< hr 0)
          (%ble-log r "watcher add_Received: ~a" (%ble-hr-text hr))
          (setf (w32-ble-watcher-token r) (wrt-out-ref ctx 5))))
    (let ((hr (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-start+)))
      (%ble-log r "looking for peers~:[: ~a~;~*~]" (>= hr 0) (%ble-hr-text hr)))
    (setf (w32-ble-watcher r) w)))

;;; ---- the peripheral: offering the service ----------------------------------------------------

(defun %ble-offer-start (r)
  (let* ((ctx (w32-ble-ctx r))
         (statics (wrt-factory ctx "Windows.Devices.Bluetooth.GenericAttributeProfile.GattServiceProvider"
                               +wrt-iid-igattserviceproviderstatics+)))
    (if (= statics 0)
        (progn (%ble-log r "no GattServiceProvider class") (setf (w32-ble-offer r) :off))
        (let ((hr (wrt-call ctx statics +wrt-igattserviceproviderstatics-createasync+
                            (wrt-guid ctx 0 (w32-ble-service r)) (wrt-out-clear ctx 6))))
          (wrt-release ctx statics)
          (if (< hr 0)
              (progn (%ble-log r "GattServiceProvider.CreateAsync: ~a" (%ble-hr-text hr)) (setf (w32-ble-offer r) :off))
              (setf (w32-ble-offer-op r) (wrt-out-ref ctx 6) (w32-ble-offer r) :provider))))))

(defun %ble-offer-step (r)
  "Advance the peripheral's setup: the provider, then the characteristic, then advertising."
  (let ((ctx (w32-ble-ctx r)) (op (w32-ble-offer-op r)))
    (when (and (member (w32-ble-offer r) '(:provider :characteristic)) (/= op 0))
      (case (wrt-op-state ctx op)
        (:running nil)
        (:failed (wrt-release ctx op) (setf (w32-ble-offer-op r) 0 (w32-ble-offer r) :off)
         (%ble-log r "offering the service failed (~(~a~))" (w32-ble-offer r)))
        (:done
         (let ((res (wrt-op-result ctx op)))
           (setf (w32-ble-offer-op r) 0)
           (if (eq (w32-ble-offer r) :provider)
               (%ble-offer-provider r res)
               (%ble-offer-characteristic r res))))))))

(defun %ble-offer-provider (r res)
  (let ((ctx (w32-ble-ctx r)))
    (wrt-call ctx res +wrt-igattserviceproviderresult-get_error+ (wrt-out-clear ctx 7))
    (let ((err (logand (wrt-out-ref ctx 7) #xFFFFFFFF)))
      (wrt-call ctx res +wrt-igattserviceproviderresult-get_serviceprovider+ (wrt-out-clear ctx 8))
      (let ((provider (wrt-out-ref ctx 8)))
        (wrt-release ctx res)
        (if (or (/= err 0) (= provider 0))
            (progn (%ble-log r "service provider: Bluetooth error ~d" err) (setf (w32-ble-offer r) :off))
            (progn
              (setf (w32-ble-provider r) provider)
              (wrt-call ctx provider +wrt-igattserviceprovider-get_service+ (wrt-out-clear ctx 9))
              (setf (w32-ble-local-service r) (wrt-out-ref ctx 9))
              ;; the characteristic: read, write, write without response, notify; no protection
              (let* ((insp (wrt-activate ctx "Windows.Devices.Bluetooth.GenericAttributeProfile.GattLocalCharacteristicParameters"))
                     (params (wrt-qi ctx insp +wrt-iid-igattlocalcharacteristicparameters+)))
                (wrt-release ctx insp)
                (wrt-call ctx params +wrt-igattlocalcharacteristicparameters-put_characteristicproperties+ 30)
                (wrt-call ctx params +wrt-igattlocalcharacteristicparameters-put_readprotectionlevel+ 0)
                (wrt-call ctx params +wrt-igattlocalcharacteristicparameters-put_writeprotectionlevel+ 0)
                (let ((hr (wrt-call ctx (w32-ble-local-service r) +wrt-igattlocalservice-createcharacteristicasync+
                                    (wrt-guid ctx 1 (w32-ble-characteristic r)) params (wrt-out-clear ctx 10))))
                  (wrt-release ctx params)
                  (if (< hr 0)
                      (progn (%ble-log r "CreateCharacteristicAsync: ~a" (%ble-hr-text hr)) (setf (w32-ble-offer r) :off))
                      (setf (w32-ble-offer-op r) (wrt-out-ref ctx 10) (w32-ble-offer r) :characteristic))))))))))

(defun %ble-offer-characteristic (r res)
  (let ((ctx (w32-ble-ctx r)))
    (wrt-call ctx res +wrt-igattlocalcharacteristicresult-get_error+ (wrt-out-clear ctx 11))
    (let ((err (logand (wrt-out-ref ctx 11) #xFFFFFFFF)))
      (wrt-call ctx res +wrt-igattlocalcharacteristicresult-get_characteristic+ (wrt-out-clear ctx 12))
      (let ((ch (wrt-out-ref ctx 12)))
        (wrt-release ctx res)
        (if (or (/= err 0) (= ch 0))
            (progn (%ble-log r "characteristic: Bluetooth error ~d" err) (setf (w32-ble-offer r) :off))
            (progn
              (setf (w32-ble-local-char r) ch)
              (wrt-call ctx ch +wrt-igattlocalcharacteristic-add_writerequested+
                        (wrt-delegate ctx 5 +wrt-iid-handler-write-requested+) (wrt-out-clear ctx 13))
              (wrt-call ctx ch +wrt-igattlocalcharacteristic-add_subscribedclientschanged+
                        (wrt-delegate ctx 4 +wrt-iid-handler-subscribers-changed+) (wrt-out-clear ctx 13))
              ;; advertise, connectable and discoverable; no local name (as bitchat)
              (let* ((insp (wrt-activate ctx "Windows.Devices.Bluetooth.GenericAttributeProfile.GattServiceProviderAdvertisingParameters"))
                     (ap (wrt-qi ctx insp +wrt-iid-igattserviceprovideradvertisingparameters+)))
                (wrt-release ctx insp)
                (wrt-call ctx ap +wrt-igattserviceprovideradvertisingparameters-put_isconnectable+ 1)
                (wrt-call ctx ap +wrt-igattserviceprovideradvertisingparameters-put_isdiscoverable+ 1)
                (let ((hr (wrt-call ctx (w32-ble-provider r) +wrt-igattserviceprovider-startadvertisingwithparameters+ ap)))
                  (wrt-release ctx ap)
                  (%ble-log r "offering the service~:[: ~a~;~*~]" (>= hr 0) (%ble-hr-text hr))))
              (setf (w32-ble-offer r) :on)))))))

;;; ---- the central: connecting to a peer -------------------------------------------------------

(defun %ble-delegate (r kind serial iid)
  "A handler for KIND of connection SERIAL: a spare one re-numbered, or a new one."
  (let ((ctx (w32-ble-ctx r)) (id (+ kind (* 8 serial))))
    (let ((d (find-if (lambda (x) (eql (car x) kind)) (w32-ble-spare-delegates r))))
      (if d
          (progn (setf (w32-ble-spare-delegates r) (remove d (w32-ble-spare-delegates r)))
                 (%ww-put64 (+ (cdr d) 16) id)
                 (cdr d))
          (wrt-delegate ctx id iid)))))

(defun %ble-found (r address)
  "An advertisement of the service from ADDRESS: connect, unless already linked or connecting."
  (unless (or (member address (w32-ble-seen r))
              (>= (+ (length (w32-ble-connecting r))
                     (count :to-peripheral (w32-ble-links r) :key #'w32-ble-link-kind))
                  +ble-max-central-links+))
    (let* ((ctx (w32-ble-ctx r))
           (statics (wrt-factory ctx "Windows.Devices.Bluetooth.BluetoothLEDevice" +wrt-iid-ibluetoothledevicestatics+)))
      (unless (= statics 0)
        (let ((hr (wrt-call ctx statics +wrt-ibluetoothledevicestatics-frombluetoothaddressasync+ address
                            (wrt-out-clear ctx 14))))
          (wrt-release ctx statics)
          (when (>= hr 0)
            (push address (w32-ble-seen r))
            (%ble-log r "found ~12,'0X" address)
            (push (%make-w32-ble-conn :address address :serial (w32-ble-next-serial r) :op (wrt-out-ref ctx 14))
                  (w32-ble-connecting r))
            (incf (w32-ble-next-serial r))))))))

(defun %ble-spare (r kind d)
  "Delegate D (of KIND) free for the next connection."
  (unless (= d 0) (push (cons kind d) (w32-ble-spare-delegates r))))

(defun %ble-conn-fail (r c why)
  (let ((ctx (w32-ble-ctx r)))
    (%ble-log r "connect ~12,'0X failed: ~a" (w32-ble-conn-address c) why)
    (unless (= (w32-ble-conn-value-delegate c) 0)
      (wrt-call ctx (w32-ble-conn-char c) +wrt-igattcharacteristic-remove_valuechanged+ (w32-ble-conn-value-token c))
      (%ble-spare r 2 (w32-ble-conn-value-delegate c)))
    (wrt-release ctx (w32-ble-conn-char c))
    (wrt-release ctx (w32-ble-conn-device c))
    (setf (w32-ble-connecting r) (remove c (w32-ble-connecting r))
          (w32-ble-seen r) (remove (w32-ble-conn-address c) (w32-ble-seen r)))))

(defun %ble-conn-step (r c)
  "Advance connection C one step if its operation is done."
  (let* ((ctx (w32-ble-ctx r)) (op (w32-ble-conn-op c)) (st (wrt-op-state ctx op)))
    (case st
      (:running nil)
      (:failed (wrt-release ctx op) (%ble-conn-fail r c (format nil "~(~a~)" (w32-ble-conn-stage c))))
      (:done
       (let ((res (wrt-op-result ctx op)))
         (setf (w32-ble-conn-op c) 0)
         (case (w32-ble-conn-stage c)
           (:device                              ; the device: ask for the service
            (if (or (null res) (= res 0))
                (%ble-conn-fail r c "no device")
                (let ((d3 (wrt-qi ctx res +wrt-iid-ibluetoothledevice3+)))
                  (setf (w32-ble-conn-device c) res)
                  (let ((hr (wrt-call ctx d3 +wrt-ibluetoothledevice3-getgattservicesforuuidasync+
                                      (wrt-guid ctx 2 (w32-ble-service r)) (wrt-out-clear ctx 15))))
                    (wrt-release ctx d3)
                    (if (< hr 0)
                        (%ble-conn-fail r c (%ble-hr-text hr))
                        (setf (w32-ble-conn-op c) (wrt-out-ref ctx 15) (w32-ble-conn-stage c) :services))))))
           (:services                            ; the service: ask for the characteristic
            (let ((svc (%ble-first-of ctx res +wrt-igattdeviceservicesresult-get_status+
                                      +wrt-igattdeviceservicesresult-get_services+)))
              (if (= svc 0)
                  (%ble-conn-fail r c "no service")
                  (let ((s3 (wrt-qi ctx svc +wrt-iid-igattdeviceservice3+)))
                    (wrt-release ctx svc)
                    (let ((hr (wrt-call ctx s3 +wrt-igattdeviceservice3-getcharacteristicsforuuidasync+
                                        (wrt-guid ctx 3 (w32-ble-characteristic r)) (wrt-out-clear ctx 16))))
                      (wrt-release ctx s3)
                      (if (< hr 0)
                          (%ble-conn-fail r c (%ble-hr-text hr))
                          (setf (w32-ble-conn-op c) (wrt-out-ref ctx 16) (w32-ble-conn-stage c) :characteristic)))))))
           (:characteristic                      ; the characteristic: subscribe to it
            (let ((ch (%ble-first-of ctx res +wrt-igattcharacteristicsresult-get_status+
                                     +wrt-igattcharacteristicsresult-get_characteristics+)))
              (if (= ch 0)
                  (%ble-conn-fail r c "no characteristic")
                  (progn
                    (setf (w32-ble-conn-char c) ch)
                    (let ((vd (%ble-delegate r 2 (w32-ble-conn-serial c) +wrt-iid-handler-value-changed+)))
                      (setf (w32-ble-conn-value-delegate c) vd)
                      (wrt-call ctx ch +wrt-igattcharacteristic-add_valuechanged+ vd (wrt-out-clear ctx 17))
                      (setf (w32-ble-conn-value-token c) (wrt-out-ref ctx 17)))
                    (let ((hr (wrt-call ctx ch +wrt-igattcharacteristic-writeclientcharacteristicconfigurationdescriptorasync+
                                        1 (wrt-out-clear ctx 18))))       ; Notify
                      (if (< hr 0)
                          (%ble-conn-fail r c (%ble-hr-text hr))
                          (setf (w32-ble-conn-op c) (wrt-out-ref ctx 18) (w32-ble-conn-stage c) :subscribe)))))))
           (:subscribe                           ; subscribed (status 0 = Success): the session's MTU
            (if (not (eql (and res (logand res #xFFFFFFFF)) 0))
                (%ble-conn-fail r c (format nil "subscribe status ~a" res))
                (let* ((d4 (wrt-qi ctx (w32-ble-conn-device c) +wrt-iid-ibluetoothledevice4+))
                       (id (progn (wrt-call ctx d4 +wrt-ibluetoothledevice4-get_bluetoothdeviceid+ (wrt-out-clear ctx 19))
                                  (wrt-out-ref ctx 19)))
                       (statics (wrt-factory ctx "Windows.Devices.Bluetooth.GenericAttributeProfile.GattSession"
                                             +wrt-iid-igattsessionstatics+)))
                  (wrt-release ctx d4)
                  (let ((hr (if (or (= id 0) (= statics 0)) -1
                                (wrt-call ctx statics +wrt-igattsessionstatics-fromdeviceidasync+ id (wrt-out-clear ctx 20)))))
                    (wrt-release ctx statics) (wrt-release ctx id)
                    (if (< hr 0)
                        (%ble-conn-up r c 20)       ; no session: the smallest a write always carries
                        (setf (w32-ble-conn-op c) (wrt-out-ref ctx 20) (w32-ble-conn-stage c) :session))))))
           (:session                             ; the MTU, less the 3-byte ATT header
            (let ((mtu 23))
              (when (and res (/= res 0))
                (wrt-call ctx res +wrt-igattsession-put_maintainconnection+ 1)
                (when (>= (wrt-call ctx res +wrt-igattsession-get_maxpdusize+ (wrt-out-clear ctx 21)) 0)
                  (setq mtu (logand (wrt-out-ref ctx 21) #xFFFF)))
                (wrt-release ctx res))
              (%ble-conn-up r c (max 20 (- mtu 3)))))))))))

(defun %ble-first-of (ctx res status-slot list-slot)
  "The first element of RES's list (a services or characteristics result), if its status is
   Success; RES is released.  0 if there is none."
  (if (or (null res) (= res 0))
      0
      (let ((ok (and (>= (wrt-call ctx res status-slot (wrt-out-clear ctx 22)) 0)
                     (= 0 (logand (wrt-out-ref ctx 22) #xFFFFFFFF)))))
        (let ((v (if (and ok (>= (wrt-call ctx res list-slot (wrt-out-clear ctx 22)) 0)) (wrt-out-ref ctx 22) 0)))
          (wrt-release ctx res)
          (if (= v 0)
              0
              (let ((n (progn (wrt-call ctx v +wrt-ivectorview-get_size+ (wrt-out-clear ctx 22))
                              (logand (wrt-out-ref ctx 22) #xFFFFFFFF))))
                (let ((x (if (> n 0)
                             (progn (wrt-call ctx v +wrt-ivectorview-getat+ 0 (wrt-out-clear ctx 22))
                                    (wrt-out-ref ctx 22))
                             0)))
                  (wrt-release ctx v)
                  x)))))))

(defun %ble-conn-up (r c max)
  (let* ((ctx (w32-ble-ctx r)) (id (w32-ble-next-link r))
         (sd (%ble-delegate r 3 (w32-ble-conn-serial c) +wrt-iid-handler-connection-changed+)))
    (incf (w32-ble-next-link r))
    (setf (w32-ble-conn-status-delegate c) sd)
    (wrt-call ctx (w32-ble-conn-device c) +wrt-ibluetoothledevice-add_connectionstatuschanged+ sd (wrt-out-clear ctx 23))
    (setf (w32-ble-connecting r) (remove c (w32-ble-connecting r)))
    (push (%make-w32-ble-link :id id :kind :to-peripheral :max max :device (w32-ble-conn-device c)
                              :char (w32-ble-conn-char c) :address (w32-ble-conn-address c)
                              :serial (w32-ble-conn-serial c) :value-token (w32-ble-conn-value-token c)
                              :status-token (wrt-out-ref ctx 23)
                              :value-delegate (w32-ble-conn-value-delegate c) :status-delegate sd)
          (w32-ble-links r))
    (%ble-event r :link id 1 max)))

(defun %ble-link-down (r l)
  (let ((ctx (w32-ble-ctx r)))
    (setf (w32-ble-links r) (remove l (w32-ble-links r)))
    (if (eq (w32-ble-link-kind l) :to-peripheral)
        (progn
          (wrt-call ctx (w32-ble-link-char l) +wrt-igattcharacteristic-remove_valuechanged+ (w32-ble-link-value-token l))
          (wrt-call ctx (w32-ble-link-device l) +wrt-ibluetoothledevice-remove_connectionstatuschanged+
                    (w32-ble-link-status-token l))
          (%ble-spare r 2 (w32-ble-link-value-delegate l))
          (%ble-spare r 3 (w32-ble-link-status-delegate l))
          (wrt-release ctx (w32-ble-link-char l))
          ;; IClosable: drop the connection
          (let ((cl (wrt-qi ctx (w32-ble-link-device l) +wrt-iid-iclosable+)))
            (unless (= cl 0) (wrt-call ctx cl +wrt-iclosable-close+) (wrt-release ctx cl)))
          (wrt-release ctx (w32-ble-link-device l))
          (setf (w32-ble-seen r) (remove (w32-ble-link-address l) (w32-ble-seen r))))
        (wrt-release ctx (w32-ble-link-client l)))
    (%ble-event r :link (w32-ble-link-id l) 0 0)))

;;; ---- the events ------------------------------------------------------------------------------

(defun %ble-peer-of-session (ctx session)
  "The device id (text) of the peer on SESSION (a GattSession), which is released."
  (if (= session 0)
      ""
      (let ((id (progn (wrt-call ctx session +wrt-igattsession-get_deviceid+ (wrt-out-clear ctx 24))
                       (wrt-out-ref ctx 24))))
        (wrt-release ctx session)
        (if (= id 0)
            ""
            (let ((h (progn (wrt-call ctx id +wrt-ibluetoothdeviceid-get_id+ (wrt-out-clear ctx 24))
                            (wrt-out-ref ctx 24))))
              (wrt-release ctx id)
              (wrt-hstring-text ctx h))))))

(defun %ble-central-link (r peer max client)
  "The link from the central PEER, made if there is none (CLIENT, when known, kept for notifying)."
  (or (find-if (lambda (l) (and (eq (w32-ble-link-kind l) :from-central)
                                (string= (w32-ble-link-peer l) peer)))
               (w32-ble-links r))
      (let ((l (%make-w32-ble-link :id (w32-ble-next-link r) :kind :from-central :max max
                                   :client client :peer peer)))
        (incf (w32-ble-next-link r))
        (push l (w32-ble-links r))
        (%ble-event r :link (w32-ble-link-id l) 1 max)
        l)))

(defun %ble-subscribers (r)
  "The subscribed clients changed: a link for each new one, and the gone ones' links closed."
  (let* ((ctx (w32-ble-ctx r)) (ch (w32-ble-local-char r)) (now nil))
    (when (>= (wrt-call ctx ch +wrt-igattlocalcharacteristic-get_subscribedclients+ (wrt-out-clear ctx 25)) 0)
      (let* ((v (wrt-out-ref ctx 25))
             (n (progn (wrt-call ctx v +wrt-ivectorview-get_size+ (wrt-out-clear ctx 25))
                       (logand (wrt-out-ref ctx 25) #xFFFFFFFF))))
        (dotimes (i n)
          (wrt-call ctx v +wrt-ivectorview-getat+ i (wrt-out-clear ctx 25))
          (let* ((client (wrt-out-ref ctx 25))
                 (max (progn (wrt-call ctx client +wrt-igattsubscribedclient-get_maxnotificationsize+ (wrt-out-clear ctx 26))
                             (logand (wrt-out-ref ctx 26) #xFFFF)))
                 (peer (progn (wrt-call ctx client +wrt-igattsubscribedclient-get_session+ (wrt-out-clear ctx 26))
                              (%ble-peer-of-session ctx (wrt-out-ref ctx 26))))
                 (l (%ble-central-link r peer (max 20 max) client)))
            (push peer now)
            (if (= (w32-ble-link-client l) client)
                nil
                (progn (wrt-release ctx (w32-ble-link-client l))        ; a write made it before:
                       (setf (w32-ble-link-client l) client             ; now it can be notified
                             (w32-ble-link-max l) (max 20 max))))))
        (wrt-release ctx v)))
    (dolist (l (w32-ble-links r))
      (when (and (eq (w32-ble-link-kind l) :from-central) (/= (w32-ble-link-client l) 0)
                 (not (member (w32-ble-link-peer l) now :test #'string=)))
        (%ble-link-down r l)))))

(defun %ble-write-requested (r args)
  "A peer wrote: hold the request open (a deferral) while its value is fetched."
  (let* ((ctx (w32-ble-ctx r))
         (deferral (progn (wrt-call ctx args +wrt-igattwriterequestedeventargs-getdeferral+ (wrt-out-clear ctx 27))
                          (wrt-out-ref ctx 27)))
         (peer (progn (wrt-call ctx args +wrt-igattwriterequestedeventargs-get_session+ (wrt-out-clear ctx 27))
                      (%ble-peer-of-session ctx (wrt-out-ref ctx 27)))))
    (when (>= (wrt-call ctx args +wrt-igattwriterequestedeventargs-getrequestasync+ (wrt-out-clear ctx 27)) 0)
      (push (list (wrt-out-ref ctx 27) deferral peer) (w32-ble-writes r)))))

(defun %ble-write-step (r w)
  (let ((ctx (w32-ble-ctx r)))
    (destructuring-bind (op deferral peer) w
      (case (wrt-op-state ctx op)
        (:running nil)
        (t
         (let ((req (and (eq (wrt-op-state ctx op) :done) (wrt-op-result ctx op))))
           (setf (w32-ble-writes r) (remove w (w32-ble-writes r)))
           (when (and req (/= req 0))
             (let ((bytes (if (>= (wrt-call ctx req +wrt-igattwriterequest-get_value+ (wrt-out-clear ctx 28)) 0)
                              (let ((buf (wrt-out-ref ctx 28)))
                                (prog1 (wrt-buffer-bytes ctx buf) (wrt-release ctx buf)))
                              (make-array 0 :element-type '(unsigned-byte 8))))
                   (with-response (and (>= (wrt-call ctx req +wrt-igattwriterequest-get_option+ (wrt-out-clear ctx 28)) 0)
                                       (= 0 (logand (wrt-out-ref ctx 28) #xFFFFFFFF)))))
               (when with-response (wrt-call ctx req +wrt-igattwriterequest-respond+))
               (wrt-release ctx req)
               (let ((l (%ble-central-link r peer 20 0)))
                 (%ble-event r :data (w32-ble-link-id l) bytes))))
           (unless (= deferral 0)
             (wrt-call ctx deferral +wrt-ideferral-complete+)
             (wrt-release ctx deferral))))))))

(defun %ble-handle (r id sender args)
  (let ((ctx (w32-ble-ctx r)) (kind (logand id 7)) (serial (ash id -3)))
    (case kind
      (1                                       ; an advertisement of the service
       (when (and (/= args 0)
                  (>= (wrt-call ctx args +wrt-ibluetoothleadvertisementreceivedeventargs-get_bluetoothaddress+
                                (wrt-out-clear ctx 29)) 0))
         (%ble-found r (wrt-out-ref ctx 29))))
      (2                                       ; a notification on a central link
       (let ((l (find-if (lambda (l) (and (eq (w32-ble-link-kind l) :to-peripheral) (= (w32-ble-link-serial l) serial)))
                         (w32-ble-links r))))
         (when (and l (/= args 0)
                    (>= (wrt-call ctx args +wrt-igattvaluechangedeventargs-get_characteristicvalue+ (wrt-out-clear ctx 29)) 0))
           (let ((buf (wrt-out-ref ctx 29)))
             (%ble-event r :data (w32-ble-link-id l) (wrt-buffer-bytes ctx buf))
             (wrt-release ctx buf)))))
      (3                                       ; a device's connection changed
       (let ((l (find-if (lambda (l) (and (eq (w32-ble-link-kind l) :to-peripheral) (= (w32-ble-link-serial l) serial)))
                         (w32-ble-links r))))
         (when (and l (>= (wrt-call ctx (w32-ble-link-device l) +wrt-ibluetoothledevice-get_connectionstatus+
                                    (wrt-out-clear ctx 29)) 0)
                    (= 0 (logand (wrt-out-ref ctx 29) #xFFFFFFFF)))   ; Disconnected
           (%ble-link-down r l))))
      (4 (%ble-subscribers r))
      (5 (when (/= args 0) (%ble-write-requested r args))))
    (wrt-release ctx sender)
    (wrt-release ctx args)))

;;; ---- polling, sending, stopping ----------------------------------------------------------

(defun w32-ble-state (radio)
  "Where RADIO is: :ADAPTER (still asking for one), :ON, :OFF (no adapter, or no LE), :STOPPED."
  (w32-ble-phase radio))


(defun w32-ble-poll (radio)
  "Advance everything that can advance without waiting; the events since the last poll, oldest first."
  (let ((ctx (w32-ble-ctx radio)))
    (when (and (eq (w32-ble-phase radio) :adapter) (w32-ble-adapter-op radio)
               (/= (w32-ble-adapter-op radio) 0))
      (case (wrt-op-state ctx (w32-ble-adapter-op radio))
        (:running nil)
        (:failed (wrt-release ctx (w32-ble-adapter-op radio)) (setf (w32-ble-adapter-op radio) 0)
         (%ble-log radio "no adapter (the query failed)") (%ble-event radio :state 0) (setf (w32-ble-phase radio) :off))
        (:done (%ble-adapter-ready radio))))
    (when (eq (w32-ble-phase radio) :on)
      (dolist (e (wrt-events ctx)) (apply #'%ble-handle radio e))
      (dolist (c (w32-ble-connecting radio)) (%ble-conn-step radio c))
      (dolist (w (w32-ble-writes radio)) (%ble-write-step radio w))
      (%ble-offer-step radio))
    (prog1 (reverse (w32-ble-out radio)) (setf (w32-ble-out radio) nil))))

(defun w32-ble-send (radio link bytes)
  "BYTES (an octet vector) to LINK's peer: a write to its characteristic, or a notification to it.
   T if it went to the radio, NIL if there is no such link."
  (let* ((ctx (w32-ble-ctx radio))
         (l (find link (w32-ble-links radio) :key #'w32-ble-link-id)))
    (when (and l (> (length bytes) 0))
      (let ((buf (wrt-buffer ctx bytes)))
        (unless (= buf 0)
          (let ((hr (if (eq (w32-ble-link-kind l) :to-peripheral)
                        ;; without response when it fits in one packet, with (a long write) when not
                        (wrt-call ctx (w32-ble-link-char l) +wrt-igattcharacteristic-writevaluewithoptionasync+
                                  buf (if (<= (length bytes) (w32-ble-link-max l)) 1 0) (wrt-out-clear ctx 30))
                        (if (= (w32-ble-link-client l) 0)
                            -1                   ; it wrote but has not subscribed: nothing to notify
                            (wrt-call ctx (w32-ble-local-char radio)
                                      +wrt-igattlocalcharacteristic-notifyvalueforsubscribedclientasync+
                                      buf (w32-ble-link-client l) (wrt-out-clear ctx 30))))))
            (wrt-release ctx buf)
            ;; the operation finishes on its own; nothing here waits for it
            (when (>= hr 0) (wrt-release ctx (wrt-out-ref ctx 30)))
            (>= hr 0)))))))

(defun w32-ble-stop (radio)
  "Every link closed, the radio quiet, everything released."
  (let ((ctx (w32-ble-ctx radio)))
    (dolist (l (w32-ble-links radio)) (%ble-link-down radio l))
    (dolist (c (w32-ble-connecting radio))
      (unless (= (w32-ble-conn-op c) 0) (wrt-release ctx (w32-ble-conn-op c)))
      (setf (w32-ble-conn-op c) 0)
      (%ble-conn-fail radio c "stopped"))
    (unless (= (w32-ble-watcher radio) 0)
      (wrt-call ctx (w32-ble-watcher radio) +wrt-ibluetoothleadvertisementwatcher-stop+)
      (wrt-call ctx (w32-ble-watcher radio) +wrt-ibluetoothleadvertisementwatcher-remove_received+
                (w32-ble-watcher-token radio))
      (wrt-release ctx (w32-ble-watcher radio)))
    (unless (= (w32-ble-provider radio) 0)
      (wrt-call ctx (w32-ble-provider radio) +wrt-igattserviceprovider-stopadvertising+)
      (wrt-release ctx (w32-ble-local-char radio))
      (wrt-release ctx (w32-ble-local-service radio))
      (wrt-release ctx (w32-ble-provider radio)))
    (setf (w32-ble-phase radio) :stopped)
    (wrt-close ctx)
    nil))

;;; ---- a check of the plumbing, radio or not ---------------------------------------------------

(defun w32-ble-selftest ()
  "Every step below the radio, each with what it answered: the Windows Runtime, an activation
   factory, an instance, the event-handler registration (which fails if a handler's IID is
   wrong), an IBuffer round trip, and an asynchronous operation to its end."
  (multiple-value-bind (ctx why) (wrt-open)
   (let ((out nil))
    (flet ((note (what v) (push (list what v) out)))
      (note :winrt (or (and ctx t) why))
      (when ctx
        (let* ((insp (wrt-activate ctx "Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementWatcher"))
               (w (wrt-qi ctx insp +wrt-iid-ibluetoothleadvertisementwatcher+)))
          (note :watcher (/= w 0))
          (unless (= w 0)
            (note :add-received (%ble-hr-text (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-add_received+
                                                        (wrt-delegate ctx 1 +wrt-iid-handler-adv-received+)
                                                        (wrt-out-clear ctx 5))))
            (note :start (%ble-hr-text (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-start+)))
            (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-get_status+ (wrt-out-clear ctx 6))
            (note :watcher-status (logand (wrt-out-ref ctx 6) #xFFFFFFFF))   ; 0 created 1 started 2 stopping 3 stopped 4 aborted
            (wrt-call ctx w +wrt-ibluetoothleadvertisementwatcher-stop+)
            (wrt-release ctx w))
          (wrt-release ctx insp))
        (let* ((b (wrt-buffer ctx (coerce '(1 2 3 250) '(simple-array (unsigned-byte 8) (*)))))
               (back (and (/= b 0) (wrt-buffer-bytes ctx b))))
          (note :buffer (and back (coerce back 'list)))
          (wrt-release ctx b))
        (let ((statics (wrt-factory ctx "Windows.Devices.Bluetooth.BluetoothAdapter" +wrt-iid-ibluetoothadapterstatics+)))
          (note :adapter-statics (/= statics 0))
          (unless (= statics 0)
            (let ((hr (wrt-call ctx statics +wrt-ibluetoothadapterstatics-getdefaultasync+ (wrt-out-clear ctx 7))))
              (wrt-release ctx statics)
              (note :getdefaultasync (%ble-hr-text hr))
              (when (>= hr 0)
                (let ((op (wrt-out-ref ctx 7)) (st :running) (n 0))
                  (loop while (and (eq st :running) (< n 500))
                        do (progn (setq st (wrt-op-state ctx op)) (sleep 0.01) (setq n (+ n 1))))
                  (note :op-state st)
                  (when (eq st :done)
                    (let ((adapter (wrt-op-result ctx op)))
                      (note :adapter (and adapter (/= adapter 0)))
                      (wrt-release ctx adapter))))))))
        (let ((statics (wrt-factory ctx "Windows.Devices.Bluetooth.GenericAttributeProfile.GattServiceProvider"
                                    +wrt-iid-igattserviceproviderstatics+)))
          (note :provider-statics (/= statics 0))
          (wrt-release ctx statics))
        (note :events (length (wrt-events ctx)))
        (wrt-close ctx)))
    (nreverse out))))
