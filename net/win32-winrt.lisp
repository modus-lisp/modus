;;;; net/win32-winrt.lisp -- calling the Windows Runtime (WinRT) from the Windows image.
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, *WIN64-PLATFORM-FILES*).  What
;;;; net/win32-ble.lisp needs to talk to Windows.Devices.Bluetooth, and nothing of Bluetooth:
;;;;
;;;;   (wrt-open)                        a context, or NIL: combase loaded, this thread in the
;;;;                                     multithreaded apartment, a raw block for everything below
;;;;   (wrt-call CTX OBJ SLOT ARGS...)   OBJ's method at vtable SLOT (net/win32-winrt-ids.lisp), OBJ
;;;;                                     first; the HRESULT (a 32-bit integer; negative = failure)
;;;;   (wrt-out CTX I) / (wrt-out-ref CTX I)   out-parameter cell I: its address / what is in it
;;;;   (wrt-factory CTX CLASS IID) / (wrt-activate CTX CLASS)   a runtime class's statics / an instance
;;;;   (wrt-qi CTX OBJ IID) / (wrt-release CTX OBJ)
;;;;   (wrt-op-state CTX OP)             an IAsyncOperation's: :running, :done, :failed (never waits)
;;;;   (wrt-op-result CTX OP)            a done operation's result (a pointer, or a small value)
;;;;   (wrt-guid CTX I TEXT)             GUID cell I holding TEXT ("8-4-4-4-12"); its address
;;;;   (wrt-buffer CTX BYTES) / (wrt-buffer-bytes CTX BUF)   an IBuffer from bytes / its bytes
;;;;   (wrt-delegate CTX ID IID)         an event handler (TypedEventHandler) object answering to IID;
;;;;                                     what it is called with arrives through WRT-EVENTS
;;;;   (wrt-hstring-text CTX H)          an HSTRING's text (and the HSTRING deleted)
;;;;   (wrt-events CTX)                  (ID SENDER ARGS) for every call since the last time; each
;;;;                                     SENDER and ARGS is a reference the caller releases
;;;;
;;;; WHY A CONTEXT AND NOT GLOBALS: on modus a thread may not store objects of its own anywhere
;;;; another thread can reach (the shared-store guard), and the caller -- glass's compositor, say --
;;;; is such a thread.  The context is a vector the caller keeps, over one raw block
;;;; (VirtualAlloc'd) holding every pointer, string and argument Windows reads or writes: nothing
;;;; Lisp allocates is ever handed to Windows.
;;;;
;;;; NOTHING HERE WAITS.  An asynchronous WinRT call returns an operation; WRT-OP-STATE looks at its
;;;; status (IAsyncInfo) and returns, so a caller that polls -- a window's tick -- never blocks on
;;;; the radio.  Event handlers run on Windows' thread pool; the object WRT-DELEGATE makes is native
;;;; (boot-win64, WIN64-COM-DELEGATE-ITEMS) and only appends to the context's ring.
;;;;
;;;; A context: 0 block  1 next delegate index.
;;;; Block: +#x000 combase's functions  +#x100 the arguments of a call (20 qwords)
;;;;        +#x200 out cells (32 qwords)  +#x400 GUID cells (16 x 16 bytes)
;;;;        +#x800 a UTF-16 string (2 KB)  +#x1000 the delegate vtable  +#x1100 64 delegates (48 each)
;;;;        +#x2000 the event ring (4096 entries)  +#x20000 the IBuffer factory  .. 1 MB

(defconstant +wrt-block-bytes+ #x100000)

(defun %wrt-procs ()
  '("RoInitialize" "RoGetActivationFactory" "RoActivateInstance"            ; 0 1 2
    "WindowsCreateString" "WindowsDeleteString" "WindowsGetStringRawBuffer")) ; 3 4 5

(defun %wrt-hr (v)
  "The HRESULT in a returned RAX: its low 32 bits, signed."
  (%ww-s32 (logand v #xFFFFFFFF)))

(defun %wrt-raw-call (ctx fn args)
  (let* ((block (aref ctx 0)) (a (+ block #x100)) (n 0))
    (dolist (x args) (%ww-put64 (+ a (* 8 n)) x) (setq n (+ n 1)))
    (syscall3 #x3000 fn a n)))

(defun %wrt-combase (ctx i &rest args)
  (%wrt-raw-call ctx (%ww-get64 (+ (aref ctx 0) (* 8 i))) args))

(defun wrt-open ()
  "A WinRT context for this thread (which joins the multithreaded apartment), or NIL and why."
  (let ((block (syscall6 9 0 +wrt-block-bytes+ 3 34 -1 0)))
    (when (or (not (integerp block)) (<= block 0))
      (return-from wrt-open (values nil "no memory for the context")))
    (let ((lib (syscall6 +w32-loadlibrarya+ (%ww-put-cstr (+ block #x800) "combase.dll") 0 0 0 0 0))
          (i 0))
      (dolist (name (%wrt-procs))
        (let ((f (if (= lib 0) 0
                     (syscall6 +w32-getprocaddress+ lib (%ww-put-cstr (+ block #x800) name) 0 0 0 0))))
          (when (= f 0)
            (syscall6 11 block +wrt-block-bytes+ 0 0 0 0)
            (return-from wrt-open
              (values nil (if (= lib 0) "combase.dll is not there"
                              (format nil "combase.dll has no ~a" name)))))
          (%ww-put64 (+ block (* 8 i)) f)
          (setq i (+ i 1)))))
    (let ((ctx (make-array 2 :initial-element 0)))
      (setf (aref ctx 0) block)
      ;; RO_INIT_MULTITHREADED.  S_FALSE (1: already) and RPC_E_CHANGED_MODE are not failures here.
      (%wrt-combase ctx 0 1)
      ;; the delegate vtable: QueryInterface AddRef Release Invoke
      (%ww-put64 (+ block #x1000) +w32-com-qi-va+)
      (%ww-put64 (+ block #x1008) +w32-com-addref-va+)
      (%ww-put64 (+ block #x1010) +w32-com-release-va+)
      (%ww-put64 (+ block #x1018) +w32-com-invoke-va+)
      ;; the ring: unlocked (a qword: XCHG-MEM is 64-bit), empty, 4096 entries
      (%ww-put64 (+ block #x2000) 0) (%ww-put32 (+ block #x2008) 0) (%ww-put32 (+ block #x200C) 4096)
      ctx)))

(defun wrt-close (ctx)
  (syscall6 11 (aref ctx 0) +wrt-block-bytes+ 0 0 0 0)
  nil)

(defun wrt-call (ctx obj slot &rest args)
  "OBJ's method at vtable SLOT, called with OBJ and ARGS; the HRESULT."
  (%wrt-hr (%wrt-raw-call ctx (%ww-get64 (+ (%ww-get64 obj) (* 8 slot))) (cons obj args))))

(defun wrt-out (ctx i) (+ (aref ctx 0) #x200 (* 8 i)))
(defun wrt-out-ref (ctx i) (%ww-get64 (wrt-out ctx i)))
(defun wrt-out-clear (ctx i) (%ww-put64 (wrt-out ctx i) 0) (wrt-out ctx i))

(defun %wrt-hex-byte (text i)
  (parse-integer text :start i :end (+ i 2) :radix 16))

(defun wrt-guid (ctx i text)
  "GUID cell I holding TEXT -- \"8-4-4-4-12\", or the 32 hex digits of the GUID's bytes in memory
   (net/win32-winrt-ids.lisp's IIDs); its address."
  (let ((a (+ (aref ctx 0) #x400 (* 16 i))))
    (if (= (length text) 32)
        (dotimes (k 16) (setf (mem-ref (+ a k) :u8) (%wrt-hex-byte text (* 2 k))))
        (let ((h (remove #\- text)))
          ;; Data1, Data2, Data3 little-endian; Data4 as written
          (dotimes (k 4) (setf (mem-ref (+ a k) :u8) (%wrt-hex-byte h (* 2 (- 3 k)))))
          (dotimes (k 2) (setf (mem-ref (+ a 4 k) :u8) (%wrt-hex-byte h (+ 8 (* 2 (- 1 k))))))
          (dotimes (k 2) (setf (mem-ref (+ a 6 k) :u8) (%wrt-hex-byte h (+ 12 (* 2 (- 1 k))))))
          (dotimes (k 8) (setf (mem-ref (+ a 8 k) :u8) (%wrt-hex-byte h (+ 16 (* 2 k)))))))
    a))

(defun %wrt-hstring (ctx text)
  "An HSTRING of TEXT (ASCII), which the caller deletes (%WRT-HSTRING-FREE)."
  (let ((a (+ (aref ctx 0) #x800)))
    (dotimes (k (length text))
      (setf (mem-ref (+ a (* 2 k)) :u8) (char-code (char text k)))
      (setf (mem-ref (+ a (* 2 k) 1) :u8) 0))
    (%wrt-combase ctx 3 a (length text) (wrt-out-clear ctx 31))
    (wrt-out-ref ctx 31)))

(defun %wrt-hstring-free (ctx h) (when (/= h 0) (%wrt-combase ctx 4 h)))

(defun wrt-hstring-text (ctx h)
  "The text of the HSTRING H (ASCII kept, anything else as ?), which is then deleted."
  (if (= h 0)
      ""
      (let* ((p (%wrt-combase ctx 5 h (wrt-out-clear ctx 23)))
             (n (logand (wrt-out-ref ctx 23) #xFFFFFFFF))
             (s (make-string n)))
        (dotimes (k n)
          (let ((c (+ (mem-ref (+ p (* 2 k)) :u8) (* 256 (mem-ref (+ p (* 2 k) 1) :u8)))))
            (setf (char s k) (if (< c 128) (code-char c) #\?))))
        (%wrt-hstring-free ctx h)
        s)))

(defun wrt-factory (ctx class iid)
  "CLASS's activation factory (its statics) as the interface IID; 0 if there is none."
  (let ((h (%wrt-hstring ctx class)))
    (let ((hr (%wrt-hr (%wrt-combase ctx 1 h (wrt-guid ctx 15 iid) (wrt-out-clear ctx 30)))))
      (%wrt-hstring-free ctx h)
      (if (< hr 0) 0 (wrt-out-ref ctx 30)))))

(defun wrt-activate (ctx class)
  "A new instance of CLASS (its default constructor), as IInspectable; 0 if it cannot be made."
  (let ((h (%wrt-hstring ctx class)))
    (let ((hr (%wrt-hr (%wrt-combase ctx 2 h (wrt-out-clear ctx 30)))))
      (%wrt-hstring-free ctx h)
      (if (< hr 0) 0 (wrt-out-ref ctx 30)))))

(defun wrt-qi (ctx obj iid)
  "OBJ as the interface IID (a new reference), or 0."
  (if (= obj 0)
      0
      (let ((hr (wrt-call ctx obj 0 (wrt-guid ctx 14 iid) (wrt-out-clear ctx 29))))
        (if (< hr 0) 0 (wrt-out-ref ctx 29)))))

(defun wrt-release (ctx obj)
  (when (and (integerp obj) (/= obj 0)) (wrt-call ctx obj 2))
  nil)

(defun wrt-op-state (ctx op)
  "OP's state, without waiting: :RUNNING, :DONE, or :FAILED (an error, or cancelled)."
  (let ((info (wrt-qi ctx op +wrt-iid-iasyncinfo+)))
    (if (= info 0)
        :failed
        (let ((hr (wrt-call ctx info +wrt-iasyncinfo-get_status+ (wrt-out-clear ctx 28))))
          (let ((st (logand (wrt-out-ref ctx 28) #xFFFFFFFF)))
            (wrt-release ctx info)
            (cond ((< hr 0) :failed)
                  ((= st 0) :running)          ; Started
                  ((= st 1) :done)             ; Completed
                  (t :failed)))))))            ; Canceled, Error

(defun wrt-op-result (ctx op)
  "A done operation's result (the out value of GetResults), or NIL.  The operation is released."
  (let ((hr (wrt-call ctx op +wrt-iasyncoperation-getresults+ (wrt-out-clear ctx 27))))
    (wrt-release ctx op)
    (if (< hr 0) nil (wrt-out-ref ctx 27))))

;;; ---- buffers ---------------------------------------------------------------------------------

(defconstant +wrt-iid-ibufferbyteaccess+ "ef0f5a9053bcdf118c49001e4fc686da")   ; 905a0fef-bc53-11df-...

(defun %wrt-buffer-factory (ctx)
  (let ((slot (+ (aref ctx 0) #x20000)))
    (when (= (%ww-get64 slot) 0)
      (%ww-put64 slot (wrt-factory ctx "Windows.Storage.Streams.Buffer" +wrt-iid-ibufferfactory+)))
    (%ww-get64 slot)))

(defun %wrt-buffer-data (ctx buf)
  "The address of BUF's bytes (IBufferByteAccess::Buffer), or 0."
  (let ((acc (wrt-qi ctx buf +wrt-iid-ibufferbyteaccess+)))
    (if (= acc 0)
        0
        (let ((hr (wrt-call ctx acc 3 (wrt-out-clear ctx 26))))
          (wrt-release ctx acc)
          (if (< hr 0) 0 (wrt-out-ref ctx 26))))))

(defun wrt-buffer (ctx bytes)
  "A new IBuffer holding BYTES (a vector of octets), or 0."
  (let ((f (%wrt-buffer-factory ctx)) (n (length bytes)))
    (if (= f 0)
        0
        (let ((hr (wrt-call ctx f +wrt-ibufferfactory-create+ (max 1 n) (wrt-out-clear ctx 25))))
          (if (< hr 0)
              0
              (let* ((buf (wrt-out-ref ctx 25)) (data (%wrt-buffer-data ctx buf)))
                (if (= data 0)
                    (progn (wrt-release ctx buf) 0)
                    (progn
                      (dotimes (k n) (setf (mem-ref (+ data k) :u8) (aref bytes k)))
                      (wrt-call ctx buf +wrt-ibuffer-put_length+ n)
                      buf))))))))

(defun wrt-buffer-bytes (ctx buf)
  "BUF's bytes, as a fresh octet vector (empty if it cannot be read)."
  (let ((hr (wrt-call ctx buf +wrt-ibuffer-get_length+ (wrt-out-clear ctx 24))))
    (let* ((n (if (< hr 0) 0 (logand (wrt-out-ref ctx 24) #xFFFFFFFF)))
           (data (if (> n 0) (%wrt-buffer-data ctx buf) 0))
           (v (make-array (if (= data 0) 0 n) :element-type '(unsigned-byte 8))))
      (dotimes (k (length v)) (setf (aref v k) (mem-ref (+ data k) :u8)))
      v)))

;;; ---- event handlers --------------------------------------------------------------------------

(defun wrt-delegate (ctx id iid)
  "A TypedEventHandler object answering to IID; a call to it arrives as (ID SENDER ARGS) from
   WRT-EVENTS.  0 when all 64 are made."
  (let ((k (aref ctx 1)))
    (if (>= k 64)
        0
        (let* ((block (aref ctx 0)) (d (+ block #x1100 (* 48 k))))
          (setf (aref ctx 1) (+ k 1))
          (%ww-put64 d (+ block #x1000))           ; the vtable
          (%ww-put64 (+ d 8) 1)
          (%ww-put64 (+ d 16) id)
          (%ww-put64 (+ d 24) (+ block #x2000))    ; the ring
          (let ((g (wrt-guid ctx 13 iid)))
            (dotimes (b 16) (setf (mem-ref (+ d 32 b) :u8) (mem-ref (+ g b) :u8))))
          d))))

(defun wrt-events (ctx)
  "Every (ID SENDER ARGS) the handlers were called with since the last call, oldest first."
  (let* ((ring (+ (aref ctx 0) #x2000)) (out nil))
    ;; the ring's lock is the native Invoke's: an exchange on its first word
    (loop (when (= 0 (xchg-mem ring 1)) (return nil)))
    (let ((n (%ww-get32 (+ ring 8))))
      (dotimes (i n)
        (let ((e (+ ring 16 (* 24 i))))
          (push (list (%ww-get64 e) (%ww-get64 (+ e 8)) (%ww-get64 (+ e 16))) out)))
      (%ww-put32 (+ ring 8) 0))
    (%ww-put32 ring 0)
    (nreverse out)))
