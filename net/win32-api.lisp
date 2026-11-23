;;;; net/win32-api.lisp -- calling any Windows function from the Windows image.
;;;;
;;;; Compiled into the Windows image only (boot/boot-win64.lisp, *WIN64-PLATFORM-FILES*), before
;;;; the files that use it.  The image imports 58 functions (the IAT, +W32-NAME+); everything
;;;; else in Windows is reached at run time through LoadLibraryA / GetProcAddress and the stub's
;;;; #x3000 call (the function at RDI, RDX qword arguments at RSI, <= 20).  Every other win32-*
;;;; file had grown its own copy of that: a scratch block, a cstring for the name, a hand-laid
;;;; argument vector.  This is the general form.
;;;;
;;;;   (w32-proc "user32.dll" "MessageBoxW")       the function's address, or 0
;;;;   (w32-call FN ARG...)                        call it: (values RESULT LAST-ERROR OUT...)
;;;;   (w32-call* TYPE FN ARG...)                  the same, RESULT read as TYPE
;;;;
;;;; FN is an address, "dll!Name", or ("dll" "Name").  An ARG is
;;;;
;;;;   an integer (a negative one as two's complement), NIL (0), T (1)
;;;;   a string             a NUL-terminated UTF-16 copy (the W functions)
;;;;   (:a STRING)          a NUL-terminated 8-bit copy (the A functions)
;;;;   (:guid TEXT)         a 16-byte GUID, "8-4-4-4-12" (braces allowed)
;;;;   (:double X) (:float X)   a floating-point argument (in XMM0-3 when it is one of the first four)
;;;;   (:out TYPE)          a zeroed 8-byte cell; its value, read as TYPE, is returned as an OUT
;;;;   (:buf N)             N zeroed bytes; returned as an octet vector
;;;;   (:wbuf N)            room for N UTF-16 units; returned as the string up to the first NUL
;;;;   (:bytes OCTETS)      a copy of OCTETS (a struct, say), copied back into OCTETS after the call
;;;;
;;;; OUTs come back in the order their arguments were given.  TYPE (results, :out, W32-PEEK) is
;;;; :u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64 :ptr :bool :hresult :double :float, :raw (the
;;;; 64 bits, unsigned) or :void.  LAST-ERROR is GetLastError right after the call, on the same
;;;; thread; it means something only when the function says it failed.
;;;;
;;;; Also: raw memory (W32-ALLOC, W32-PEEK / W32-POKE, the string readers), COM (W32-COM-CALL,
;;;; W32-CO-CREATE), Windows' own error text (W32-ERROR-TEXT), and callbacks that record what
;;;; Windows calls them with (W32-MAKE-CALLBACK) -- enough for EnumWindows and its kind.
;;;;
;;;; NOTHING IS KEPT BETWEEN CALLS.  On modus a worker thread may not store objects of its own
;;;; anywhere another thread can reach, and a glass app's tick is such a thread.  So W32-CALL
;;;; maps a frame for each call -- the argument vector, every string and cell Windows reads or
;;;; writes, the call thunk -- and unmaps it after; nothing Lisp allocated is ever handed to
;;;; Windows (a collection could move it).  A DLL is looked up with GetModuleHandleA first and
;;;; loaded (LoadLibraryA) only if it is not mapped yet.  A caller in a hot loop should look its
;;;; functions up once (W32-PROC / W32-PROCS) and keep the addresses in its own closure, as
;;;; glass's window does.
;;;;
;;;; THE THUNK.  The stub fills RCX RDX R8 R9 and the stack; Win64 passes a floating-point
;;;; argument in XMM0-3 by position and returns one in XMM0, and the stub's answer is RAX as a
;;;; fixnum, which loses its top bit (1024.0's pattern #x4090... came back as -1024.0).  So
;;;; every call goes through a 57-byte thunk written into its (executable) frame: it copies
;;;; RCX..R9 into XMM0..3 -- the same bits, and harmless for the integer arguments, whose XMM
;;;; registers the callee does not read -- calls the function, and stores RAX and XMM0 in the
;;;; frame, where they are read back whole.  A float past the fourth argument is on the stack
;;;; already, as its bits.
;;;;
;;;; CALLBACKS are native code too, in a page of their own: each call atomically takes the next
;;;; slot of a ring, records RCX RDX R8 R9 there, and returns a fixed value.  No Lisp runs on
;;;; Windows' thread.  Read the ring after the call that made them (W32-CALLBACK-TAKE); a
;;;; callback Windows calls later, from another thread, may be read before its slot is written.

;;; ------------------------------------------------------------------------------------------
;;; Raw memory.  mmap here is VirtualAlloc (boot-win64's shim): zeroed, 64 KB granular.

(defun w32-alloc (n &optional exec)
  "N zeroed bytes of raw memory (EXEC: executable too); their address, or 0."
  (let ((a (syscall6 9 0 (max n 1) (if exec 7 3) 34 -1 0)))
    (if (and (integerp a) (> a 0)) a 0)))

(defun w32-free (a n)
  (unless (= a 0) (syscall6 11 a (max n 1) 0 0 0 0))
  nil)

(defmacro with-w32-memory (((var n) &rest more) &body body)
  "BODY with VAR the address of N zeroed raw bytes, freed on the way out."
  (let ((size (gensym "N")))
    `(let* ((,size ,n) (,var (w32-alloc ,size)))
       (when (= ,var 0) (error "modus: no memory for ~D raw bytes" ,size))
       (unwind-protect
            ,(if more `(with-w32-memory ,more ,@body) `(progn ,@body))
         (w32-free ,var ,size)))))

(defun %w32-u64 (v) (logand v #xFFFFFFFFFFFFFFFF))
(defun %w32-signed (v bits)
  (let ((x (logand v (- (ash 1 bits) 1))))
    (if (logbitp (- bits 1) x) (- x (ash 1 bits)) x)))

(defun %w32-double-bits (x)
  (let ((d (float x 1d0)))
    (+ (* (logand (%float-hi32 d) #xFFFFFFFF) 4294967296) (logand (%float-lo32 d) #xFFFFFFFF))))
(defun %w32-bits-double (u)
  (%make-typed-float (logand (ash u -32) #xFFFFFFFF) (logand u #xFFFFFFFF) 'double-float))
(defun %w32-float-bits (x) (logand (%single->bits (float x 1.0)) #xFFFFFFFF))
(defun %w32-bits-float (u) (%bits->single (logand u #xFFFFFFFF)))

(defun %w32-type-size (type)
  (case type
    ((:u8 :s8) 1) ((:u16 :s16) 2) ((:u32 :s32 :bool :hresult :float) 4)
    (t 8)))

(defun %w32-convert (u type)
  "The unsigned value U (as many bytes as TYPE has) read as TYPE."
  (case type
    ((:raw :u64 :ptr) (%w32-u64 u))
    (:s64 (%w32-signed u 64))
    (:u32 (logand u #xFFFFFFFF))
    ((:s32 :hresult) (%w32-signed u 32))
    (:u16 (logand u #xFFFF))
    (:s16 (%w32-signed u 16))
    (:u8 (logand u #xFF))
    (:s8 (%w32-signed u 8))
    (:bool (/= (logand u #xFFFFFFFF) 0))
    (:double (%w32-bits-double (%w32-u64 u)))
    (:float (%w32-bits-float u))
    (:void nil)
    (t (error "modus: ~S is not a Win32 type" type))))

(defun w32-peek (a type)
  "The TYPE value at address A."
  (let ((n (%w32-type-size type)))
    (%w32-convert (case n
                    (1 (mem-ref a :u8))
                    (2 (+ (mem-ref a :u8) (* 256 (mem-ref (+ a 1) :u8))))
                    (4 (mem-ref a :u32))
                    (t (%ww-get64 a)))
                  type)))

(defun w32-poke (a type v)
  "Store V at address A as TYPE; V."
  (let ((u (case type
             (:double (%w32-double-bits v))
             (:float (%w32-float-bits v))
             (:bool (if v 1 0))
             (t (if (< v 0) (%w32-u64 v) v)))))
    (case (%w32-type-size type)
      (1 (setf (mem-ref a :u8) (logand u #xFF)))
      (2 (%ww-put16 a u))
      (4 (%ww-put32 a u))
      (t (%ww-put64 a u)))
    v))

(defun w32-octets-get (v off type)
  "The TYPE value at byte OFF of the octet vector V (little-endian, as Windows lays a struct)."
  (let ((u 0))
    (dotimes (k (%w32-type-size type))
      (setq u (+ u (ash (aref v (+ off k)) (* 8 k)))))
    (%w32-convert u type)))

(defun w32-octets-put (v off type x)
  "Store X at byte OFF of the octet vector V as TYPE; X."
  (let ((u (case type
             (:double (%w32-double-bits x))
             (:float (%w32-float-bits x))
             (:bool (if x 1 0))
             (t (if (< x 0) (%w32-u64 x) x)))))
    (dotimes (k (%w32-type-size type))
      (setf (aref v (+ off k)) (logand (ash u (* -8 k)) #xFF)))
    x))

;;; Strings.  UTF-16: a character above U+FFFF is a surrogate pair.

(defun %w32-utf16-units (s)
  (let ((n 0))
    (dotimes (i (length s) n)
      (setq n (+ n (if (> (char-code (char s i)) #xFFFF) 2 1))))))

(defun w32-put-wstr (a s)
  "S at A as NUL-terminated UTF-16; the bytes written."
  (let ((p a))
    (dotimes (i (length s))
      (let ((c (char-code (char s i))))
        (if (> c #xFFFF)
            (let ((v (- c #x10000)))
              (%ww-put16 p (+ #xD800 (ash v -10)))
              (%ww-put16 (+ p 2) (+ #xDC00 (logand v #x3FF)))
              (setq p (+ p 4)))
            (progn (%ww-put16 p c) (setq p (+ p 2))))))
    (%ww-put16 p 0)
    (- (+ p 2) a)))

(defun w32-wstr (a &optional max)
  "The NUL-terminated UTF-16 string at A (at most MAX units)."
  (if (= a 0)
      nil
      (let ((out (make-string-output-stream)) (i 0))
        (loop
          (when (and max (>= i max)) (return))
          (let ((u (+ (mem-ref (+ a (* 2 i)) :u8) (* 256 (mem-ref (+ a (* 2 i) 1) :u8)))))
            (when (= u 0) (return))
            (setq i (+ i 1))
            (if (and (>= u #xD800) (< u #xDC00) (or (null max) (< i max)))
                (let ((lo (+ (mem-ref (+ a (* 2 i)) :u8) (* 256 (mem-ref (+ a (* 2 i) 1) :u8)))))
                  (if (and (>= lo #xDC00) (< lo #xE000))
                      (progn (setq i (+ i 1))
                             (write-char (code-char (+ #x10000 (ash (- u #xD800) 10) (- lo #xDC00))) out))
                      (write-char (code-char #xFFFD) out)))
                (write-char (code-char u) out))))
        (get-output-stream-string out))))

(defun w32-put-astr (a s)
  "S at A as NUL-terminated 8-bit text (codes above 255 become ?); the bytes written."
  (dotimes (i (length s))
    (let ((c (char-code (char s i))))
      (setf (mem-ref (+ a i) :u8) (if (> c 255) 63 c))))
  (setf (mem-ref (+ a (length s)) :u8) 0)
  (+ (length s) 1))

(defun w32-astr (a &optional max)
  "The NUL-terminated 8-bit string at A (at most MAX bytes)."
  (if (= a 0)
      nil
      (let ((out (make-string-output-stream)) (i 0))
        (loop
          (when (and max (>= i max)) (return))
          (let ((b (mem-ref (+ a i) :u8)))
            (when (= b 0) (return))
            (write-char (code-char b) out)
            (setq i (+ i 1))))
        (get-output-stream-string out))))

(defun %w32-hex2 (text i) (parse-integer text :start i :end (+ i 2) :radix 16))

(defun w32-put-guid (a text)
  "The GUID TEXT (\"8-4-4-4-12\", braces allowed) at A, in its 16-byte memory layout; A."
  (let ((h (remove #\} (remove #\{ (remove #\- text)))))
    (unless (= (length h) 32) (error "modus: ~S is not a GUID" text))
    ;; Data1, Data2, Data3 little-endian; Data4 as written
    (dotimes (k 4) (setf (mem-ref (+ a k) :u8) (%w32-hex2 h (* 2 (- 3 k)))))
    (dotimes (k 2) (setf (mem-ref (+ a 4 k) :u8) (%w32-hex2 h (+ 8 (* 2 (- 1 k))))))
    (dotimes (k 2) (setf (mem-ref (+ a 6 k) :u8) (%w32-hex2 h (+ 12 (* 2 (- 1 k))))))
    (dotimes (k 8) (setf (mem-ref (+ a 8 k) :u8) (%w32-hex2 h (+ 16 (* 2 k)))))
    a))

(defun w32-guid (a)
  "The GUID at A as \"8-4-4-4-12\" text."
  (flet ((b (k) (mem-ref (+ a k) :u8)))
    (format nil "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~{~2,'0X~}"
            (+ (b 0) (* 256 (b 1)) (* 65536 (b 2)) (* 16777216 (b 3)))
            (+ (b 4) (* 256 (b 5))) (+ (b 6) (* 256 (b 7))) (b 8) (b 9)
            (list (b 10) (b 11) (b 12) (b 13) (b 14) (b 15)))))

;;; ------------------------------------------------------------------------------------------
;;; Finding functions.

(defun w32-module (dll)
  "DLL's module handle -- mapping it if it is not mapped yet -- or 0."
  (with-w32-memory ((name (+ (length dll) 1)))
    (w32-put-astr name dll)
    (let ((h (syscall6 +w32-getmodulehandlea+ name 0 0 0 0 0)))
      (if (and (integerp h) (/= h 0))
          h
          (let ((l (syscall6 +w32-loadlibrarya+ name 0 0 0 0 0)))
            (if (integerp l) l 0))))))

(defun w32-proc (dll name)
  "The address of DLL's function NAME, or 0 (and why, as a second value)."
  (let ((m (w32-module dll)))
    (if (= m 0)
        (values 0 (format nil "~a is not there" dll))
        (with-w32-memory ((a (+ (length name) 1)))
          (w32-put-astr a name)
          (let ((f (syscall6 +w32-getprocaddress+ m a 0 0 0 0)))
            (if (and (integerp f) (/= f 0))
                f
                (values 0 (format nil "~a has no ~a" dll name))))))))

(defun w32-procs (dll names)
  "A vector of the addresses of DLL's functions NAMES, or NIL and the first one missing."
  (let ((v (make-array (length names) :initial-element 0)) (i 0))
    (dolist (n names v)
      (let ((f (w32-proc dll n)))
        (when (= f 0) (return (values nil n)))
        (setf (aref v i) f)
        (setq i (+ i 1))))))

(defun %w32-resolve (fn)
  (cond ((integerp fn) fn)
        ((consp fn)
         (multiple-value-bind (f why) (w32-proc (first fn) (second fn))
           (if (= f 0) (error "modus: ~a" why) f)))
        ((stringp fn)
         (let ((bang (position #\! fn)))
           (unless bang (error "modus: ~S is not \"dll!Name\"" fn))
           (%w32-resolve (list (subseq fn 0 bang) (subseq fn (+ bang 1))))))
        (t (error "modus: ~S is not a Win32 function" fn))))

;;; ------------------------------------------------------------------------------------------
;;; Calling.
;;;
;;; A frame: +#x000 the argument vector (20 qwords)  +#x100 the thunk  +#x180 its return address
;;; +#x188 the function  +#x190 RAX  +#x198 XMM0  +#x200.. the arguments' strings and cells,
;;; 16-byte aligned.

(defconstant +w32-max-args+ 20)

(defun %w32-arg-bytes (a)
  (cond ((stringp a) (* 2 (+ (%w32-utf16-units a) 1)))
        ((consp a)
         (case (first a)
           (:a (+ (length (second a)) 1))
           (:guid 16)
           (:out 8)
           (:buf (second a))
           (:wbuf (* 2 (second a)))
           (:bytes (length (second a)))
           (t 0)))
        (t 0)))

(defun %w32-thunk (frame)
  "The thunk at FRAME+#x100: XMM0-3 from RCX..R9, call [+#x188], RAX to +#x190, XMM0 to +#x198."
  (let ((code '(#x66 #x48 #x0F #x6E #xC1              ;  0 movq xmm0,rcx
                #x66 #x48 #x0F #x6E #xCA              ;  5 movq xmm1,rdx
                #x66 #x49 #x0F #x6E #xD0              ; 10 movq xmm2,r8
                #x66 #x49 #x0F #x6E #xD9              ; 15 movq xmm3,r9
                #x41 #x5A                             ; 20 pop r10  (the return address)
                #x4C #x89 #x15 99 0 0 0               ; 22 mov [rip+99],r10     -> +#x180
                #xFF #x15 101 0 0 0                   ; 29 call [rip+101]       -> +#x188
                ;; (the stack arguments are where the function expects them: one pop, one call)
                #x48 #x89 #x05 102 0 0 0              ; 35 mov [rip+102],rax    -> +#x190
                #x66 #x48 #x0F #x7E #x05 101 0 0 0    ; 42 movq [rip+101],xmm0  -> +#x198
                #xFF #x25 71 0 0 0))                  ; 51 jmp [rip+71]         -> +#x180
        (p (+ frame #x100)) (i 0))
    (dolist (b code)
      (setf (mem-ref (+ p i) :u8) b)
      (setq i (+ i 1)))
    p))

(defun w32-call* (type fn &rest args)
  "Call FN with ARGS (see the top of net/win32-api.lisp); (values RESULT LAST-ERROR OUT...),
   RESULT read as TYPE."
  (when (> (length args) +w32-max-args+)
    (error "modus: a Win32 call takes at most ~D arguments" +w32-max-args+))
  (let ((f (%w32-resolve fn)) (data 0))
    (dolist (a args) (setq data (+ data (logand (+ (%w32-arg-bytes a) 15) -16))))
    (let* ((size (+ #x200 data))
           (frame (w32-alloc size t)))
      (when (= frame 0) (error "modus: no memory for a Win32 call"))
      (unwind-protect
           (let ((p (+ frame #x200)) (k 0) (outs nil))
             (dolist (a args)
               (let ((v (cond ((integerp a) a)
                              ((null a) 0)
                              ((eq a t) 1)
                              ((stringp a) (w32-put-wstr p a) p)
                              ((not (consp a)) (error "modus: ~S is not a Win32 argument" a))
                              (t (case (first a)
                                   (:a (w32-put-astr p (second a)) p)
                                   (:guid (w32-put-guid p (second a)))
                                   (:double (%w32-double-bits (second a)))
                                   (:float (%w32-float-bits (second a)))
                                   ((:out :buf :wbuf) (push (cons a p) outs) p)
                                   (:bytes (let ((o (second a)))
                                             (dotimes (j (length o))
                                               (setf (mem-ref (+ p j) :u8) (aref o j))))
                                           (push (cons a p) outs) p)
                                   (:ptr (second a))
                                   (t (error "modus: ~S is not a Win32 argument" a)))))))
                 (%ww-put64 (+ frame (* 8 k)) v)
                 (setq p (+ p (logand (+ (%w32-arg-bytes a) 15) -16)))
                 (setq k (+ k 1))))
             (%ww-put64 (+ frame #x188) f)
             (syscall3 #x3000 (%w32-thunk frame) frame k)
             (let ((err (logand (syscall6 +w32-getlasterror+ 0 0 0 0 0 0) #xFFFFFFFF))
                   (r (%ww-get64 (+ frame (if (member type '(:double :float)) #x198 #x190))))
                   (vals nil))
               (dolist (o outs)                  ; OUTS is newest first; this builds them in order
                 (let ((spec (car o)) (q (cdr o)))
                   (case (first spec)
                     (:out (push (w32-peek q (second spec)) vals))
                     (:buf (let ((v (make-array (second spec) :element-type '(unsigned-byte 8))))
                             (dotimes (j (second spec)) (setf (aref v j) (mem-ref (+ q j) :u8)))
                             (push v vals)))
                     (:wbuf (push (w32-wstr q (second spec)) vals))
                     (:bytes (let ((v (second spec)))
                               (dotimes (j (length v)) (setf (aref v j) (mem-ref (+ q j) :u8))))))))
               (values-list (list* (%w32-convert r type) err vals))))
        (w32-free frame size)))))

(defun w32-call (fn &rest args)
  "Call FN with ARGS; (values RESULT LAST-ERROR OUT...), RESULT the raw 64 bits of RAX."
  (apply (function w32-call*) :raw fn args))

;;; ------------------------------------------------------------------------------------------
;;; Errors.

(defun w32-error-text (code)
  "Windows' own description of the error (or HRESULT) CODE, or NIL."
  ;; FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS
  (multiple-value-bind (n err text)
      (w32-call* :u32 "kernel32.dll!FormatMessageW" #x1200 0 (logand code #xFFFFFFFF) 0
                 '(:wbuf 1024) 1024 0)
    (declare (ignore err))
    (and (> n 0) (string-right-trim '(#\Space #\Return #\Newline #\Tab #\.) text))))

(defun w32-succeeded-p (hr) (>= (%w32-signed hr 32) 0))

;;; ------------------------------------------------------------------------------------------
;;; COM.  An interface pointer's first word is its vtable; slot 0 QueryInterface, 1 AddRef,
;;; 2 Release, then the interface's own methods in declaration order.

(defun w32-com-method (obj slot)
  (%ww-get64 (+ (%ww-get64 obj) (* 8 slot))))

(defun w32-com-call* (type obj slot &rest args)
  "OBJ's method at vtable SLOT called with OBJ and ARGS; as W32-CALL*."
  (apply (function w32-call*) type (w32-com-method obj slot) obj args))

(defun w32-com-call (obj slot &rest args)
  "OBJ's method at vtable SLOT called with OBJ and ARGS; (values HRESULT LAST-ERROR OUT...)."
  (apply (function w32-call*) :hresult (w32-com-method obj slot) obj args))

(defun w32-com-release (obj)
  (unless (= obj 0) (w32-com-call* :u32 obj 2))
  nil)

(defun w32-com-query (obj iid)
  "OBJ's interface IID (GUID text): (values POINTER HRESULT); POINTER 0 if it has none."
  (multiple-value-bind (hr err p) (w32-com-call obj 0 (list :guid iid) '(:out :ptr))
    (declare (ignore err))
    (values (if (w32-succeeded-p hr) p 0) hr)))

(defun w32-co-initialize (&optional (model 0))
  "CoInitializeEx on this thread: MODEL 0 the multithreaded apartment, 2 a single-threaded one.
   The HRESULT (1, S_FALSE: it already was)."
  (values (w32-call* :hresult "ole32.dll!CoInitializeEx" 0 model)))

(defun w32-co-create (clsid iid &optional (context 23))
  "A new instance of the COM class CLSID, as its interface IID (both GUID text): (values
   POINTER HRESULT); POINTER 0 on failure.  CONTEXT is CLSCTX (23: in-process, local or remote)."
  (multiple-value-bind (hr err p)
      (w32-call* :hresult "ole32.dll!CoCreateInstance" (list :guid clsid) 0 context
                 (list :guid iid) '(:out :ptr))
    (declare (ignore err))
    (values (if (w32-succeeded-p hr) p 0) hr)))

;;; ------------------------------------------------------------------------------------------
;;; Callbacks.  A page: +#x00 the code  +#x80 the call count  +#x88 the value returned
;;; +#x90 the capacity  +#x100 the ring, 32 bytes (RCX RDX R8 R9) a call.

(defun w32-make-callback (&key (returns 1) (capacity 1024))
  "A native function Windows can call (with up to four arguments), recording each call's
   arguments and returning RETURNS (1: TRUE, \"keep going\", for the Enum* functions); its
   address.  Free it with W32-FREE-CALLBACK."
  (unless (and (integerp capacity) (> capacity 0) (< capacity #x1000000))
    (error "modus: a callback records 1 to 16M calls, not ~S" capacity))
  (let* ((size (+ #x100 (* 32 capacity)))
         (page (w32-alloc size t))
         (code (list #x48 #xC7 #xC0 1 0 0 0                  ;  0 mov rax,1
                     #xF0 #x48 #x0F #xC1 #x05 112 0 0 0      ;  7 lock xadd [rip+112],rax  -> +#x80
                     #x48 #x3D                               ; 16 cmp rax,capacity
                     (logand capacity #xFF) (logand (ash capacity -8) #xFF)
                     (logand (ash capacity -16) #xFF) 0
                     #x73 29                                 ; 22 jae +53
                     #x48 #xC1 #xE0 #x05                     ; 24 shl rax,5
                     #x4C #x8D #x15 221 0 0 0                ; 28 lea r10,[rip+221]        -> +#x100
                     #x49 #x01 #xC2                          ; 35 add r10,rax
                     #x49 #x89 #x0A                          ; 38 mov [r10],rcx
                     #x49 #x89 #x52 #x08                     ; 41 mov [r10+8],rdx
                     #x4D #x89 #x42 #x10                     ; 45 mov [r10+16],r8
                     #x4D #x89 #x4A #x18                     ; 49 mov [r10+24],r9
                     #x48 #x8B #x05 76 0 0 0                 ; 53 mov rax,[rip+76]         -> +#x88
                     #xC3))                                  ; 60 ret
         (i 0))
    (when (= page 0) (error "modus: no memory for a callback"))
    (dolist (b code) (setf (mem-ref (+ page i) :u8) b) (setq i (+ i 1)))
    (%ww-put64 (+ page #x88) (%w32-u64 returns))
    (%ww-put64 (+ page #x90) capacity)
    page))

(defun w32-callback-count (cb)
  "How many times CB has been called since it was made or last taken from."
  (%ww-get64 (+ cb #x80)))

(defun w32-callback-take (cb)
  "Each recorded call's arguments, (RCX RDX R8 R9) as unsigned integers, oldest first; the
   ring is emptied.  Calls past the capacity were counted but not recorded."
  (let ((n (min (w32-callback-count cb) (%ww-get64 (+ cb #x90)))) (acc nil))
    (dotimes (i n)
      (let ((e (+ cb #x100 (* 32 i))))
        (push (list (%ww-get64 e) (%ww-get64 (+ e 8)) (%ww-get64 (+ e 16)) (%ww-get64 (+ e 24)))
              acc)))
    (%ww-put64 (+ cb #x80) 0)
    (nreverse acc)))

(defun w32-free-callback (cb)
  (w32-free cb (+ #x100 (* 32 (%ww-get64 (+ cb #x90))))))
