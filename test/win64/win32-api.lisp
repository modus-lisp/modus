;;;; test/win64/win32-api.lisp -- net/win32-api.lisp against Windows itself.
;;;;
;;;;   modus.exe --script test/win64/win32-api.lisp
;;;;
;;;; Every check asks Windows twice, or asks it something with a known answer: a function looked
;;;; up by name against the same import called through the IAT, a string round trip that Windows
;;;; measures (lstrlenW), an error code Windows names, doubles and singles through the C runtime,
;;;; a GUID parsed by both sides, a COM object made and queried, and a callback EnumWindows calls.
;;;; Prints "win32-api: N/M" and exits 0 only if all pass.  MODUS_W32_API_FILE loads a newer
;;;; net/win32-api.lisp over the image's (with the constants it needs), for testing without a
;;;; rebuild.

(in-package "MODUS-INT")

;; The IAT slots the checks below name.  An image's own constants are folded into its code
;; and are not bound for a script, so a script that wants one defines it.
(unless (boundp '+w32-getcurrentprocessid+)
  (eval '(defconstant +w32-getlasterror+ #x100B))
  (eval '(defconstant +w32-getcurrentprocessid+ #x1006))
  (eval '(defconstant +w32-getmodulehandlea+ #x1021))
  (eval '(defconstant +w32-loadlibrarya+ #x1022))
  (eval '(defconstant +w32-getprocaddress+ #x1023)))

;; Likewise an image's macros: WITH-W32-MEMORY is compiled into net/win32-api.lisp's own
;; callers, and a script needs its own copy.
(unless (macro-function 'with-w32-memory)
  (eval '(defmacro with-w32-memory (((var n) &rest more) &body body)
          (let ((size (gensym "N")))
            `(let* ((,size ,n) (,var (w32-alloc ,size)))
               (when (= ,var 0) (error "modus: no memory for ~D raw bytes" ,size))
               (unwind-protect
                    ,(if more `(with-w32-memory ,more ,@body) `(progn ,@body))
                 (w32-free ,var ,size)))))))

(let ((f (%cli-getenv "MODUS_W32_API_FILE")))
  (when (and f (> (length f) 0))
    (load f)
    (let ((time (concatenate 'string (subseq f 0 (- (length f) (length "api.lisp"))) "time.lisp")))
      (load time))))

(defvar *pass* 0)
(defvar *total* 0)
(defmacro check (name form)
  `(progn (setq *total* (+ *total* 1))
          (let ((ok (handler-case ,form (error (c) (format t "    ~a: ~a~%" ,name c) nil))))
            (if ok (setq *pass* (+ *pass* 1)) (format t "FAIL ~a~%" ,name)))))

;; lookup
(check "GetProcAddress finds kernel32's GetTickCount64" (/= 0 (w32-proc "kernel32.dll" "GetTickCount64")))
(check "a missing function is 0 with a reason"
       (multiple-value-bind (f why) (w32-proc "kernel32.dll" "NoSuchFunction") (and (= f 0) (stringp why))))
(check "a missing DLL is 0" (= 0 (w32-proc "no-such-library.dll" "X")))
(check "by name = through the IAT (GetCurrentProcessId)"
       (= (w32-call* :u32 "kernel32.dll!GetCurrentProcessId")
          (logand (syscall6 +w32-getcurrentprocessid+ 0 0 0 0 0 0) #xFFFFFFFF)))
(check "W32-PROCS" (let ((v (w32-procs "user32.dll" '("GetDesktopWindow" "GetSystemMetrics")))) (and v (= 2 (length v)))))

;; strings
(check "lstrlenW counts a surrogate pair as two"
       (= 7 (w32-call* :s32 "kernel32.dll!lstrlenW" (format nil "h~cllo~c" (code-char #xE9) (code-char #x1F600)))))
(check "UTF-16 round trip"
       (let ((s (format nil "a~cb~cc" (code-char #x3B1) (code-char #x1F600))))
         (with-w32-memory ((p 64)) (w32-put-wstr p s) (string= s (w32-wstr p)))))
(check "lstrlenA" (= 5 (w32-call* :s32 "kernel32.dll!lstrlenA" '(:a "hello"))))
(check "GetEnvironmentVariableW into a :wbuf"
       (multiple-value-bind (n err text) (w32-call* :u32 "kernel32.dll!GetEnvironmentVariableW" "SystemRoot" '(:wbuf 260) 260)
         (declare (ignore err))
         (format t "    SystemRoot = ~s~%" text)
         (and (> n 0) (= n (length text)) (string-equal "\\WINDOWS" (subseq text (- n 8))))))

;; out cells, buffers, structs
(check ":out :u64 (QueryPerformanceFrequency)"
       (multiple-value-bind (ok err f) (w32-call* :bool "kernel32.dll!QueryPerformanceFrequency" '(:out :u64))
         (declare (ignore err)) (and ok (> f 0))))
(check ":buf -- GetSystemInfo's page size is 4096"
       (multiple-value-bind (r err b) (w32-call* :void "kernel32.dll!GetSystemInfo" '(:buf 48))
         (declare (ignore r err)) (= 4096 (w32-octets-get b 4 :u32))))
(check ":bytes is copied back -- GetSystemTime's year"
       (let ((st (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
         (w32-call* :void "kernel32.dll!GetSystemTime" (list :bytes st))
         (let ((y (w32-octets-get st 0 :u16))) (format t "    year ~d~%" y) (>= y 2026))))

;; errors
(check "CloseHandle of nothing fails with ERROR_INVALID_HANDLE (6), and Windows names it"
       (multiple-value-bind (ok err) (w32-call* :bool "kernel32.dll!CloseHandle" #x1234)
         (let ((text (w32-error-text err)))
           (format t "    error ~d: ~s~%" err text)
           (and (not ok) (= err 6) (stringp text) (> (length text) 5)))))
(check "W32-ERROR-TEXT of an HRESULT (E_NOINTERFACE)" (stringp (w32-error-text #x80004002)))

;; floats
(check "a double in and out (ntdll sqrt)"
       (let ((r (w32-call* :double "ntdll.dll!sqrt" '(:double 2d0)))) (< (abs (- r 1.4142135623730951d0)) 1d-15)))
(check "two doubles (ntdll pow 2^10)" (= 1024d0 (w32-call* :double "ntdll.dll!pow" '(:double 2d0) '(:double 10d0))))
(check "a single in and out (ucrtbase sqrtf)"
       (let ((r (w32-call* :float "ucrtbase.dll!sqrtf" '(:float 9.0)))) (= r 3.0)))
(check "a double into an integer result (ucrtbase lround)"
       (= 3 (w32-call* :s32 "ucrtbase.dll!lround" '(:double 2.5d0))))
(check "an integer and a double mixed (ucrtbase ldexp 3 * 2^4)"
       (= 48d0 (w32-call* :double "ucrtbase.dll!ldexp" '(:double 3d0) 4)))

(check "a 64-bit result with its top bit set (ucrtbase _byteswap_uint64)"
       (= #x8000000000000001 (w32-call* :u64 "ucrtbase.dll!_byteswap_uint64" #x0100000000000080)))
(check "varargs, a double on the stack (msvcrt _snwprintf)"
       (multiple-value-bind (n err text)
           (w32-call* :s32 "msvcrt.dll!_snwprintf" '(:wbuf 64) 63 "%d %.3f %s" 42 '(:double 3.14159d0) "ok")
         (declare (ignore err))
         (format t "    ~s~%" text)
         (and (= n 11) (equal text "42 3.142 ok"))))

;; GUIDs and COM
(check "a GUID parsed by both sides (CLSIDFromString)"
       (multiple-value-bind (hr err b) (w32-call* :hresult "ole32.dll!CLSIDFromString" "{00021401-0000-0000-C000-000000000046}" '(:buf 16))
         (declare (ignore err))
         (and (= hr 0)
              (with-w32-memory ((p 16))
                (w32-put-guid p "00021401-0000-0000-C000-000000000046")
                (and (equal "00021401-0000-0000-C000-000000000046" (w32-guid p))
                     (let ((same t)) (dotimes (i 16 same) (unless (= (aref b i) (mem-ref (+ p i) :u8)) (setq same nil)))))))))
(check "CoInitializeEx" (w32-succeeded-p (w32-co-initialize)))
(check "CoCreateInstance(ShellLink), QueryInterface(IShellLinkW), a string through two methods"
       (multiple-value-bind (obj hr) (w32-co-create "00021401-0000-0000-C000-000000000046" "00000000-0000-0000-C000-000000000046")
         (format t "    CoCreateInstance #x~8,'0x~%" (logand hr #xFFFFFFFF))
         (and (/= obj 0)
              (let ((sl (w32-com-query obj "000214F9-0000-0000-C000-000000000046")))
                (prog1 (and (/= sl 0)
                            ;; IShellLinkW::SetDescription (slot 7), GetDescription (slot 6)
                            (= 0 (w32-com-call sl 7 "made by modus"))
                            (multiple-value-bind (hr3 err d) (w32-com-call sl 6 '(:wbuf 64) 64)
                              (declare (ignore err))
                              (format t "    description ~s~%" d)
                              (and (= hr3 0) (equal d "made by modus"))))
                  (w32-com-release sl) (w32-com-release obj))))))
(check "QueryInterface for something it is not gives E_NOINTERFACE"
       (let ((obj (w32-co-create "00021401-0000-0000-C000-000000000046" "00000000-0000-0000-C000-000000000046")))
         (multiple-value-bind (p hr2) (w32-com-query obj "11111111-2222-3333-4444-555555555555")
           (w32-com-release obj)
           (and (= p 0) (= (logand hr2 #xFFFFFFFF) #x80004002)))))

;; callbacks
(check "EnumWindows calls a callback, which records its arguments"
       (let ((cb (w32-make-callback :returns 1 :capacity 4096)))
         (unwind-protect
              (let* ((ok (w32-call* :bool "user32.dll!EnumWindows" cb 4242))
                     (n (w32-callback-count cb))
                     (calls (w32-callback-take cb)))
                (format t "    ~d top-level windows~%" n)
                (let ((titled 0))
                  (dolist (c calls)
                    (multiple-value-bind (len err title) (w32-call* :s32 "user32.dll!GetWindowTextW" (first c) '(:wbuf 128) 128)
                      (declare (ignore err))
                      (when (and (> len 0) (< titled 3)) (format t "      ~s~%" title) (setq titled (+ titled 1)))))
                  (and ok (> n 0) (= n (length calls)) (every (lambda (c) (= (second c) 4242)) calls)
                       (= 0 (w32-callback-count cb)))))
           (w32-free-callback cb))))
(check "a callback returning FALSE stops EnumWindows after one call"
       (let ((cb (w32-make-callback :returns 0)))
         (unwind-protect (progn (w32-call* :bool "user32.dll!EnumWindows" cb 0) (= 1 (w32-callback-count cb)))
           (w32-free-callback cb))))

;; net/win32-time.lisp, which is built on this
(check "W32-UTC-OFFSET is whole minutes, within a day"
       (let ((o (w32-utc-offset))) (format t "    UTC offset ~d s~%" o) (and (= 0 (mod o 60)) (< (abs o) 86400))))

(format t "win32-api: ~d/~d~%" *pass* *total*)
(%cli-exit (if (= *pass* *total*) 0 1))
