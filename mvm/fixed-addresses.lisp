;;;; fixed-addresses.lisp — HOST-ONLY registry of every FIXED runtime address
;;;; (low-block words, per-thread window slots, per-arch extension words) and
;;;; the build-time checks that keep two features from claiming the same one.
;;;;
;;;; THIS FILE IS NEVER BAKED INTO AN IMAGE.  lib/load-mvm.lisp loads it right
;;;; after mvm/build-checks.lisp; nothing here changes a byte of any image.
;;;;
;;;; ------------------------------------------------------------------
;;;; WHY
;;;; ------------------------------------------------------------------
;;;; The runtime's fixed words are hand-allocated LITERAL ADDRESSES: argc at
;;;; #x10000200, the hosted scheduler lock at #x10000FC0, the stop-the-world
;;;; flag at #x10000FC8, AArch64's JIT constant-vector root at #x10000FD0, the
;;;; handler-frame extension at window +0x5600, i386's at #x1001FF00, ...
;;;; Nothing checked them.  "Clear of X" in a docstring was a claim nobody
;;;; verified, and three collisions bit in one campaign:
;;;;   1. SMP-INIT zeroed FC0/FC8/FD0 after FC8 had become the STW flag and FD0
;;;;      the AArch64 constvec root (4f12b7e8): every actor bring-up nulled
;;;;      compiled QUOTE loads on AArch64.
;;;;   2. The threads merge silently collided EB8 (eval lock owner) and F18
;;;;      (live per-region table).
;;;;   3. (same shape, other class) a 29-bit name-hash collision made a gensym
;;;;      special (d2541ac).
;;;;
;;;; ------------------------------------------------------------------
;;;; THE MODEL
;;;; ------------------------------------------------------------------
;;;; An entry (DEFINE-FIXED-WORD) is a byte range [ADDR, ADDR+SIZE) claimed in a
;;;; set of MEMORY MAPS.  A map is one image class with one address layout:
;;;;
;;;;   :x64-hosted :x64-bare  :a64-hosted :a64-bare  :rv64-hosted :rv32-hosted
;;;;   :rv-bare  :ppc-hosted :ppc-bare  :i386-hosted :i386-bare
;;;;   :arm32-hosted :arm32-bare  :68k-hosted :68k-bare
;;;;
;;;; and :IMAGES names maps or GROUPS of them (*FIXED-ADDRESS-GROUPS*).  The
;;;; one that matters most is :LOW-BLOCK — every map whose shared convention
;;;; block really is RAM at #x10000000.  Bare RISC-V (NS16550 UART at
;;;; #x10000000), bare ARM32 (versatilepb/virt device space) and bare PowerPC
;;;; (outside the boot TLBs) are NOT in it: their back ends relocate the
;;;; convention slots (*RV-GLOBALS-BASE*, *ARM32-GLOBALS-BASE*,
;;;; *PPC-GLOBALS-BASE*), and the registry records what IS at #x10000000 there
;;;; as a :REGION, so a word claimed for them by mistake collides with it.
;;;;
;;;; SPACE says how the address is reached:
;;;;   :global  a process-wide absolute address (the default).
;;;;   :window  a per-thread window slot (mvm/compiler.lisp THE PER-THREAD
;;;;            WINDOW; translate-aarch64 THE PER-THREAD WINDOW, AARCH64).  ADDR
;;;;            is the MAIN thread's copy; a worker's copy is its block + (ADDR
;;;;            - #x10000000).  It therefore occupies both views.
;;;;   :block   an offset into a WORKER's window block that the main thread
;;;;            never addresses (e.g. the dynamic-binding cache at block+0xC50).
;;;;            ADDR is #x10000000 + offset.  It is checked against :window
;;;;            entries (same block) but not against :global ones (the main
;;;;            thread's bytes at that address are someone else's).
;;;; So there are two VIEWS per map: ABSOLUTE (:global + :window) and BLOCK
;;;; (:window + :block), and the overlap check runs in both.
;;;;
;;;; KIND is documentation plus one rule: :word (a fixed word or small record),
;;;; :range (an array, a frame stack, a table), :region (a heap, an arena, a
;;;; scratch page, device space — not a "word" but it owns the bytes).  All
;;;; three take part in the overlap check equally.  :container is a MAPPING
;;;; rather than a claim (i386's demand-zeroed BSS): anything may lie inside it,
;;;; but an entry that straddles its edge is reported.
;;;;
;;;; NESTING.  :WITHIN names a parent range: the child must lie inside it, and
;;;; may overlap it, but not its siblings.  (The MCGC config block is a range;
;;;; its 25 words are children.)
;;;;
;;;; SANCTIONED SHARING.  :SHARES names entries this one deliberately overlaps
;;;; (with :REASON).  Not an escape hatch for a collision: each one is printed
;;;; on every build, as a near-miss someone chose, and the list is in the docs.
;;;;
;;;; CONSTANTS.  :CONSTANTS lists the host symbols (as "PKG::NAME" strings, so
;;;; this file does not depend on load order) that name the same address.  Each
;;;; one that is BOUND must equal ADDR, or the check fails: the registry and the
;;;; translator cannot drift apart silently.  Unbound ones (a boot descriptor a
;;;; given build never loads) are skipped and re-checked at every BUILD-IMAGE.
;;;;
;;;; ------------------------------------------------------------------
;;;; THE CHECKS (CHECK H here; build-checks.lisp has A..G)
;;;; ------------------------------------------------------------------
;;;; H1 OVERLAP.  In every map, in both views, no two entries intersect unless
;;;;    one is :WITHIN the other or they :SHARE.  Fails the build naming both
;;;;    owners.  Runs when this file loads (every build script loads it via
;;;;    lib/load-mvm.lisp) and again inside BUILD-IMAGE.
;;;; H2 CONSTANTS.  Every bound :CONSTANTS symbol equals its entry's ADDR.
;;;; H3 WINDOW EXTENT.  Every :window/:block entry claimed for a threaded map
;;;;    lies inside that map's worker block (x86-64 0x8000, AArch64 0x11000 —
;;;;    net/hosted-sync.lisp %THR-TLS-BLOCK), or a worker's copy is in the
;;;;    NEXT worker's block (the bug that layout comment records).
;;;; H4 THE RATCHET.  Every #x1000xxxx..#x1002xxxx literal in mvm/ net/ lib/
;;;;    boot/ *.lisp code (comments stripped), and every window-offset literal
;;;;    (+ b #x5600)-style, must fall inside a registered entry (or be its
;;;;    one-past-end bound).  Unregistered ones are compared against
;;;;    mvm/fixed-address-allowlist.lisp, which may only SHRINK: a new
;;;;    unregistered literal, or more of an old one, fails the build.
;;;;
;;;; KNOBS: MODUS_FIXED_ADDR_CHECK=0 (off) | warn (report, never fail) | report
;;;; (also print every map's table).  MODUS_FIXED_ADDR_ALLOWLIST_WRITE=1
;;;; rewrites the allowlist from the tree (use it to SHRINK it after
;;;; registering words; a growing diff in review is the ratchet being cheated).
;;;;
;;;; HOW TO CLAIM A NEW WORD: docs/fixed-addresses.md.

(in-package :modus.mvm)

;;; ------------------------------------------------------------------
;;; Maps and groups
;;; ------------------------------------------------------------------

(defparameter *fixed-address-maps*
  '(:x64-hosted :x64-bare :a64-hosted :a64-bare :rv64-hosted :rv32-hosted :rv-bare
    :ppc-hosted :ppc-bare :i386-hosted :i386-bare :arm32-hosted :arm32-bare
    :68k-hosted :68k-bare :68k-mac :web)
  "Every image memory map the registry knows.  :WEB is the browser image
   (mvm/build-web.lisp), whose linear memory mirrors the low block.  :68K-MAC
   is the bare Macintosh (boot/boot-mac68k.lisp): the hosted 68k low block,
   relocated into the Mac's RAM by CONV-REAL, with no Linux argv to stage --
   its argv area holds the ROM serial drivers' parameter blocks.")

(defparameter *fixed-address-groups*
  '((:all          . :all)
    (:hosted       :x64-hosted :a64-hosted :rv64-hosted :rv32-hosted :ppc-hosted
                   :i386-hosted :arm32-hosted :68k-hosted)
    (:bare         :x64-bare :a64-bare :rv-bare :ppc-bare :i386-bare :arm32-bare :68k-bare)
    ;; Every map whose convention block is RAM at #x10000000 (see THE MODEL).
    (:low-block    :x64-hosted :x64-bare :a64-hosted :a64-bare :rv64-hosted :rv32-hosted
                   :ppc-hosted :i386-hosted :i386-bare :arm32-hosted :68k-hosted :68k-bare :68k-mac :web)
    ;; The two back ends with native threads and a per-thread window.
    (:threads      :x64-hosted :a64-hosted)
    (:x64          :x64-hosted :x64-bare)
    (:a64          :a64-hosted :a64-bare)
    (:x64+a64      :x64-hosted :x64-bare :a64-hosted :a64-bare)
    (:hosted-64    :x64-hosted :a64-hosted :rv64-hosted)
    (:rv-hosted    :rv64-hosted :rv32-hosted)
    ;; The 32-bit hosted ports that share i386's / RV32's low-block layout
    ;; (argv staging, cstr / I/O scratch pages below the allocator).
    (:hosted-32    :i386-hosted :arm32-hosted :rv32-hosted :68k-hosted :ppc-hosted))
  "Group keyword -> maps.  :ALL means every map.")

(defparameter *fixed-address-window-block-size*
  '((:x64-hosted . #x8000) (:a64-hosted . #x11000))
  "Bytes in one worker's window block (net/hosted-sync.lisp %THR-TLS-BLOCK).")

(defun %fa-expand-images (images)
  (let ((out nil))
    (dolist (i (if (listp images) images (list images)))
      (cond ((member i *fixed-address-maps*) (pushnew i out))
            ((assoc i *fixed-address-groups*)
             (let ((g (cdr (assoc i *fixed-address-groups*))))
               (dolist (m (if (eq g :all) *fixed-address-maps* g)) (pushnew m out))))
            (t (error "fixed-addresses: unknown map or group ~S" i))))
    (remove-if-not (lambda (m) (member m out)) *fixed-address-maps*)))

;;; ------------------------------------------------------------------
;;; The registry
;;; ------------------------------------------------------------------

(defclass fixed-word ()
  ((name      :initarg :name      :reader fw-name)
   (addr      :initarg :addr      :reader fw-addr)
   (size      :initarg :size      :reader fw-size)
   (images    :initarg :images    :reader fw-images)   ; as written
   (maps      :initarg :maps      :reader fw-maps)     ; expanded
   (space     :initarg :space     :reader fw-space)
   (base      :initarg :base      :reader fw-base)
   (kind      :initarg :kind      :reader fw-kind)
   (within    :initarg :within    :reader fw-within)
   (shares    :initarg :shares    :reader fw-shares)
   (reason    :initarg :reason    :reader fw-reason)
   (constants :initarg :constants :reader fw-constants)
   (owner     :initarg :owner     :reader fw-owner)
   (doc       :initarg :doc       :reader fw-doc)))

(defmethod print-object ((w fixed-word) s)
  (print-unreadable-object (w s :type t)
    (format s "~A #x~X+~D" (fw-name w) (fw-addr w) (fw-size w))))

(defvar *fixed-words* (make-hash-table :test 'eq) "Name -> FIXED-WORD.")
(defvar *fixed-word-order* nil "Names, in definition order.")

(defun fw-end (w) (+ (fw-addr w) (fw-size w)))

(defun %register-fixed-word (name addr size &key (images :low-block) (space :global)
                                              (base :absolute)
                                              (kind :word) within shares reason
                                              constants owner doc)
  (check-type name symbol)
  (check-type addr (integer 0))
  (check-type size (integer 1))
  (assert (member space '(:global :window :block)) () "~S: bad :space ~S" name space)
  (assert (member kind '(:word :range :region :container)) () "~S: bad :kind ~S" name kind)
  (when (and shares (null reason))
    (error "fixed-addresses: ~S :SHARES ~S without a :REASON" name shares))
  (unless (gethash name *fixed-words*)
    (setf *fixed-word-order* (append *fixed-word-order* (list name))))
  (setf (gethash name *fixed-words*)
        (make-instance 'fixed-word
                       :name name :addr addr :size size :images images
                       :maps (%fa-expand-images images) :space space :kind kind
                       :base base
                       :within within
                       :shares (if (listp shares) shares (list shares))
                       :reason reason :constants constants :owner owner :doc doc))
  name)

(defmacro define-fixed-word (name addr size &rest keys
                             &key images space base kind within shares reason constants owner doc)
  "Claim [ADDR, ADDR+SIZE) for NAME in the maps :IMAGES names.  See the file
   header for SPACE / KIND / WITHIN / SHARES / CONSTANTS.  OWNER is the file
   that reads and writes the word; DOC says what it holds."
  (declare (ignore images space base kind within shares reason constants owner doc))
  ;; Every keyword value is data (lists of maps, constant names, strings), so
  ;; it is quoted; ADDR and SIZE are evaluated, so a size may be (- END START).
  `(%register-fixed-word ',name ,addr ,size
                         ,@(loop for (k v) on keys by #'cddr append (list k `',v))))

(defmacro define-fixed-words (common &body rows)
  "Several entries sharing keyword defaults COMMON.  Each row is
   (NAME ADDR SIZE . KEYS); a row's KEYS override COMMON."
  `(progn
     ,@(loop for (name addr size . keys) in rows
             collect `(define-fixed-word ,name ,addr ,size ,@keys ,@common))))

(defun fixed-word (name) (gethash name *fixed-words*))

(defun all-fixed-words () (mapcar #'fixed-word *fixed-word-order*))

;;; ------------------------------------------------------------------
;;; Mode
;;; ------------------------------------------------------------------

(defun %fa-getenv (name)
  "getenv(3) DIRECTLY, not SB-EXT:POSIX-GETENV.  lib/image-config.lisp wraps
   POSIX-GETENV to record every MODUS_* knob a build reads INTO the image, so
   merely reading these two would add two entries to every image's
   configuration record.  They cannot change a byte of an image -- the check
   passes a build or stops it -- so, like output paths, they stay out of the
   record, and a build with this file is byte-identical to one without it."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "getenv" (function sb-alien:c-string sb-alien:c-string))
   name))

(defun %fa-mode ()
  (let ((v (%fa-getenv "MODUS_FIXED_ADDR_CHECK")))
    (cond ((null v) :error)
          ((string= v "0") :off)
          ((string-equal v "warn") :warn)
          ((string-equal v "report") :report)
          (t :error))))

(defun %fa-fail (fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    ;; Printed here as well as in the error: a build log is read from the
    ;; bottom, and the error report can be buried under a backtrace.
    (format t "~&~A~%" msg) (finish-output)
    (unless (eq (%fa-mode) :warn)
      ;; GCK-CHECK-FAILED (build-checks.lisp) is the condition a BUILD-IMAGE
      ;; check uses to say "findings": %GCK-GUARD lets it through, where it
      ;; would report and step over any other error as a broken walker.
      (if (find-class 'gck-check-failed nil)
          (error 'gck-check-failed :format-control "~A" :format-arguments (list msg))
          (error "~A" msg)))))

;;; ------------------------------------------------------------------
;;; H1 OVERLAP / H2 CONSTANTS / H3 WINDOW EXTENT
;;; ------------------------------------------------------------------

(defun %fa-ancestor-p (a b)
  "T when entry A is B or lies (transitively) :WITHIN B."
  (loop for w = a then (and (fw-within w) (fixed-word (fw-within w)))
        while w
        thereis (eq w b)))

(defun %fa-contained-p (c w)
  "T when C is a :CONTAINER and W lies wholly inside it."
  (and (eq (fw-kind c) :container)
       (<= (fw-addr c) (fw-addr w)) (<= (fw-end w) (fw-end c))))

(defun %fa-shared-p (a b)
  (or (member (fw-name b) (fw-shares a)) (member (fw-name a) (fw-shares b))))

(defun %fa-views ()
  "Every (BASE . VIEW) the registry has entries in."
  (let ((v (list '(:absolute . :absolute) '(:absolute . :block))))
    (dolist (w (all-fixed-words))
      (unless (eq (fw-base w) :absolute)
        (pushnew (cons (fw-base w) :offset) v :test #'equal)))
    (reverse v)))

(defun %fa-in-view-p (w view)
  (destructuring-bind (base . v) view
    (and (eq (fw-base w) base)
         (ecase v
           (:absolute (member (fw-space w) '(:global :window)))
           (:block    (member (fw-space w) '(:window :block)))
           (:offset   t)))))

(defun %fa-describe (w)
  (format nil "~A [#x~8,'0X, #x~8,'0X) ~(~A~)~A ~(~A~), owner ~A~@[ -- ~A~]"
          (fw-name w) (fw-addr w) (fw-end w) (fw-space w)
          (if (eq (fw-base w) :absolute) "" (format nil " of ~(~A~)" (fw-base w)))
          (fw-kind w) (or (fw-owner w) "?")
          (and (fw-doc w) (let ((d (substitute #\Space #\Newline (fw-doc w))))
                            (subseq d 0 (min 90 (length d)))))))

(defun fixed-address-overlaps ()
  "List of (MAP VIEW A B) for every unsanctioned overlap, and as a second value
   every SANCTIONED one (same shape) — the near-misses someone chose."
  (let ((bad nil) (shared nil) (seen (make-hash-table :test 'equal)))
    (dolist (map *fixed-address-maps*)
      (dolist (view (%fa-views))
        (let ((ws (sort (remove-if-not (lambda (w) (and (member map (fw-maps w))
                                                        (%fa-in-view-p w view)))
                                       (all-fixed-words))
                        #'< :key #'fw-addr)))
          (loop for (a . rest) on ws do
            (loop for b in rest
                  while (< (fw-addr b) (fw-end a))
                  do (unless (or (%fa-ancestor-p a b) (%fa-ancestor-p b a)
                                 (%fa-contained-p a b) (%fa-contained-p b a))
                       (let ((key (list (fw-name a) (fw-name b))))
                         (if (%fa-shared-p a b)
                             (unless (gethash (list* :s map key) seen)
                               (setf (gethash (list* :s map key) seen) t)
                               (push (list map view a b) shared))
                             (unless (gethash key seen)
                               (setf (gethash key seen) t)
                               (push (list map view a b) bad))))))))))
    (values (nreverse bad) (nreverse shared))))

(defun %fa-parse-symbol (s)
  "\"PKG::NAME\" -> the symbol, or NIL when the package or symbol is absent."
  (let* ((p (search "::" s))
         (pkg (find-package (subseq s 0 p))))
    (and pkg (find-symbol (subseq s (+ p 2)) pkg))))

(defun fixed-address-constant-problems ()
  "Values: (problems resolved unresolved)."
  (let ((problems nil) (resolved 0) (unresolved 0))
    (dolist (w (all-fixed-words))
      (dolist (c (fw-constants w))
        (let ((sym (%fa-parse-symbol c)))
          (if (and sym (boundp sym))
              (let ((v (symbol-value sym)))
                (incf resolved)
                (unless (eql v (fw-addr w))
                  (push (format nil "~A = ~A, but the registry says ~A is at #x~8,'0X"
                                c (if (integerp v) (format nil "#x~8,'0X" v) v)
                                (fw-name w) (fw-addr w))
                        problems)))
              (progn (incf unresolved)
                     (when (eq (%fa-mode) :report)
                       (format t ";;   unbound here: ~A (~A)~%" c (fw-name w))))))))
    (values (nreverse problems) resolved unresolved)))

(defun fixed-address-extent-problems ()
  (let ((problems nil))
    (dolist (w (all-fixed-words))
      (when (and (eq (fw-base w) :absolute) (member (fw-space w) '(:window :block)))
        (dolist (m (fw-maps w))
          (let ((size (cdr (assoc m *fixed-address-window-block-size*))))
            (when (and size (> (- (fw-end w) #x10000000) size))
              (push (format nil "~A ends at window offset #x~X, past ~(~A~)'s ~
                                 #x~X-byte worker block: a worker's copy lands in ~
                                 the next worker's block" (fw-name w)
                            (- (fw-end w) #x10000000) m size)
                    problems))))))
    (nreverse problems)))

(defun fixed-address-within-problems ()
  (let ((problems nil))
    (dolist (w (all-fixed-words))
      (let ((p (and (fw-within w) (fixed-word (fw-within w)))))
        (cond ((and (fw-within w) (null p))
               (push (format nil "~A is :WITHIN ~A, which is not registered"
                             (fw-name w) (fw-within w)) problems))
              ((and p (or (< (fw-addr w) (fw-addr p)) (> (fw-end w) (fw-end p))))
               (push (format nil "~A [#x~X, #x~X) is not inside its parent ~A [#x~X, #x~X)"
                             (fw-name w) (fw-addr w) (fw-end w)
                             (fw-name p) (fw-addr p) (fw-end p)) problems))
              ((and p (set-difference (fw-maps w) (fw-maps p)))
               (push (format nil "~A claims ~(~{~A~^ ~}~), where its parent ~A is not claimed"
                             (fw-name w) (set-difference (fw-maps w) (fw-maps p))
                             (fw-name p)) problems)))
        (dolist (s (fw-shares w))
          (unless (fixed-word s)
            (push (format nil "~A :SHARES ~A, which is not registered" (fw-name w) s)
                  problems)))))
    (nreverse problems)))

(defvar *fa-sanctioned-printed* nil)

(defun check-fixed-address-registry (&key (where "load"))
  "CHECK H1-H3.  Fails loudly on any overlap, drifted constant, or window entry
   outside its worker block."
  (when (eq (%fa-mode) :off) (return-from check-fixed-address-registry nil))
  (multiple-value-bind (bad shared) (fixed-address-overlaps)
    (multiple-value-bind (cproblems resolved unresolved) (fixed-address-constant-problems)
      (let ((eproblems (fixed-address-extent-problems))
            (wproblems (fixed-address-within-problems)))
        (format t "~&;; check-fixed-addresses (~A): ~D entr~:@P, ~D overlap~:P, ~
                   ~D sanctioned, ~D/~D constants bound and checked~%"
                where (length *fixed-word-order*) (length bad) (length shared)
                resolved (+ resolved unresolved))
        (when (and shared (not *fa-sanctioned-printed*))
          ;; One line per pair, its maps listed; the reasons are in the
          ;; registry and in docs/fixed-addresses.md.
          (setf *fa-sanctioned-printed* t)
          (let ((pairs nil))
            (dolist (sh shared)
              (destructuring-bind (map view a b) sh
                (declare (ignore view))
                (let ((e (assoc (cons (fw-name a) (fw-name b)) pairs :test #'equal)))
                  (if e (pushnew map (cdr e)) (push (list (cons (fw-name a) (fw-name b)) map) pairs)))))
            (format t ";;   sanctioned sharing (near-misses by design, see :REASON):~%")
            (dolist (p (reverse pairs))
              (format t ";;     ~(~A <-> ~A~)  [~(~{~A~^ ~}~)]~%" (car (car p)) (cdr (car p))
                      (reverse (cdr p))))))
        (when (eq (%fa-mode) :report) (fixed-address-report))
        (finish-output)
        (when (or bad cproblems eproblems wproblems)
          (%fa-fail "~&~%*** check-fixed-addresses: the fixed-address registry is inconsistent ***~%~
~@[~%TWO FEATURES CLAIM THE SAME BYTES (mvm/fixed-addresses.lisp):~%~{~A~}~]~
~@[~%A CONSTANT DISAGREES WITH THE REGISTRY:~%~{  - ~A~%~}~]~
~@[~%A WINDOW WORD IS OUTSIDE ITS WORKER BLOCK:~%~{  - ~A~%~}~]~
~@[~%MALFORMED ENTRIES:~%~{  - ~A~%~}~]~
~%Move one of them, or -- only if the sharing is deliberate and safe -- declare~%~
it with :SHARES and a :REASON.  docs/fixed-addresses.md says how to claim a word.~%~
MODUS_FIXED_ADDR_CHECK=warn downgrades this to a report.~%"
                    (mapcar (lambda (o)
                              (destructuring-bind (map view a b) o
                                (format nil "  - in ~(~A~) (~:[~(~A~) ~;~*~]~(~A~) view):~%      ~A~%      ~A~%"
                                        map (eq (car view) :absolute) (car view) (cdr view)
                                        (%fa-describe a) (%fa-describe b))))
                            bad)
                    cproblems eproblems wproblems))
        (values bad shared)))))

(defun fixed-address-report (&optional (stream *standard-output*))
  "Print every map's sorted table (MODUS_FIXED_ADDR_CHECK=report)."
  (dolist (map *fixed-address-maps*)
    (format stream "~&;; ---- ~(~A~) ----~%" map)
    (dolist (w (sort (remove-if-not (lambda (w) (member map (fw-maps w))) (all-fixed-words))
                     #'< :key #'fw-addr))
      (format stream ";;   #x~8,'0X ~6D ~7A ~6A ~A~@[ < ~A~]~%" (fw-addr w) (fw-size w)
              (string-downcase (fw-space w)) (string-downcase (fw-kind w))
              (fw-name w) (fw-within w)))))

;;; ------------------------------------------------------------------
;;; H4 THE RATCHET: unregistered literal uses
;;; ------------------------------------------------------------------
;;; A TEXT scan, like CHECK F: it has to see source as WRITTEN, including image
;;; source inside build-script string literals.  Comments (; and #| |#) are
;;; stripped; strings are kept, so a docstring that names an unregistered
;;; address counts too (they almost always name registered ones).
;;;
;;; WHAT IS SCANNED:
;;;   absolute  every #xHEX literal in [#x10000001, #x10030000) -- the low block,
;;;             the per-thread window and every per-port extension word.  Code
;;;             and heap bases above that (#x10100000 bare i386 block,
;;;             #x10400000 AArch64 per-record windows, ...) are not fixed words
;;;             and are not scanned.  #x10000000 itself is the window/convention
;;;             base, used as a base everywhere.
;;;   offset    (+ BASE #xHEX) where BASE is one of *FA-OFFSET-BASES*: a form
;;;             that names a block with a fixed layout (a worker's window block,
;;;             the thread table).  The variable-named rows are per file:
;;;             `b' means a window block in net/hosted-sync.lisp and nothing in
;;;             particular elsewhere.
;;; A literal is COVERED when it lies inside a registered entry for that base
;;; (absolute literals: :global or :window entries; window offsets: :window or
;;; :block entries) in ANY map, or equals an entry's one-past-end bound.  The
;;; ratchet does not check the MAP -- that is H1's job, from the registry.

(defparameter *fa-scan-dirs* '("mvm" "net" "lib" "boot"))

(defparameter *fa-scan-exclusions* '("fixed-addresses" "fixed-address-allowlist")
  "Pathname-names not scanned: the registry and its allowlist name every address.")

(defparameter *fa-offset-bases*
  '(("(%tls-self-base)" :window)
    ("(%dynb-block)"    :window)
    ("(%thr-table)"     :thr-table)
    ("blk" :window ("mvm/prelude.lisp"))
    ("b"   :window ("net/hosted-sync.lisp") ("%tls-prepare-block" "%tls-install"))
    ("b"   :window ("net/hosted-actor-runtime.lisp") ("%ar-prepare-window")))
  "(BASE-TEXT BASE &optional FILES DEFUNS): `(+ BASE-TEXT #xOFF)' is an offset
   into BASE (:WINDOW = a worker's window block, i.e. #x10000000 + OFF in the
   main view).  FILES / DEFUNS limit a variable-name row to where that name
   means that block: `b' is a window block in %TLS-PREPARE-BLOCK and a region
   control block, a scheduler block or a scratch page elsewhere.")

(defparameter *fa-allowlist-path* "mvm/fixed-address-allowlist.lisp")

(defvar *fa-ratchet-done* nil "Tree-wide: once per SBCL process.")

(defun %fa-root ()
  (let ((s (find-symbol "*MODUS-BASE*" "CL-USER")))
    (and s (boundp s) (pathname (symbol-value s)))))

(defun %fa-read-file (path)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let* ((buf (make-string (file-length s)))
           (n (read-sequence buf s)))
      (subseq buf 0 n))))

(defun %fa-strip-comments (text)
  "TEXT with every ; and #| |# comment replaced by spaces (newlines kept, so
   positions still map to lines).  Strings and character literals are honoured
   so `\";\"' and `#\;' do not start a comment."
  (let* ((n (length text)) (out (copy-seq text)) (i 0) (state :code) (depth 0))
    (flet ((blank (j) (unless (char= (char out j) #\Newline) (setf (char out j) #\Space))))
      (loop while (< i n) do
        (let ((c (char text i)))
          (ecase state
            (:code
             (cond ((char= c #\;) (setf state :line) (blank i))
                   ((char= c #\") (setf state :string))
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
                    (incf i 2))      ; #\x : skip the character itself
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                    (setf state :block depth 1) (blank i) (blank (1+ i)) (incf i))))
            (:line (if (char= c #\Newline) (setf state :code) (blank i)))
            (:string (cond ((char= c #\\) (incf i))
                           ((char= c #\") (setf state :code))))
            (:block
             (cond ((and (char= c #\|) (< (1+ i) n) (char= (char text (1+ i)) #\#))
                    (blank i) (blank (1+ i)) (incf i)
                    (when (zerop (decf depth)) (setf state :code)))
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                    (blank i) (blank (1+ i)) (incf i) (incf depth))
                   (t (blank i))))))
        (incf i)))
    out))

(defun %fa-hex-at (text i)
  "If TEXT has #xHEX at I, values (value end)."
  (let ((n (length text)))
    (when (and (< (+ i 2) n) (char= (char text i) #\#) (char-equal (char text (1+ i)) #\x)
               (digit-char-p (char text (+ i 2)) 16)
               (or (zerop i) (not (alphanumericp (char text (1- i))))))
      (let ((j (+ i 2)))
        (loop while (and (< j n) (digit-char-p (char text j) 16)) do (incf j))
        (unless (and (< j n) (or (alphanumericp (char text j)) (char= (char text j) #\-)))
          (values (parse-integer text :start (+ i 2) :end j :radix 16) j))))))

(defun %fa-scan-text (text rel)
  "List of (KIND VALUE) uses in TEXT (already comment-stripped).  KIND is
   :ABS for an absolute literal, or the base keyword for an offset literal."
  (let ((uses nil) (n (length text)) (defun-name nil)
        (bases (remove-if-not (lambda (row) (or (null (third row))
                                                (member rel (third row) :test #'string=)))
                              *fa-offset-bases*)))
    (dotimes (i n)
      ;; the enclosing top-level DEFUN, for the DEFUNS column of a base row
      (when (and (char= (char text i) #\() (or (zerop i) (char= (char text (1- i)) #\Newline))
                 (< (+ i 7) n) (string-equal "(defun " text :start2 i :end2 (+ i 7)))
        (let ((e (position-if (lambda (c) (member c '(#\Space #\Newline #\( #\)))) text
                              :start (+ i 7))))
          (setf defun-name (string-downcase (subseq text (+ i 7) (or e n))))))
      (when (char= (char text i) #\#)
        (multiple-value-bind (v end) (%fa-hex-at text i)
          (declare (ignore end))
          (when (and v (< #x10000000 v #x10030000))
            (push (list :abs v) uses))))
      (when (and (char= (char text i) #\() (< (+ i 3) n)
                 (char= (char text (1+ i)) #\+) (char= (char text (+ i 2)) #\Space))
        (let ((k (+ i 3)))
          (dolist (row bases)
            (let* ((bt (first row)) (e (+ k (length bt))))
              (when (and (<= (1+ e) n) (string= bt text :start2 k :end2 e)
                         (or (null (fourth row))
                             (member defun-name (fourth row) :test #'string-equal))
                         (char= (char text e) #\Space))
                (multiple-value-bind (v end) (%fa-hex-at text (1+ e))
                  (when (and v (< end n) (char= (char text end) #\))
                             (< v #x20000))
                    (push (list (second row) v) uses)))))))))
    uses))

(defun %fa-covered-p (kind value)
  "A use is covered by a WORD or RANGE that contains it, or by ANY entry it is
   the first or one-past-last byte of.  Not by lying somewhere inside a REGION
   or CONTAINER: a literal in the middle of a heap is not a registered word,
   and bare x86-64's heap starts at #x10001000 -- covering 'inside' would make
   the ratchet blind to everything above it."
  (flet ((hit (w a)
           (or (= a (fw-addr w)) (= a (fw-end w))
               (and (member (fw-kind w) '(:word :range))
                    (<= (fw-addr w) a) (< a (fw-end w))))))
    (if (eq kind :abs)
        (some (lambda (w) (and (eq (fw-base w) :absolute)
                               (member (fw-space w) '(:global :window))
                               (hit w value)))
              (all-fixed-words))
        (let ((a (if (eq kind :window) (+ #x10000000 value) value))
              (base (if (eq kind :window) :absolute kind)))
          (some (lambda (w) (and (eq (fw-base w) base)
                                 (or (not (eq base :absolute))
                                     (member (fw-space w) '(:window :block)))
                                 (hit w a)))
                (all-fixed-words))))))

(defun fixed-address-unregistered-uses ()
  "Hash (FILE KIND VALUE) -> count of every uncovered use in the tree."
  (let ((root (%fa-root)) (table (make-hash-table :test 'equal)))
    (dolist (d *fa-scan-dirs*)
      (dolist (p (sort (directory (merge-pathnames
                                   (make-pathname :directory (list :relative d)
                                                  :name :wild :type "lisp")
                                   root))
                       #'string< :key #'namestring))
        (unless (member (pathname-name p) *fa-scan-exclusions* :test #'string-equal)
          (let* ((rel (format nil "~A/~A.lisp" d (pathname-name p)))
                 (text (%fa-strip-comments (%fa-read-file p))))
            (dolist (u (%fa-scan-text text rel))
              (unless (%fa-covered-p (first u) (second u))
                (incf (gethash (list rel (first u) (second u)) table 0))))))))
    table))

(defun %fa-load-allowlist ()
  (let ((p (merge-pathnames *fa-allowlist-path* (%fa-root))))
    (if (probe-file p)
        (with-open-file (s p) (let ((*read-base* 10)) (read s)))
        nil)))

(defun %fa-write-allowlist (table)
  (let ((rows nil))
    (maphash (lambda (k v) (push (append k (list v)) rows)) table)
    (setf rows (sort rows (lambda (a b)
                            (or (string< (first a) (first b))
                                (and (string= (first a) (first b))
                                     (or (string< (string (second a)) (string (second b)))
                                         (and (eq (second a) (second b))
                                              (< (third a) (third b)))))))))
    (with-open-file (s (merge-pathnames *fa-allowlist-path* (%fa-root))
                       :direction :output :if-exists :supersede)
      (format s ";;;; fixed-address-allowlist.lisp — the RATCHET's debt register (CHECK H4).~%~
;;;;~%~
;;;; Literal uses of fixed addresses that are NOT in the registry~%~
;;;; (mvm/fixed-addresses.lisp), as (FILE KIND VALUE COUNT).  KIND :ABS is an~%~
;;;; absolute #x1000xxxx literal; :WINDOW / :THR-TABLE an offset literal~%~
;;;; (+ BASE #xOFF).  A use not listed here, or more uses than COUNT, fails the~%~
;;;; build.  THIS LIST MAY ONLY SHRINK: register the word instead of adding a~%~
;;;; row.  Regenerate (to shrink) with MODUS_FIXED_ADDR_ALLOWLIST_WRITE=1.~%~
;;;; Data, not code: read with READ, never loaded or baked.~%~%(~%")
      (dolist (r rows)
        (format s " (~S ~S #x~X ~D)~%" (first r) (second r) (third r) (fourth r)))
      (format s ")~%"))
    (length rows)))

(defun check-fixed-address-ratchet ()
  "CHECK H4."
  (when (or (eq (%fa-mode) :off) *fa-ratchet-done* (null (%fa-root)))
    (return-from check-fixed-address-ratchet nil))
  (setf *fa-ratchet-done* t)
  (let* ((table (fixed-address-unregistered-uses))
         (allow (%fa-load-allowlist))
         (allowed (make-hash-table :test 'equal))
         (new nil) (shrunk nil) (total 0))
    (when (equal (%fa-getenv "MODUS_FIXED_ADDR_ALLOWLIST_WRITE") "1")
      (format t "~&;; check-fixed-addresses: wrote ~D allowlist row~:P to ~A~%"
              (%fa-write-allowlist table) *fa-allowlist-path*)
      (return-from check-fixed-address-ratchet nil))
    (dolist (r allow)
      (setf (gethash (list (first r) (second r) (third r)) allowed) (fourth r)))
    (maphash (lambda (k v)
               (incf total v)
               (let ((a (gethash k allowed 0)))
                 (when (> v a) (push (list k v a) new))))
             table)
    (maphash (lambda (k a)
               (let ((v (gethash k table 0)))
                 (when (< v a) (push (list k v a) shrunk))))
             allowed)
    (format t "~&;; check-fixed-addresses ratchet: ~D unregistered literal use~:P ~
               (~D distinct), allowlist ~D row~:P~%"
            total (hash-table-count table) (length allow))
    (when shrunk
      (format t ";;   the allowlist can SHRINK by ~D row~:P (MODUS_FIXED_ADDR_ALLOWLIST_WRITE=1):~%"
              (length shrunk))
      (dolist (s (subseq shrunk 0 (min 10 (length shrunk))))
        (destructuring-bind ((file kind v) now was) s
          (format t ";;     ~A ~(~A~) #x~X: ~D now, ~D allowed~%" file kind v now was))))
    (finish-output)
    (when new
      (%fa-fail "~&~%*** check-fixed-addresses ratchet: ~D NEW unregistered fixed-address ~
use~:P ***~%~{  - ~A~%~}~%~
A literal fixed address (or window offset) that no entry in mvm/fixed-addresses.lisp~%~
covers.  Register the word -- DEFINE-FIXED-WORD with its size, maps and owner --~%~
so the overlap check can see it.  docs/fixed-addresses.md says how.  Do not grow~%~
mvm/fixed-address-allowlist.lisp: it may only shrink.~%"
                (length new)
                (mapcar (lambda (x)
                          (destructuring-bind ((file kind v) now was) x
                            (format nil "~A: ~(~A~) #x~X (~D use~:P, ~D allowed)"
                                    file kind v now was)))
                        (sort new #'string< :key #'caar))))
    (values new shrunk)))

(defun check-fixed-addresses (&key (where "build"))
  "Every fixed-address check: H1-H3 over the registry, H4 over the tree."
  (check-fixed-address-registry :where where)
  (check-fixed-address-ratchet))

;;; ==================================================================
;;; THE REGISTRY
;;; ==================================================================
;;; Sorted by address within each block.  Sizes are BYTES.  Where a word's
;;; owner is a host constant it is listed in :CONSTANTS and checked.  A
;;; DEFPARAMETER that a target installer re-points (+MV-COUNT-ADDR+,
;;; *RV-GLOBALS-BASE*, *I386-HSTACK-BASE-ADDR*, ...) is deliberately NOT
;;; listed: its value depends on which target is installed, so it is described
;;; in :DOC instead.

;;; ---- 0x000-0x0FF: GC control block, globals, multiple values ----------

(define-fixed-word :conv-base-page #x10000000 #x40
  :kind :range :doc "Below the GC block: no word is claimed here today (the window
   base itself is used as a base, never as a word)."
  :owner "mvm/compiler.lisp"
  :constants ("MODUS-INT::+TLS-WINDOW-BASE+" "MODUS-INT::+CONV-REGION-BASE+"
              "MODUS-INT::+A64-CONV-BASE+" "MODUS-INT::+RV-HOSTED-CONVENTION-BASE+"
              "MODUS-INT::+LINUX-X64-GLOBALS+"))

(define-fixed-word :gc-region-0-block #x10000040 #x40
  :kind :range :owner "mvm/compiler.lisp (+GC-OFF-*+), mvm/gc.lisp"
  :doc "Region 0's 64-byte Cheney control block (every port's collector)."
  :constants ("MODUS-INT::+GC-REGION-0-BASE+" "MODUS-INT::+68K-GC-META+"
              "MODUS-INT::+PPC-GC-META+" "MODUS-INT::+RV-HOSTED-GC-META+"))

(define-fixed-words (:within :gc-region-0-block :owner "mvm/compiler.lisp")
  (:gc-from-start  #x10000040 8 :constants ("MODUS-INT::+GC-FROM-START-ADDR+"))
  (:gc-to-start    #x10000048 8 :constants ("MODUS-INT::+GC-TO-START-ADDR+"))
  (:gc-space-size  #x10000050 8 :constants ("MODUS-INT::+GC-SPACE-SIZE-ADDR+"))
  (:gc-stack-base  #x10000058 8 :constants ("MODUS-INT::+GC-STACK-BASE-ADDR+"))
  (:gc-count       #x10000060 8 :constants ("MODUS-INT::+GC-COUNT-ADDR+"))
  (:gc-saved-sp    #x10000068 8 :constants ("MODUS-INT::+GC-SAVED-SP-ADDR+"))
  (:gc-saved-alloc #x10000070 8 :constants ("MODUS-INT::+GC-SAVED-ALLOC-ADDR+"))
  (:gc-saved-limit #x10000078 8 :constants ("MODUS-INT::+GC-SAVED-LIMIT-ADDR+")))

(define-fixed-words (:owner "mvm/prelude.lisp, lib/save-image.lisp")
  (:globals-alist      #x10000080 8 :doc "Global variable table head (alist).")
  (:symbol-table       #x10000088 8 :doc "Symbol intern table."))

(define-fixed-word :mv-count #x10000090 8 :space :window
  :owner "mvm/target.lisp (+MV-COUNT-ADDR+, per target)"
  :doc "Multiple-values COUNT.  +MV-COUNT-ADDR+ is a per-target DEFPARAMETER:
   bare RISC-V and ppc32 point it elsewhere (not RAM there).")

(define-fixed-word :mv-values #x10000098 #xA8 :space :window :kind :range
  :owner "mvm/target.lisp (+MV-VALUES-ADDR+), mvm/compiler.lisp"
  :doc "Multiple-values extras: 21 words, #x10000098..#x1000013F.")

(define-fixed-word :gc-scratch-historic #x10000100 #x20 :kind :range
  :owner "mvm/gc.lisp (%GC-SCRATCH-CELL)"
  :doc "The Lisp collector's 32-byte per-collection scratch block when no per-CPU
   array is installed (+GC-SCRATCH-CFG-ADDR+ = 0): forwarded value, tmp-free,
   to-end, pad."
  :shares (:mv-values)
  :reason "Lies on MV extras 13..16.  The Lisp collector (%GC-COLLECT) runs
   only on the aarch64 shim path / targets without a native collector, none of
   which reads a 14th+ value across an allocation.  KNOWN NEAR-MISS:
   docs/fixed-addresses.md.")

;;; ---- 0x140-0x1FF: closure env, intern tables, code bounds, handler frame --

(define-fixed-words (:owner "mvm/compiler.lisp, mvm/prelude.lisp")
  (:closure-env        #x10000140 8 :constants ("MODUS-INT::+CLOSURE-ENV-ADDR+")
                       :doc "Closure environment slot (x86-64 passes it in R13).")
  (:keyword-table      #x10000148 8 :doc "Keyword intern table.")
  (:dynamic-nargs      #x10000150 8 :space :window
                       :doc "Dynamic nargs (u32), written by a caller, read by &rest prologues.")
  (:intern-counter     #x10000158 8 :doc "Intern counter.")
  (:code-base          #x10000160 8 :constants ("MODUS.MVM.X64::+CODE-BASE-SLOT+")
                       :doc "code_base: FUNCTIONP's range arm.")
  (:code-end           #x10000168 8 :constants ("MODUS.MVM.X64::+CODE-END-SLOT+")
                       :doc "code_end.")
  (:package-table      #x10000170 8 :doc "Package-by-hash table.")
  (:jit-fnaddr-idx     #x10000178 8 :space :window :owner "mvm/mvm-eval.lisp"
                       :doc "Late-bound bridge's callee-index slot (%JIT-FNADDR-IDX-SLOT)."))

(define-fixed-word :handler-jmpbuf-area #x10000180 #x80 :kind :range
  :owner "translate-*.lisp (SETJMP/LONGJMP)"
  :doc "The armed handler-case frame / jmpbuf and its saved copies.")

(define-fixed-words (:within :handler-jmpbuf-area :space :window)
  (:armed-frame #x10000180 #x20
   :images (:x64 :a64 :i386-hosted :i386-bare :arm32-hosted :68k-hosted :68k-bare :web)
   :owner "translate-x64 / -aarch64 / -i386 / -arm32 / -68k"
   :doc "x86-64/AArch64 RSP/RBP/IP/RBX; i386 6-word, arm32/68k 7-word jmpbuf.")
  (:armed-frame-rv #x10000180 #x60 :images (:rv-hosted) :owner "mvm/translate-riscv.lisp"
   :doc "RISC-V hosted 12-word (96-byte) jmpbuf (*RV-JMPBUF-ADDR*).")
  (:armed-frame-ppc #x10000180 #x40 :images (:ppc-hosted) :owner "mvm/translate-ppc.lisp"
   :doc "PowerPC hosted 8-word jmpbuf (ppc-jmpbuf-addr).")
  (:saved-outer-frame #x100001A0 #x18 :images (:x64+a64) :space :global
   :owner "mvm/translate-aarch64.lisp, boot/boot-x64.lisp"
   :doc "fork-file's SAVE-OUTER copy of 180/188/190 (the IRQ-deadline fallback).")
  (:a64-saved-frame #x100001C0 #x18 :images (:a64) :owner "mvm/translate-aarch64.lisp"
   :doc "AArch64 handler-case slot copy (a64-load-tls-addr x17 #x100001C0).")
  (:a64-heap-exhausted-byte #x100001F0 8 :images (:a64-hosted) :space :global
   :owner "mvm/translate-aarch64.lisp"
   :doc "Byte buffer the native collector's HEAP EXHAUSTED message writes from."))

;;; ---- 0x200-0x3FF: argc/argv, longjmp scratch, per-port globals --------

(define-fixed-word :argc #x10000200 8 :images (:hosted)
  :owner "boot/boot-linux-*.lisp" :doc "argc (u32), stored by every hosted stub.")

(define-fixed-words (:images (:x64-hosted :a64-hosted :i386-hosted) :owner "boot/boot-linux-*.lisp")
  (:argv1-copy #x10000208 #x40 :doc "argv[1] (63 bytes + NUL); lib/save-image reads \"-core\" here.")
  (:argv2-copy #x10000248 #x40 :doc "argv[2] (63 bytes + NUL)."))

(define-fixed-word :a64-itimerval #x100002A0 #x20 :images (:a64-hosted)
  :owner "mvm/translate-aarch64.lisp" :doc "fork-file's setitimer itimerval staging.
   It was #x10000280, 8 bytes into argv[2]'s copy; moved clear of it.")

(define-fixed-word :i386-saved-esp #x10000290 4 :images (:i386-hosted)
  :owner "boot/boot-linux-i386.lisp" :doc "The kernel's initial ESP (argv/envp walk).")

(define-fixed-word :lj-scratch #x10000300 #x40 :space :window
  :images (:ppc-hosted :arm32-hosted :68k-hosted)
  :owner "translate-ppc / -arm32 / -68k (lj-scratch-addr)"
  :doc "LONGJMP's jmpbuf copy (hbase+#x300).")
(define-fixed-word :lj-scratch-rv #x10000300 #x60 :images (:rv-hosted)
  :owner "mvm/translate-riscv.lisp" :doc "RISC-V LONGJMP scratch (96 bytes).")

(define-fixed-word :rv-hosted-globals #x10000380 #x40 :images (:rv-hosted)
  :owner "mvm/translate-riscv.lisp (*RV-HOSTED-GLOBALS-BASE*)"
  :doc "RISC-V hosted convention slots (cenv at +8, ...).")

(define-fixed-words (:images (:i386-hosted :i386-bare) :owner "mvm/translate-i386.lisp")
  (:i386-lj-scratch  #x100003C0 #x18 :doc "*I386-LONGJMP-SCRATCH-ADDR*: 6 words.")
  (:i386-gc-rcb      #x100003D8 4 :doc "*I386-GC-RCB-ADDR*: active region control block.")
  (:i386-gc-size     #x100003DC 4 :doc "*I386-GC-SIZE-ADDR*.")
  (:i386-gc-sb       #x100003E0 4 :doc "*I386-GC-SB-ADDR*.")
  (:i386-gc-to       #x100003E4 4 :doc "*I386-GC-TO-ADDR*.")
  (:i386-gcroot-bound #x100003E8 4 :doc "*I386-GCROOT-BOUND-ADDR*."))

;;; ---- 0x400-0xC0F: the handler-frame stack ---------------------------

(define-fixed-word :hstack-depth #x10000400 8 :space :window
  :images (:x64 :i386-hosted :i386-bare :ppc-hosted :arm32-hosted :rv-hosted
           :68k-hosted :68k-bare :web)
  :owner "translate-* (hdepth-addr)"
  :doc "Handler-frame stack depth.  AArch64 keeps its own at #x10010000.")

(define-fixed-words (:space :window)
  (:hstack-frames-x64-bare #x10000408 #x800 :kind :range :images (:x64-bare :web)
   :owner "mvm/translate-x64.lisp" :doc "Bare x86-64: 64 frames x 32 bytes (to #x10000C08).")
  (:hstack-frames-i386-bare #x10000408 #x5E8 :kind :range :images (:i386-bare)
   :owner "mvm/translate-i386.lisp" :doc "Bare i386: 63 frames x 24 bytes.")
  (:hstack-frames-46 #x10000408 #x5C0 :kind :range :images (:ppc-hosted :68k-hosted :68k-bare)
   :owner "translate-ppc / -68k"
   :doc "46 32-byte (ppc32/68k) or 23 64-byte (ppc64) frames, to #x100009C8."))

(define-fixed-words (:images (:ppc-hosted) :owner "mvm/translate-ppc.lisp")
  (:ppc-hstack-ext       #x100009D0 8 :constants ("MODUS-INT::+PPC-HSTACK-EXT-ADDR+")
                         :doc "Handler-frame extension address.")
  (:ppc-hstack-ext-limit #x100009D8 8 :constants ("MODUS-INT::+PPC-HSTACK-EXT-LIMIT-ADDR+")
                         :doc "TEST-ONLY extension limit."))

(define-fixed-word :hosted-32-globals #x10000A00 #x200
  :images (:ppc-hosted :i386-hosted :i386-bare :68k-hosted)
  :owner "translate-ppc / -i386 / -68k (*-HOSTED-GLOBALS-BASE*), boot/boot-linux-i386.lisp"
  :doc "The 32-bit ports' convention slot block (nargs, cenv, MV for some ports).
   Size is an upper bound: nothing else is claimed in #xA00..#xC0F there."
  :constants ("MODUS-INT::+LINUX-I386-GLOBALS+"))

;;; ---- 0xC10-0xC6F: longjmp scratch, the self slot, fault diagnostics ---

(define-fixed-word :x64-lj-scratch #x10000C10 #x20 :space :window :images (:x64+a64 :web)
  :owner "mvm/translate-x64.lisp, mvm/translate-aarch64.lisp"
  :doc "LONGJMP scratch: the frame copied here before the pop (C10/C18/C20/C28).")

(define-fixed-word :tls-self #x10000C30 8 :space :window :images (:low-block)
  :owner "mvm/compiler.lisp (+TLS-SELF-ADDR+), mvm/prelude.lisp (%DYNB-BLOCK)"
  :constants ("MODUS-INT::+TLS-SELF-ADDR+")
  :doc "This thread's window SEGMENT BASE (delta), 0 on main.  On bare x86-64 /
   AArch64 / i386 it is the fault-diagnostic RIP/ELR slot.")

(define-fixed-word :fault-diag #x10000C30 #x30 :kind :range :images (:x64 :a64-bare :i386-bare)
  :owner "mvm/translate-x64.lisp (fault stub), boot/boot-aarch64.lisp"
  :doc "Fault diagnostics: C30 RIP/ELR, C38 RSP, C40 [RSP]/FAR, C48 RAX/X0, C50, C58."
  :shares (:tls-self :dynb-next :dynb-depth)
  :reason "C30 is also the self slot.  The x64 fault stub skips the RIP store while
   the threads gate (0x10000DB8) is open and in every image that can run
   threads (X64-SSG-P) -- translate-x64 THE FAULT RECORD.  Bare images have no
   threads.  The C50/C58 diag words are main/bare BSS; a worker's C50/C58 are
   its block-private dynamic-binding words.")

(define-fixed-words (:images (:low-block))
  (:clos-debug-budget #x10000C60 8 :owner "mvm/cl-clos.lisp"
   :doc "cl-clos's dispatch debug budget.")
  (:gethash-latch     #x10000C68 8 :owner "mvm/cl-conditions.lisp"
   :doc "Type-error recursion latch (raw so it needs no hash lookup).")
  (:deadline-tick     #x10000C70 8 :owner "boot/boot-x64.lisp, AArch64 IRQ deadline"
   :doc "Per-test deadline counter (zeroed by the bare stubs)."))

(define-fixed-word :x64-bare-rescue-rip #x10000C60 8 :images (:x64-bare)
  :shares (:clos-debug-budget)
  :reason "NEAR-MISS: a rescued fault on bare x86-64 leaves the RIP in cl-clos's
   EQ-COLL print budget, re-arming a diagnostic that was meant to stay capped.
   Diagnostic output only; nothing reads C60 as data otherwise."
  :owner "boot/boot-x64.lisp (IDT fault stub)"
  :doc "The no-handler rescue path copies the faulting RIP here: [C60] = [C30].")

(define-fixed-word :x64-dynb-entry0-value #x10000C78 8 :images (:x64-hosted) :space :block
  :within :dynb-fallback :owner "mvm/translate-x64.lisp"
  :doc "&entry0.value of the in-block binding stack (block-relative).")

(define-fixed-words (:images (:low-block))
  (:sym-alloc-counter #x10000C80 8 :owner "mvm/prelude.lisp"
   :doc "%ALLOC-SYM3 retry counter (bumped around the symbol-table root re-read)."))

;;; ---- 0xC88-0xDFF: generational GC, fault records, runtime locks -------

(define-fixed-words (:images (:x64+a64) :owner "mvm/translate-x64.lisp, mvm/translate-aarch64.lisp, mvm/gc.lisp")
  (:gen-tenure-req #x10000C88 #x18 :constants ("MODUS.MVM.X64::+X64-GEN-TENURE-REQ+"
                                                "MODUS-INT::+GC-TENURE-REQ-ADDR+")
   :doc "Tenure request: young from/to/size, <<1.")
  (:fault-count      #x10000CA0 8 :images (:low-block) :owner "mvm/cl-conditions.lisp"
   :doc "Recovered-fault COUNT (u32), lock-incremented by the fault stubs.")
  (:fault-count-seen #x10000CA8 8 :images (:low-block) :owner "mvm/cl-conditions.lisp"
   :doc "Last-seen fault count (u32).")
  (:pending-fault    #x10000CB0 8 :images (:low-block) :owner "mvm/cl-conditions.lisp (%TAKE-PENDING-FAULT)"
   :constants ("MODUS-INT::+68K-PENDING-FAULT-ADDR+")
   :doc "Pending fault code (u32: signal, #x0B0B, #x0F0F handler-stack overflow).
   Process-global; bare RISC-V/ARM32 use their relocated block's +#xCB0/+#x360.")
  (:x64-tsc-hz       #x10000CB8 8 :images (:x64-hosted) :owner "mvm/translate-x64.lisp"
   :doc "Measured TSC rate, cached TAGGED.")
  (:gen-old-lo       #x10000CC0 8 :constants ("MODUS.MVM.X64::+X64-GEN-OLD-LO-ADDR+"
                                              "MODUS-INT::+GC-OLD-LO-ADDR+")
   :doc "Old generation low bound, <<1.")
  (:gen-old-hi       #x10000CC8 8 :constants ("MODUS.MVM.X64::+X64-GEN-OLD-HI-ADDR+"
                                              "MODUS-INT::+GC-OLD-HI-ADDR+")
   :doc "Old generation high bound, <<1.")
  (:a64-fault-budget #x10000CD0 8 :images (:a64) :owner "boot/boot-aarch64.lisp"
   :doc "Fault-storm budget (sync recovery entry 4).")
  (:a64-rescue-budget #x10000CD8 8 :images (:a64) :owner "boot/boot-aarch64.lisp"
   :doc "Rescue budget.")
  (:gen-cards        #x10000CE0 8 :constants ("MODUS.MVM.X64::+X64-GEN-CARDS-ADDR+"
                                              "MODUS-INT::+GC-CARD-BASE-ADDR+")
   :doc "Card table base.")
  (:gen-area         #x10000CE8 8 :images (:x64) :constants ("MODUS.MVM.X64::+X64-GEN-AREA-ADDR+"))
  (:gen-ncards       #x10000CF0 8 :images (:x64) :constants ("MODUS.MVM.X64::+X64-GEN-NCARDS-ADDR+"))
  (:gen-crossing     #x10000CF8 8 :images (:x64) :constants ("MODUS.MVM.X64::+X64-GEN-CROSSING-ADDR+")))

(define-fixed-words (:images (:x64) :owner "mvm/translate-x64.lisp (handler helpers)")
  (:hc-capcount     #x10000D00 8 :doc "Handler-stack overflow event count.")
  (:hc-max-depth    #x10000D08 8 :doc "DIAG: max-depth watermark.")
  (:hc-pop-at-empty #x10000D10 8 :doc "DIAG: pop-at-empty counter.")
  (:hc-overflow     #x10000D20 8 :doc "Live-overflow absorb flag.")
  (:hc-transition   #x10000D28 8 :doc "In-transition flag (fault during a push/pop).")
  (:deadline-pending #x10000D30 8 :doc "Deadline pending flag (IRQ deadline -> longjmp).")
  (:bytes-consed    #x10000D38 8 :images (:x64-hosted)
   :constants ("MODUS.MVM.X64::+X64-BYTES-CONSED-ADDR+")
   :doc "SB-EXT:GET-BYTES-CONSED accumulator, <<1."))

(define-fixed-words (:images (:a64-bare :web) :owner "mvm/build-web.lisp, mvm/build-cl-repl-common.lisp")
  (:jit-quote-vector  #x10000D40 8 :doc "JIT quote-constant vector root.")
  (:jit-callee-table  #x10000D48 8 :doc "JIT callee table.")
  (:jit-sync-word     #x10000D50 8 :doc "JIT sync word.")
  (:jit-spare-d58     #x10000D58 8 :doc "Zeroed by the virt prologue; no reader found."))

(define-fixed-word :fork-rescue-frame #x10000D68 #x20 :images (:x64-bare)
  :owner "boot/boot-x64.lisp (IDT rescue)"
  :doc "fork-file's per-file rescue frame D68 armed, D70 rbp, D78 rdx, D80 rbx.")

(define-fixed-word :gen-card-root #x10000D88 8 :images (:x64+a64)
  :owner "mvm/translate-x64.lisp, mvm/translate-aarch64.lisp"
  :constants ("MODUS.MVM.X64::+X64-GEN-CARD-ROOT+" "MODUS-INT::+GC-CARD-ROOT-ADDR+")
  :doc "The card vector, TAGGED (a GC root).")

(define-fixed-word :fork-rescue-budget #x10000D90 8 :images (:x64-bare)
  :owner "boot/boot-x64.lisp" :doc "Rescue budget for the fork-file rescue frame.")

(define-fixed-words (:images (:threads) :owner "net/hosted-sync.lisp, mvm/translate-x64.lisp")
  (:lock-arena-words #x10000D98 8 :constants ("MODUS.MVM.X64::+X64-LOCK-ARENA-WORDS-SLOT+")
   :doc "Runtime lock arena's word count.")
  (:safepoint-boundary #x10000DA0 8 :images (:x64 :a64 :i386-bare)
   :doc "Safepoint boundary (x64 deadline handler compares the caller against it).")
  (:thr-page-slot   #x10000DA8 8 :constants ("MODUS.MVM.X64::+STW-THR-PAGE-SLOT+")
   :doc "The thread page's address.")
  (:thr-page-lock   #x10000DB0 8 :doc "Thread page lock.")
  (:threads-gate    #x10000DB8 8 :images (:low-block)
   :doc "THE THREADS GATE (u32): non-zero once a program declared Lisp on >1 thread.
   Read by %RT-ENTER, %DYNBIND, the eval lock, the fault stubs.")
  (:rt-mutex        #x10000DC0 8 :doc "Runtime-table mutex.")
  (:rt-owner        #x10000DC8 8 :doc "Runtime-table owner.")
  (:rt-depth        #x10000DD0 8 :doc "Runtime-table recursion depth.")
  (:rt-saved        #x10000DD8 8 :doc "Runtime-table saved allocation state.")
  (:rt-contended    #x10000DE0 8 :doc "Runtime lock contention counter.")
  (:rt-acquired     #x10000DE8 8 :doc "Runtime lock acquisition counter.")
  (:sk-page-slot    #x10000DF0 8 :owner "net/hosted-sockets-post.lisp" :doc "Socket page.")
  (:sk-page-lock    #x10000DF8 8 :owner "net/hosted-sockets-post.lisp" :doc "Socket page lock."))

;;; ---- 0xE00-0xEFF: MCGC config, eval lock, GC scratch config -----------

(define-fixed-word :mcgc-config-block #x10000E00 #xB0 :kind :range :images (:x64)
  :owner "mvm/translate-x64.lisp, boot/boot-linux-x64.lisp, mvm/mcgc-pin.lisp"
  :doc "The mostly-copying collector's config words, E00..EAF.  Its own comment
   says 'reserves through 0x10000EC0'; that tail (EB0..EC7) is now the eval lock.")

(define-fixed-words (:within :mcgc-config-block :images (:x64) :owner "mvm/translate-x64.lisp")
  (:mcgc-page-base    #x10000E00 8 :constants ("MODUS-INT::+MCGC-CFG-PAGE-BASE+"
                                               "MODUS-INT::+MCGC-PIN-CFG-PAGE-BASE+"
                                               "MODUS.MVM.X64::+MCGC-CFG-PAGE-BASE-ADDR+"))
  (:mcgc-page-count   #x10000E08 8 :constants ("MODUS-INT::+MCGC-CFG-PAGE-COUNT+"
                                               "MODUS-INT::+MCGC-PIN-CFG-PAGE-COUNT+"
                                               "MODUS.MVM.X64::+MCGC-CFG-PAGE-COUNT-ADDR+"))
  (:mcgc-descriptor   #x10000E10 8 :constants ("MODUS-INT::+MCGC-CFG-DESCRIPTOR+"
                                               "MODUS.MVM.X64::+MCGC-CFG-DESCRIPTOR-ADDR+"))
  (:mcgc-bitmap       #x10000E18 8 :constants ("MODUS-INT::+MCGC-CFG-BITMAP+"
                                               "MODUS.MVM.X64::+MCGC-CFG-BITMAP-ADDR+"))
  (:mcgc-freelist     #x10000E20 8 :constants ("MODUS-INT::+MCGC-CFG-FREELIST+"
                                               "MODUS-INT::+MCGC-PIN-CFG-FREELIST+"
                                               "MODUS.MVM.X64::+MCGC-CFG-FREELIST-BASE-ADDR+"))
  (:mcgc-freelist-count #x10000E28 8 :constants ("MODUS-INT::+MCGC-CFG-FREELIST-COUNT+"
                                                 "MODUS.MVM.X64::+MCGC-CFG-FREELIST-COUNT-ADDR+"))
  (:mcgc-alloc-page   #x10000E30 8 :constants ("MODUS-INT::+MCGC-CFG-ALLOC-PAGE+"
                                               "MODUS.MVM.X64::+MCGC-CFG-ALLOC-PAGE-ADDR+"))
  (:mcgc-data-end     #x10000E38 8 :constants ("MODUS-INT::+MCGC-CFG-DATA-END+"
                                               "MODUS.MVM.X64::+MCGC-CFG-DATA-END-ADDR+"))
  (:mcgc-run-start    #x10000E40 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-RUN-START-ADDR+"))
  (:mcgc-run-end      #x10000E48 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-RUN-END-ADDR+"))
  (:mcgc-to-start     #x10000E50 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-TO-START-ADDR+"))
  (:mcgc-to-end       #x10000E58 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-TO-END-ADDR+"))
  (:mcgc-init-done    #x10000E60 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-INIT-DONE-ADDR+"))
  (:mcgc-from-start   #x10000E68 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-FROM-START-ADDR+"))
  (:mcgc-from-end     #x10000E70 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-FROM-END-ADDR+"))
  (:mcgc-pincount     #x10000E78 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-PINCOUNT-ADDR+"))
  (:mcgc-scan-cursor  #x10000E80 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-SCAN-CURSOR-ADDR+"))
  (:mcgc-pin-init     #x10000E88 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-PIN-INIT-ADDR+"))
  (:mcgc-seg-arr      #x10000E90 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-SEG-ARR-ADDR+"))
  (:mcgc-seg-count    #x10000E98 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-SEG-COUNT-ADDR+"))
  (:mcgc-oom          #x10000EA0 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-OOM-ADDR+"))
  (:mcgc-uncap        #x10000EA8 8 :constants ("MODUS.MVM.X64::+MCGC-CFG-UNCAP-ADDR+")))

(define-fixed-words (:images (:low-block) :owner "mvm/mvm-eval.lisp (%EVAL-LOCK-*)")
  (:eval-lock       #x10000EB0 8 :doc "In-image compiler's recursive lock word.")
  (:eval-lock-owner #x10000EB8 8 :doc "Its owner (self slot + 1, u32).")
  (:eval-lock-depth #x10000EC0 8 :doc "Its depth (u32)."))

(define-fixed-word :gc-scratch-cfg #x10000EC8 8
  :owner "mvm/compiler.lisp, mvm/gc.lisp"
  :constants ("MODUS-INT::+GC-SCRATCH-CFG-ADDR+")
  :doc "Base of the per-CPU GC scratch-block array; 0 = the historic block at 0x10000100.")

(define-fixed-words (:images (:low-block) :owner "mvm/gc.lisp")
  (:gc-region-align-violations #x10000ED0 8 :doc "%GC-REGION-ALIGN-VIOLATIONS counter.")
  (:gc-region-align-last       #x10000ED8 8 :doc "Last misaligned value."))

(define-fixed-words (:images (:x64) :owner "mvm/translate-x64.lisp (concurrent-collection barrier)")
  (:gc-conc-cur     #x10000EE0 8 :constants ("MODUS.MVM.X64::+GC-CONC-CUR-ADDR+"))
  (:gc-conc-witness #x10000EE8 8 :constants ("MODUS.MVM.X64::+GC-CONC-WITNESS-ADDR+"))
  (:gc-conc-barrier #x10000EF0 8 :constants ("MODUS.MVM.X64::+GC-CONC-BARRIER-ADDR+"))
  (:gc-conc-met     #x10000EF8 8 :constants ("MODUS.MVM.X64::+GC-CONC-MET-ADDR+")))

;;; ---- 0xF00-0xFFF: roots, the per-region table, locks ------------------

(define-fixed-word :x64-jit-constvec-root #x10000F00 8 :images (:x64)
  :owner "mvm/translate-x64.lisp (*X64-JIT-CONSTVEC-ROOT*)"
  :constants ("MODUS.MVM.X64::*X64-JIT-CONSTVEC-ROOT*")
  :doc "Tagged JIT constant vector (a collector root).")

(define-fixed-word :dtb-ptr-slot #x10000F00 8 :images (:a64-bare)
  :owner "vessel boot-rpi-cl.lisp (+RPI-CL-DTB-PTR-SLOT+), lib/fdt.lisp"
  :doc "The firmware device-tree pointer.")

(define-fixed-word :gc-region-table #x10000F08 #x80 :kind :range
  :owner "mvm/compiler.lisp (+GC-REGION-ADDR+ + 8*cpu), mvm/gc.lisp"
  :constants ("MODUS-INT::+GC-REGION-ADDR+")
  :doc "Per-CPU active-region table: 16 cells, F08..F87.  CPU 0's cell is the
   historic single word; the rest are live only with the per-CPU gate on.")

(define-fixed-words (:images (:a64-bare) :owner "mvm/gc.lisp, mvm/translate-aarch64.lisp")
  (:a64-gc-stats #x10000F20 #x38 :kind :range
   :shares (:gc-region-table)
   :reason "CPU 3..9's cells.  A non-threaded AArch64 image has one CPU running
   Lisp, so only CPU 0's cell is live; the threaded layout moves these words to
   0x1000FF00 (a64-gc-stat-addr)."
   :doc "GC pause statistics: start/total/max/last ticks, total/last bytes, base count.")
  (:a64-jit-bump #x10000F58 8
   :shares (:gc-region-table)
   :reason "As :A64-GC-STATS (moves to 0x1000FF38 with threads)."
   :doc "JIT arena bump word."))

(define-fixed-word :core-cursor-slot #x10000F60 8 :images (:a64-bare)
  :shares (:gc-region-table)
  :reason "CPU 11's cell; bare AArch64 images run Lisp on one CPU."
  :owner "mvm/build-cl-repl-common.lisp (%CORE-CURSOR-SLOT)"
  :doc "Core-from-card read cursor.")

(define-fixed-words (:images (:a64) :owner "boot/boot-linux-aarch64.lisp, mvm/translate-aarch64.lisp")
  (:a64-handler-push-va #x10000F90 8 :constants ("MODUS-INT::+A64-HANDLER-PUSH-VA-SLOT+")
   :doc "VA of the handler-stack PUSH helper (JIT pages BLR through it).")
  (:a64-handler-pop-va  #x10000F98 8 :constants ("MODUS-INT::+A64-HANDLER-POP-VA-SLOT+")
   :doc "VA of the POP helper."))

(define-fixed-words (:images (:all) :owner "mvm/compiler.lisp")
  ;; :ALL, not :LOW-BLOCK: the compiler emits these two for EVERY target (the
  ;; inline global-cell read; COMPILE-QUOTE's static literals), including the
  ;; bare RISC-V / ARM32 images whose #x10000000 is device space.  See the
  ;; :SHARES on :RV-BARE-UART and :ARM32-BARE-DEVICE-SPACE.
  (:gv-cache-root  #x10000FA0 8 :constants ("MODUS-INT::+GV-CACHE-ROOT+")
   :doc "Global-cell cache vector.")
  (:gv-cache-guard #x10000FA8 8 :doc "Its init guard.")
  (:static-literal-root #x10000FB0 8 :owner "mvm/compiler.lisp (static literals), lib/save-image.lisp"
   :doc "Static-literal / quoted-symbol vector root (a root in every collector)."))

(define-fixed-word :x64-jit-bump #x10000FB8 8 :images (:x64-hosted)
  :owner "boot/boot-linux-x64.lisp" :constants ("MODUS-INT::+LINUX-X64-JIT-BUMP-SLOT+")
  :doc "JIT page bump word.")

(define-fixed-word :sched-lock #x10000FC0 8 :images (:x64+a64)
  :owner "mvm/compiler.lisp, net/hosted-actors.lisp, translate-x64 +OP-RESTORE-CTX+"
  :constants ("MODUS-INT::+HOSTED-SCHED-LOCK-ADDR+")
  :doc "The hosted actor scheduler's spinlock (ONE word; bare boards' SMP-INIT
   zeroes lock+0/+8/+16, hence the hosted override zeroes only this one).")

(define-fixed-word :stw-flag #x10000FC8 8 :images (:x64+a64)
  :owner "mvm/translate-x64.lisp, mvm/translate-aarch64.lisp"
  :constants ("MODUS.MVM.X64::+X64-STW-FLAG-ADDR+" "MODUS-INT::+A64-STW-FLAG-ADDR+")
  :doc "Region-0 stop-the-world flag.")

(define-fixed-word :a64-jit-constvec-root #x10000FD0 8 :images (:a64)
  :owner "mvm/translate-aarch64.lisp (*AARCH64-JIT-CONSTVEC-ROOT*)"
  :constants ("MODUS-INT::*AARCH64-JIT-CONSTVEC-ROOT*")
  :doc "Tagged JIT constant vector (a collector root).")

(define-fixed-word :random-seed #x10000FD8 8 :images (:low-block)
  :owner "mvm/cl-sequences.lisp" :doc "RANDOM's fallback seed (u32).")

(define-fixed-words (:images (:low-block) :owner "net/cooperative-atomics.lisp")
  (:atomics-lock     #x10000FE0 8 :doc "Cooperative atomics lock.")
  (:atomics-acquired #x10000FE8 8 :doc "Its acquisition counter (the two-way oracle)."))

(define-fixed-word :a64-lr-save #x10000FF0 8 :images (:a64)
  :owner "mvm/translate-aarch64.lisp" :doc "Unthreaded trap-time x30 save slot.")

(define-fixed-word :gc-region-percpu-gate #x10000FF8 8
  :owner "mvm/compiler.lisp, mvm/gc.lisp" :constants ("MODUS-INT::+GC-REGION-PERCPU-ADDR+")
  :doc "Per-CPU active-region mode word (0 = single word at F08).")

;;; ---- WORKER-BLOCK-PRIVATE offsets (the main thread never addresses them) --

(define-fixed-words (:images (:threads) :space :block :owner "mvm/prelude.lisp (%DYNB-*), net/hosted-sync.lisp")
  (:dynb-next  #x10000C50 8 :doc "blk+0xC50: address of the next free binding entry.")
  (:dynb-depth #x10000C58 8 :doc "blk+0xC58: binding depth, TAGGED.")
  (:dynb-ext   #x10000C60 8 :doc "blk+0xC60: base of this thread's 16 KB extension stack, or 0.")
  (:dynb-fallback #x10000C70 #x390 :kind :range
   :doc "blk+0xC70..0xFFF: 57 in-block [key][value] entries (when blk+0xC60 = 0)."))

(define-fixed-word :dynb-filter #x10000C80 #x200 :kind :range :images (:threads) :space :block
  :within :dynb-fallback :owner "mvm/prelude.lisp (%DYNB-FILTER)"
  :doc "256 16-bit counters, used only when the extension stack is (blk+0xC60 /= 0),
   i.e. exactly when the fallback entries it overlays are unused.")

;;; ---- x86-64 hosted: the window past its first page (block = 0x8000) ---

(define-fixed-word :x64-hstack-frames #x10001000 #x4000 :kind :range :space :window
  :images (:x64-hosted) :owner "mvm/translate-x64.lisp (EMIT-HANDLER-HELPERS-LINUX)"
  :doc "512 handler frames x 32 bytes, window +0x1000..+0x5000.")

(define-fixed-words (:images (:x64-hosted) :owner "mvm/translate-x64.lisp (STOP-THE-WORLD)")
  (:stw-state      #x10005000 8 :space :window :constants ("MODUS.MVM.X64::+STW-STATE-ADDR+")
   :doc "Per thread: 0 running, 1 in a syscall, 2 parked.")
  (:stw-rsp        #x10005008 8 :space :window :constants ("MODUS.MVM.X64::+STW-RSP-ADDR+")
   :doc "Per thread: the RSP a parked thread published.")
  (:stw-r0-count   #x10005010 8 :space :window :images (:threads)
   :constants ("MODUS.MVM.X64::+STW-R0-COUNT-ADDR+" "MODUS-INT::+A64-STW-R0-COUNT-ADDR+")
   :doc "Per thread: region 0's collection count when this thread last stopped.")
  (:stw-stop       #x10005018 8 :constants ("MODUS.MVM.X64::+STW-STOP-ADDR+")
   :doc "ABSOLUTE: 1 while a region-0 collection has the world stopped.")
  (:stw-clone-tls  #x10005020 8 :constants ("MODUS.MVM.X64::+STW-CLONE-TLS-ADDR+")
   :doc "ABSOLUTE: the FS base the next cloned thread is born with.")
  (:stw-owner      #x10005028 8 :constants ("MODUS.MVM.X64::+STW-OWNER-ADDR+")
   :doc "ABSOLUTE: self base + 1 of the thread holding STOP."))

(define-fixed-words (:images (:threads) :space :window :owner "mvm/translate-x64.lisp (SHARED-STORE GUARD), mvm/prelude.lisp")
  (:ssg-region-a #x10005040 8 :constants ("MODUS.MVM.X64::+SSG-A-ADDR+" "MODUS-INT::+A64-SSG-WINDOW-ADDR+")
   :doc "This thread's semispace A.")
  (:ssg-region-b #x10005048 8 :constants ("MODUS.MVM.X64::+SSG-B-ADDR+") :doc "Semispace B.")
  (:ssg-size     #x10005050 8 :constants ("MODUS.MVM.X64::+SSG-SIZE-ADDR+") :doc "Region size.")
  (:ssg-marker   #x10005058 8 :images (:x64-hosted) :constants ("MODUS.MVM.X64::+SSG-MARKER-ADDR+")
   :doc "Guard marker (operandi's shared-store guard word).")
  (:ssg-site     #x10005060 8 :images (:x64-hosted) :constants ("MODUS.MVM.X64::+SSG-SITE-ADDR+")
   :doc "Guard trap site."))

(define-fixed-words (:images (:x64-hosted) :space :window :owner "net/hosted-backtrace.lisp, net/hosted-sync.lisp, net/hosted-actor-runtime.lisp")
  (:bt-scratch       #x100050A0 8 :doc "Backtrace scratch word (%GC-WORD-OF).")
  (:thr-stack-size   #x100050A8 8 :doc "This thread's stack size (scan bound).")
  (:ar-report-word   #x100050B0 8 :doc "Actor-runtime report scratch.")
  (:actor-stack-top  #x100050C0 8 :owner "mvm/translate-x64.lisp" :doc "Actor context stack top.")
  (:actor-stack-bottom #x100050C8 8 :owner "mvm/translate-x64.lisp" :doc "Actor context stack bottom.")
  (:stw-actor-table  #x100050D0 8 :space :global :owner "mvm/translate-x64.lisp"
   :constants ("MODUS.MVM.X64::+STW-ACTOR-TABLE-ADDR+")
   :doc "ABSOLUTE: net/hosted-actor-runtime's parked-actor scan table, or 0.")
  (:fault-bt-busy    #x100051F8 8 :owner "mvm/translate-x64.lisp" :doc "Fault-backtrace walk busy word.")
  (:fault-bt-count   #x10005200 8 :owner "mvm/translate-x64.lisp" :doc "Fault backtrace frame count.")
  (:fault-bt-frames  #x10005208 #x1F8 :kind :range :owner "mvm/translate-x64.lisp"
   :doc "Up to 63 return addresses of this thread's last fault."))

(define-fixed-words (:images (:threads) :space :window :owner "translate-x64 / -aarch64 (THE FAULT RECORD IS PER THREAD)")
  (:hstack-ext       #x10005600 8 :constants ("MODUS.MVM.X64::+HSTACK-EXT-ADDR+" "MODUS-INT::+A64-HSTACK-EXT-ADDR+")
   :doc "Address of this thread's handler-frame extension (0 until reserved).")
  (:hstack-ext-limit #x10005608 8 :constants ("MODUS.MVM.X64::+HSTACK-EXT-LIMIT-ADDR+" "MODUS-INT::+A64-HSTACK-EXT-LIMIT-ADDR+")
   :doc "TEST-ONLY extension frame limit.")
  (:fault-rec-code   #x10005610 8 :constants ("MODUS.MVM.X64::+FAULT-REC-CODE-ADDR+" "MODUS-INT::+A64-FAULT-REC-CODE-ADDR+")
   :doc "Fault record: code (u32).")
  (:fault-rec-addr   #x10005618 8 :constants ("MODUS.MVM.X64::+FAULT-REC-ADDR-ADDR+" "MODUS-INT::+A64-FAULT-REC-ADDR-ADDR+")
   :doc "Fault record: si_addr.")
  (:fault-rec-pc     #x10005620 8 :constants ("MODUS.MVM.X64::+FAULT-REC-PC-ADDR+" "MODUS-INT::+A64-FAULT-REC-PC-ADDR+")
   :doc "Fault record: interrupted PC.")
  (:fault-rec-in-gc  #x10005628 8 :constants ("MODUS.MVM.X64::+FAULT-REC-GC-ADDR+" "MODUS-INT::+A64-FAULT-REC-GC-ADDR+")
   :doc "Non-zero while this thread runs the collector.")
  (:fault-rec-gc-test #x10005630 8 :constants ("MODUS.MVM.X64::+FAULT-REC-GC-TEST-ADDR+" "MODUS-INT::+A64-FAULT-REC-GC-TEST-ADDR+")
   :doc "TEST ONLY knob (test/fault-in-gc.lisp)."))

;;; ---- The worker window blocks themselves (H3 checks every window word fits) --

(define-fixed-word :x64-window-block #x10000000 #x8000 :kind :container :space :window
  :images (:x64-hosted) :owner "net/hosted-sync.lisp (%THR-TLS-BLOCK)"
  :doc "One x86-64 worker's window block: 0x8000 bytes.")
(define-fixed-word :a64-window-block #x10000000 #x11000 :kind :container :space :window
  :images (:a64-hosted) :owner "net/hosted-sync.lisp (%THR-TLS-BLOCK)"
  :doc "One AArch64 worker's window block: 0x11000 bytes.")

;;; ---- The THREAD TABLE (thread page + 0x13000): offsets, not addresses ---

(define-fixed-words (:base :thr-table :images (:threads)
                     :owner "net/hosted-sync.lisp, translate-x64 / -aarch64 (THE WAITS SLEEP)")
  (:thr-pending-slot #x00 8 :doc "The slot the child about to start should take.")
  (:thr-ack          #x08 8 :doc "That child's acknowledgement.")
  (:thr-spawn-lock   #x10 8 :doc "The spawn lock.")
  (:thr-scratch-word #x18 8 :doc "%GC-WORD-OF scratch.")
  (:thr-started      #x20 8 :doc "Threads started, ever.")
  (:thr-arrivals     #x28 8 :doc "Shared arrival counter (tests).")
  (:thr-barrier-lock #x30 8 :doc "Its lock.")
  (:thr-arena-lo     #x80 8 :doc "Lock arena low bound (stop-the-world evacuation).")
  (:thr-arena-hi     #x88 8 :doc "Lock arena high bound.")
  (:stw-csleep       #xC0 8 :constants ("MODUS.MVM.X64::+STW-CSLEEP-OFF+" "MODUS-INT::+A64-STW-CSLEEP-OFF+")
   :doc "CSLEEP: the collector's futex word.")
  (:stw-rsleep       #xC8 8 :constants ("MODUS.MVM.X64::+STW-RSLEEP-OFF+" "MODUS-INT::+A64-STW-RSLEEP-OFF+")
   :doc "RSLEEP: waiters in the stop-the-world loop.")
  (:stw-csleeps      #xD0 8 :constants ("MODUS.MVM.X64::+STW-CSLEEPS-OFF+" "MODUS-INT::+A64-STW-CSLEEPS-OFF+")
   :doc "Collector sleeps.")
  (:stw-ctimeouts    #xD8 8 :constants ("MODUS.MVM.X64::+STW-CTIMEOUTS-OFF+" "MODUS-INT::+A64-STW-CTIMEOUTS-OFF+")
   :doc "Collector timeouts with the thread already stopped (lost wakes).")
  (:stw-wsleeps      #xE0 8 :constants ("MODUS.MVM.X64::+STW-WSLEEPS-OFF+" "MODUS-INT::+A64-STW-WSLEEPS-OFF+")
   :doc "Waiter sleeps.")
  (:stw-wtimeouts    #xE8 8 :constants ("MODUS.MVM.X64::+STW-WTIMEOUTS-OFF+" "MODUS-INT::+A64-STW-WTIMEOUTS-OFF+")
   :doc "Waiter timeouts with the flag already clear (lost wakes).")
  (:thr-records      #x100 #x800 :kind :range
   :doc "16 per-thread records of 128 bytes (%THR-REC): state, clone tid, stack, ...")
  (:thr-reports      #xA00 #x400 :kind :range
   :doc "16 per-thread report blocks of 64 bytes (tests), to the end of the 4 KB table."))

;;; ---- AArch64: the window past 0xFF00 (hosted block = 0x11000) ---------

(define-fixed-word :a64-gc-stats-threaded #x1000FF00 #x40 :kind :range :images (:a64-hosted)
  :owner "mvm/gc.lisp, mvm/translate-aarch64.lisp (a64-gc-stat-addr)"
  :doc "The threaded layout's GC pause statistics + JIT bump (FF00..FF38).")

(define-fixed-word :a64-window-high-page #x1000F000 #x1000 :kind :container :space :window
  :images (:a64) :owner "boot/boot-aarch64.lisp (18b), vessel boot-rpi-cl.lisp"
  :doc "The window's high page, zeroed at boot on threaded bare images (step 18b).")

(define-fixed-words (:images (:a64) :space :window :owner "mvm/translate-aarch64.lisp")
  (:a64-percpu-ptr #x1000FFE8 8 :constants ("MODUS-INT::+A64-PERCPU-PTR-ADDR+")
   :doc "Per-CPU block pointer (x86-64 has GS for it).")
  (:a64-tls-lr-save #x1000FFF0 8 :doc "Helper-call LR save."))

(define-fixed-word :a64-hstack-depth #x10010000 8 :space :window :images (:a64)
  :owner "mvm/translate-aarch64.lisp" :doc "AArch64 handler-frame stack depth.")
(define-fixed-word :a64-hstack-frames-hosted #x10010008 #xFF0 :kind :range :space :window
  :images (:a64-hosted) :owner "mvm/translate-aarch64.lisp"
  :doc "170 frames x 24 bytes (+A64-HSTACK-WIN-FRAMES+), to +0x10FF8.")
(define-fixed-word :a64-hstack-frames-bare #x10010008 #xFFF0 :kind :range :space :window
  :images (:a64-bare) :owner "mvm/translate-aarch64.lisp"
  :doc "2730 frames x 24 bytes (+A64-HSTACK-BARE-FRAMES+), to +0x1FFF8.")

;;; ---- The hosted 32-bit / RISC-V low pages (#x1000-#x1FFFF) -------------

(define-fixed-words (:images (:arm32-hosted :rv-hosted :68k-hosted) :kind :region
                     :owner "mvm/build-{arm32,riscv32,riscv64,68k}-cli.lisp")
  (:cstr-scratch-page #x10001000 #x1000 :doc "*CSTR-SCRATCH*: C-string staging (rename uses +2048).")
  (:io-buf-page       #x10002000 #x1000 :doc "*IO-BUF-ADDR*: the 4 KB I/O page."))

(define-fixed-word :dirent-buf-page #x10003000 #x1000 :kind :region :images (:arm32-hosted :68k-hosted)
  :owner "mvm/build-arm32-cli.lisp, mvm/build-68k-cli.lisp"
  :doc "*%DIRENT-BUF-ADDR*: getdents64 scratch (cl-fileio's default #x1DD00000 is heap here).")

(define-fixed-word :68k-core-header #x10003000 #x40 :images (:68k-hosted)
  :owner "mvm/build-68k-cli.lisp (%68K-SAVE-CORE)"
  :shares (:dirent-buf-page)
  :reason "Written only while saving a core, and read only from the file; no
   directory listing runs in between."
  :doc "The saved core's header: magic, Y, three regions as address+length.")

(define-fixed-words (:images (:i386-hosted) :kind :region :owner "mvm/build-i386-cli.lisp, mvm/translate-i386.lisp")
  (:i386-cstr-scratch #x10004000 #x4000 :doc "*CSTR-SCRATCH* (i386 BSS window, below 2^30).")
  (:i386-io-buf       #x10008000 #x1000 :doc "*IO-BUF-ADDR*: one 4 KiB page, ends at the argv arena."))

(define-fixed-words (:images (:i386-hosted :arm32-hosted :rv32-hosted :68k-hosted) :kind :region
                     :owner "boot/boot-linux-{i386,arm32,riscv32,68k}.lisp")
  (:argv-ptrs  #x10009000 #x1000
   :constants ("MODUS-INT::+LINUX-I386-ARGV-PTRS+" "MODUS-INT::+LINUX-ARM32-ARGV-PTRS+"
               "MODUS-INT::+LINUX-RISCV32-ARGV-PTRS+" "MODUS-INT::+LINUX-68K-ARGV-PTRS+")
   :doc "The staged argv/envp pointer vector.")
  (:argv-arena #x1000A000 #x14000
   :constants ("MODUS-INT::+LINUX-I386-ARGV-ARENA+" "MODUS-INT::+LINUX-ARM32-ARGV-ARENA+"
               "MODUS-INT::+LINUX-RISCV32-ARGV-ARENA+" "MODUS-INT::+LINUX-68K-ARGV-ARENA+")
   :doc "The staged argv/envp strings, to #x1001E000."))

(define-fixed-words (:images (:68k-hosted :68k-bare :68k-mac) :owner "mvm/translate-68k.lisp")
  (:68k-hstack-ext       #x10005600 8 :images (:68k-hosted)
   :constants ("MODUS-INT::+68K-HSTACK-EXT-ADDR+") :doc "Handler-frame extension address.")
  (:68k-hstack-ext-limit #x10005608 8 :images (:68k-hosted)
   :constants ("MODUS-INT::+68K-HSTACK-EXT-LIMIT-ADDR+") :doc "TEST-ONLY limit.")
  (:68k-gc-static-block  #x10006000 #x24 :constants ("MODUS-INT::+68K-GC-STATIC-BLOCK+")
   :doc "Static boot heap: +0 request, +4/+8 objects, +C/+10 conses, +14 core flag,
   +18 card lo, +1C span, +20 shift (build-68k-cli.lisp).")
  (:68k-card-marks       #x10006100 #x1E00 :kind :range :constants ("MODUS-INT::+68K-CARD-MARKS+")
   :doc "One byte per static card, 7680 cards to #x10007F00.")
  (:68k-profile-counters #x10010000 #x8000 :kind :range :images (:68k-hosted)
   :constants ("MODUS-INT::+68K-PROFILE-BASE+")
   :shares (:argv-arena)
   :reason "NEAR-MISS, profiling builds only: the counters sit in the staged-argv
   arena's tail.  argv/envp strings longer than 24 KB, or read after the first
   profiled call, would be overwritten.  (translate-68k is mid-rewrite elsewhere.)"
   :doc "MODUS_68K_PROFILE builds only: a longword per function (~8K functions).")
  (:68k-card-crossing    #x1001E000 #x2000 :kind :range :constants ("MODUS-INT::+68K-CARD-CROSSING+")
   :doc "One byte per static card: its first object start.  The 8 KB below the heap."))

;;; The bare Mac (MODUS_MAC68K_MACHINE=...; boot/boot-mac68k.lisp): its words
;;; after the static block's, and the serial / console blocks in the argv area.
(define-fixed-words (:images (:68k-mac)
                     :owner "boot/boot-mac68k.lisp, mvm/translate-68k.lisp, mvm/mac68k-cold.lisp, mvm/build-cl-repl-common.lisp")
  (:mac68k-static-words #x1000602C #x5C :kind :range
   :doc "+2C the SAVE flag (the core run); +30 a core boot; +34/+38/+3C the paged
   code cache's pointer, end and start; +40 its owner map; +44/+48 the cold
   names' disk byte and index; +4C the cold functions' disk byte; +50 the Mac OS
   file's refNum (an application launch); +54 the Mac OS host table; +58/+5C the
   application's heap slot (semispace, kind map); +60/+64 its code area (the
   core run's too, :BOOT-JIT-AREA); +68 the cold floor; +6C the JIT's top; +70
   the relocation delta; +74 the kind-map move; +78 cold tables in RAM; +80/+84
   the old space's collection marks.")
  (:mac68k-serial-blocks #x10009000 #x300 :kind :range
   :doc "+000 .AOut's parameter block, +080 .AIn's, +100 the byte out, +102 the
   byte in, +10C the dead-port flag, +110 an EventRecord (GetOSEvent), +21C the
   screen console's off switch (translate-68k +68K-MAC-CONSOLE-CODE+,
   mac/mac-console.lisp)."))

(define-fixed-words (:images (:i386-hosted) :owner "mvm/translate-i386.lisp")
  (:i386-gcdbg        #x1001E000 #x20 :doc "*I386-GCDBG-BASE*: *I386-GC-INSTRUMENT* counters (32 bytes).")
  (:i386-hstack-frames #x1001E100 #x17E8 :kind :range :space :window
   :doc "*I386-HOSTED-HSTACK-BASE*: 127 frames x 24 bytes, to #x1001F8E8.")
  (:i386-hstack-ext   #x1001FF00 4 :constants ("MODUS.MVM.I386::+I386-HSTACK-EXT-ADDR+")
   :doc "Handler-frame extension address.")
  (:i386-hstack-ext-limit #x1001FF04 4 :constants ("MODUS.MVM.I386::+I386-HSTACK-EXT-LIMIT-ADDR+")
   :doc "TEST-ONLY limit."))

(define-fixed-word :i386-hosted-bss #x10000000 #x20000 :kind :container :images (:i386-hosted)
  :owner "boot/boot-linux-i386.lisp"
  :constants ("MODUS-INT::+LINUX-I386-BSS-ADDR+")
  :doc "The demand-zeroed BSS everything above lives in (to +LINUX-I386-BSS-END+).")

(define-fixed-words (:images (:arm32-hosted) :owner "mvm/translate-arm32.lisp")
  (:arm32-hstack-frames #x10020000 #x1FE0 :kind :range :space :window
   :doc "255 frames x 32 bytes at heap+#x20000 (ARM32-HFRAMES-ADDR).")
  (:arm32-hstack-ext #x10021FF0 4 :constants ("MODUS-INT::+ARM32-HSTACK-EXT-ADDR+")
   :doc "Handler-frame extension address.")
  (:arm32-hstack-ext-limit #x10021FF4 4 :constants ("MODUS-INT::+ARM32-HSTACK-EXT-LIMIT-ADDR+")
   :doc "TEST-ONLY limit."))

(define-fixed-words (:images (:rv-hosted) :owner "mvm/translate-riscv.lisp")
  (:rv-hstack-frames #x10020000 #x6000 :kind :range :space :window
   :doc "256 frames x 96 bytes at +RV-HOSTED-HSTACK-FRAMES-OFFSET+.")
  (:rv-hstack-ext #x10026000 8 :constants ("MODUS-INT::+RV-HSTACK-EXT-ADDR+")
   :doc "Handler-frame extension address.")
  (:rv-hstack-ext-limit #x10026008 8 :constants ("MODUS-INT::+RV-HSTACK-EXT-LIMIT-ADDR+")
   :doc "TEST-ONLY limit."))

;;; ---- Heaps, device space and windows: not words, but they own the bytes --

(define-fixed-word :x64-bare-heap #x10001000 (- #x1DFFF000 #x10001000) :kind :region
  :images (:x64-bare) :owner "boot/boot-x64.lisp"
  :constants ("MODUS-INT::+X64-MCGC-DATA-BASE+")
  :doc "Bare x86-64: semispaces / MCGC data from #x10001000 to +X64-MCGC-DATA-END+.")

(define-fixed-word :a64-bare-other-windows #x10020000 #x60000 :kind :region :images (:a64-bare)
  :owner "mvm/translate-aarch64.lisp (THE HANDLER-FRAME STACK, bare)"
  :doc "Main's window is #x10000000 with nothing else below #x10020000; bare
   windows are #x20000 apart (the Zero's records from #x10400000).")

(define-fixed-word :rv-bare-uart #x10000000 #x1000 :kind :region :images (:rv-bare)
  :owner "boot/boot-riscv.lisp" :constants ("MODUS-INT::+RISCV-UART-BASE+")
  :doc "QEMU virt NS16550 UART MMIO.  RISC-V bare's convention block is #x80700000."
  :shares (:gv-cache-root :gv-cache-guard :static-literal-root)
  :reason "KNOWN DEBT, unverified on silicon: the compiler bakes the global-cell
   cache root and the static-literal root as absolute #x10000FAx/FB0 for every
   target.  On QEMU virt #x10000100.. is unassigned (the UART is #x100 bytes),
   and the bare ladder passes; nothing there has been shown to read them.")

(define-fixed-word :arm32-bare-device-space #x10000000 #x200000 :kind :region :images (:arm32-bare)
  :owner "boot/boot-arm32.lisp"
  :doc "versatilepb/virt device space (system registers ..., the PL011 at #x101F1000).
   ARM32 bare's convention block is *ARM32-GLOBALS-BASE*.  (The bare CL image,
   *ARM32-CL-BARE*, keeps the HOSTED layout at #x10000000 and counts as
   :ARM32-HOSTED here.)"
  :shares (:gv-cache-root :gv-cache-guard :static-literal-root)
  :reason "KNOWN DEBT, unverified: as :RV-BARE-UART -- the compiler bakes these
   absolute roots for every target, including the bare ARM32 rungs.")

(define-fixed-words (:kind :region :owner "boot/boot-linux-*.lisp")
  (:arm32-hosted-heap #x10028000 (- #x1FC00000 #x10028000) :images (:arm32-hosted)
   :doc "The bump allocator, heap + +LINUX-ARM32-HEAP-ALLOC-START+.")
  (:rv-hosted-heap    #x10028000 (- #x1FC00000 #x10028000) :images (:rv-hosted)
   :doc "The bump allocator, heap + +LINUX-RISCV{,32}-HEAP-ALLOC-START+.")
  (:68k-hosted-heap   #x10020000 (- #x1FC00000 #x10020000) :images (:68k-hosted)
   :constants ("MODUS-INT::+68K-GC-HEAP-START+")
   :doc "The bump allocator, heap + +LINUX-68K-HEAP-ALLOC-START+ (<= 248 MB).")
  (:ppc-hosted-heap   #x10002000 (- #x1FC00000 #x10002000) :images (:ppc-hosted)
   :doc "The bump allocator, heap + +LINUX-PPC-HEAP-ALLOC-START+ (#x2000)."))

(define-fixed-word :gc-kind-map #x1FC00000 #x400000 :kind :region
  :images (:68k-hosted :ppc-hosted :arm32-hosted)
  :owner "translate-68k / -ppc / -arm32 collectors"
  :constants ("MODUS-INT::+68K-GC-KIND-MAP+" "MODUS-INT::+PPC-GC-KIND-MAP+")
  :doc "The collectors' kind map, fixed past the largest heap.")

(define-fixed-word :web-scratch #x10010000 #x5000 :kind :region :images (:web)
  :owner "mvm/build-web.lisp" :doc "*CSTR-SCRATCH* #x10010000, *IO-BUF-ADDR* #x10014000.")

;;; ==================================================================
;;; Run H1-H3 now: every build loads this file, so an overlap introduced in
;;; the registry fails the build before anything compiles.  H4 (the tree scan)
;;; and a re-run of H1-H3 (constants from boot descriptors loaded later) happen
;;; inside BUILD-IMAGE -- see the wrapper at the end of mvm/build-checks.lisp.
;;; ==================================================================
(check-fixed-address-registry :where "load")
