;;;; split-dispatch.lisp -- build-time split of a name-hash COND dispatcher
;;;;
;;;; COMPILE-COMPOUND is one COND of ~300 clauses, each keyed on the operator's
;;;; name hash ((= OP-NAME 338547669) ...  ; QUOTE).  An ordinary call falls
;;;; through every test to the last clause, so compiling (FOO X) runs through the
;;;; whole 96 KB function.  On a workstation that is a few hundred compares.  On a
;;;; 1 MB Mac, whose code pages through a 136 KB cache, it pulls most of the cache
;;;; through on every form.  Profiled on DEFMACRO, it evicted the small leaves
;;;; (GETHASH, STRING=, LENGTH ...) that make up over half of the stream loads.
;;;;
;;;; SPLIT-COND-DISPATCH rewrites the image's copy of the source text, not the
;;;; file (the host's compiler keeps the monolith):
;;;;
;;;;   - the leading clauses that are not keyed on the hash stay where they are;
;;;;   - every other clause goes, verbatim, into a handler function, one per
;;;;     distinct sequence of clauses a key can reach (its own keyed clauses
;;;;     and every unkeyed clause, in the original order, cut after the first
;;;;     clause whose test is true for that key alone);
;;;;   - the COND's tail becomes a binary search over the keys that calls the
;;;;     key's handler, or the handler of the unkeyed clauses.
;;;;
;;;; Which clause runs for a given form is unchanged: a keyed test is false for
;;;; any other key, and its guard (an AND's later conjuncts) is only evaluated
;;;; when the key matches, as before.  A test this cannot classify counts as
;;;; unkeyed, which is always safe -- it is kept, in order, in every handler.
;;;; Only keys known at build time count: integers (#.(COMPUTE-NAME-HASH ...) is
;;;; one by the time it is read) and (COMPUTE-NAME-HASH "NAME").

(in-package :cl-user)

(defun %sd-skip (text pos)
  "Index of the next character of TEXT at or after POS that is not whitespace
   or inside a comment."
  (loop
    (let ((c (char text pos)))
      (cond ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf pos))
            ((char= c #\;) (setq pos (or (position #\Newline text :start pos) (length text))))
            ((and (char= c #\#) (char= (char text (1+ pos)) #\|))
             (setq pos (+ 2 (search "|#" text :start2 pos))))
            (t (return pos))))))

(defun %sd-sym= (x name)
  (and (symbolp x) (string-equal (symbol-name x) name)))

(defun %sd-literal (x)
  "X's value if it is a key known at build time: an integer, or
   (COMPUTE-NAME-HASH \"NAME\"), which the image computes as the host does (the
   #.(COMPUTE-NAME-HASH ...) keys beside it rely on that already).  Else NIL."
  (cond ((integerp x) x)
        ((and (consp x) (%sd-sym= (car x) "COMPUTE-NAME-HASH")
              (stringp (second x)) (null (cddr x)))
         (funcall (find-symbol "COMPUTE-NAME-HASH" :modus.mvm) (second x)))))

(defun %sd-keys (test var)
  "The keys TEST can be true for: (VALUES KEYS EXACT), where EXACT means TEST
   is true for every one of them.  :ALL when it is not a test on VAR's value."
  (flet ((var-p (x) (%sd-sym= x var)))
    (cond ((atom test) :all)
          ((and (%sd-sym= (car test) "=") (= (length test) 3))
           (cond ((and (var-p (second test)) (%sd-literal (third test)))
                  (values (list (%sd-literal (third test))) t))
                 ((and (var-p (third test)) (%sd-literal (second test)))
                  (values (list (%sd-literal (second test))) t))
                 (t :all)))
          ((and (%sd-sym= (car test) "MEMBER") (= (length test) 3) (var-p (second test))
                (consp (third test)) (%sd-sym= (car (third test)) "QUOTE")
                (listp (second (third test))) (every #'integerp (second (third test))))
           (values (copy-list (second (third test))) t))
          ((and (%sd-sym= (car test) "OR") (cdr test))
           (let ((keys nil) (exact t))
             (dolist (sub (cdr test) (values (remove-duplicates keys) exact))
               (multiple-value-bind (k e) (%sd-keys sub var)
                 (when (eq k :all) (return :all))
                 (setq keys (append keys k))
                 (unless e (setq exact nil))))))
          ((and (%sd-sym= (car test) "AND") (cdr test))
           (multiple-value-bind (k e) (%sd-keys (second test) var)
             (if (eq k :all)
                 :all
                 (values k (and e (null (cddr test)))))))
          (t :all))))

(defun split-cond-dispatch (text &key head cond-marker (var "OP-NAME") lambda-list
                                      block prefix (package :modus.mvm) (leaf 4)
                                      (inline-max 400))
  "Split the COND that starts at COND-MARKER, after HEAD (a DEFUN's first
   line), into handler functions named PREFIX-n, as described above.  LAMBDA-LIST
   is the handlers' parameters (a string): every variable the clauses use.
   BLOCK is the DEFUN's name, for (RETURN-FROM BLOCK ...) inside a clause.
   A key that reaches exactly one clause, true for that key alone and at most
   INLINE-MAX characters, gets no handler: its clause's body goes into the
   search's leaf (a handler costs an image an entry stub and a stream-table
   slot, ~20 bytes resident -- the 1 MB Mac's map had 3 KB to spare).
   Returns the new text, the number of handlers and the number of keys."
  (let* ((var (string-downcase var))
         (a (or (search head text) (error "split-dispatch: no ~S" head)))
         (c (or (search cond-marker text :start2 a) (error "split-dispatch: no COND after ~S" head)))
         (pos (+ c (length cond-marker)))
         (in (make-string-input-stream text))
         (clauses nil))
    ;; the clauses: start, end, keys, exact
    (loop
      (setq pos (%sd-skip text pos))
      (when (char= (char text pos) #\)) (return))
      (file-position in pos)
      (let ((form (let ((*package* (find-package package)) (*read-eval* t)) (read-preserving-whitespace in))))
        (multiple-value-bind (keys exact) (%sd-keys (car form) var)
          ;; and where the test ends, for a clause put in a leaf
          (push (list pos (file-position in) keys exact
                      (progn (file-position in (%sd-skip text (1+ pos)))
                             (let ((*package* (find-package package)) (*read-eval* t)) (read-preserving-whitespace in))
                             (file-position in)))
                clauses))
        (file-position in (second (car clauses)))
        (setq pos (file-position in))))
    (setq clauses (coerce (nreverse clauses) 'vector))
    (let* ((cond-end pos)
           (n (length clauses))
           (nprefix (or (position-if-not (lambda (cl) (eq (third cl) :all)) clauses)
                        (error "split-dispatch: no keyed clause in ~S" head)))
           (all-keys (sort (remove-duplicates
                            (loop for i from nprefix below n
                                  unless (eq (third (aref clauses i)) :all)
                                    append (third (aref clauses i))))
                           #'<))
           (groups (make-hash-table :test 'equal))
           (order nil)
           (handler-of (make-hash-table))
           (inline-of (make-hash-table)))
      (flet ((seq-for (key)
               (let ((seq nil))
                 (loop for i from nprefix below n
                       for (nil nil keys exact) = (aref clauses i)
                       do (cond ((eq keys :all) (push i seq))
                                ((and key (member key keys))
                                 (push i seq)
                                 (when exact (return)))))
                 (nreverse seq)))
             (group (seq)
               (or (gethash seq groups)
                   (let ((name (format nil "~A-~D" prefix (hash-table-count groups))))
                     (push seq order)
                     (setf (gethash seq groups) name)))))
        (let ((generic (group (seq-for nil))))
          (dolist (k all-keys)
            (let* ((seq (seq-for k))
                   (cl (and (null (cdr seq)) (aref clauses (car seq)))))
              (if (and cl (fourth cl) (equal (third cl) (list k))
                       (<= (- (second cl) (first cl)) inline-max))
                  ;; the body, without the clause's test and closing paren
                  (setf (gethash k inline-of) (subseq text (fifth cl) (1- (second cl))))
                  (setf (gethash k handler-of) (group seq)))))
          (let* ((ret-from (format nil "(return-from ~A" block))
                 (handlers
                   (with-output-to-string (o)
                     (dolist (seq (reverse order))
                       (let ((name (gethash seq groups)))
                         (format o "(defun ~A ~A~%  (cond" name lambda-list)
                         (dolist (i seq)
                           (let ((ct (subseq text (first (aref clauses i)) (second (aref clauses i)))))
                             (loop for p = (search ret-from ct) while p
                                   do (setq ct (concatenate 'string (subseq ct 0 p)
                                                            (format nil "(return-from ~A" name)
                                                            (subseq ct (+ p (length ret-from))))))
                             (format o "~%    ~A" ct)))
                         (format o "))~%~%")))))
                 (args (string-trim "()" lambda-list))
                 (fixnum-limit (ash 1 29))
                 (small (remove-if-not (lambda (k) (< -1 k fixnum-limit)) all-keys))
                 (big (remove-if (lambda (k) (< -1 k fixnum-limit)) all-keys)))
            (labels ((call (name) (format nil "(~A ~A)" name args))
                     (chain (keys)
                       (format nil "(cond~{ ~A~} (t ~A))"
                               (mapcar (lambda (k)
                                         (if (gethash k inline-of)
                                             (format nil "((= ~A ~D)~A)" var k (gethash k inline-of))
                                             (format nil "((= ~A ~D) ~A)" var k (call (gethash k handler-of)))))
                                       keys)
                               (call generic)))
                     (tree (keys)
                       (if (<= (length keys) leaf)
                           (chain keys)
                           (let* ((mid (floor (length keys) 2))
                                  (pivot (nth mid keys)))
                             (format nil "(if (< ~A ~D)~% ~A~% ~A)" var pivot
                                     (tree (subseq keys 0 mid)) (tree (nthcdr mid keys)))))))
              (let ((dispatch (if big
                                  (format nil "(if (and (>= ~A 0) (< ~A ~D))~% ~A~% ~A)"
                                          var var fixnum-limit (tree small) (chain big))
                                  (tree small))))
                (values
                 (concatenate 'string
                              (subseq text 0 a)
                              handlers
                              (subseq text a (second (aref clauses (1- nprefix))))
                              (format nil "~%      (t ~A)" dispatch)
                              (subseq text cond-end))
                 (hash-table-count groups)
                 (length all-keys))))))))))
