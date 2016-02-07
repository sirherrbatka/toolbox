(in-package :toolbox)
(annot:enable-annot-syntax)

@export
(defun sharp-f (stream char subchar)
  "Function responsible for creating literal lambda (reader-macro)."
  (declare (ignore char subchar))
  (let ((vargs (gensym "#F"))
        (*rt* *readtable*)
        (*readtable* (copy-readtable)))
    (set-macro-character #\_ (lambda (stream char)
                               (declare (ignore char))
                               (let ((char (peek-char nil stream)))
                                 (cond
                                   ((digit-char-p char)
                                    (let ((token (read stream nil nil t)))
                                      `(nth ,token ,vargs)))
                                   ((char= char #\_)
                                    (let ((*readtable* *rt*))
                                      (read stream nil nil t)))
                                   ((char= char #\*)
                                    (read-char stream) ; ignore character
                                    vargs)
                                   (t `(first ,vargs))))))
    `(lambda (&rest ,vargs)
       ,(read stream nil nil t))))


(set-dispatch-macro-character #\# #\F #'sharp-f)

@export
(defmacro sortf (argument comparsion-fn)
  `(setf ,argument (sort ,argument ,comparsion-fn)))


@export
(defmacro destructuring-mapcar ((lambda-list argument) &body code)
  (with-gensyms (!input)
    `(mapcar (lambda (,!input)
               (destructuring-bind ,lambda-list ,!input
                 (progn ,@code)))
             ,argument)))


@export
(defmacro with-vector (((access-alias length-alias vector-alias) vector) &body body)
  `(symbol-macrolet ((,length-alias (array-dimension ,vector 0))
                     (,vector-alias ,vector))
     (labels ((,access-alias (index)
                (aref ,vector-alias index))
              ((setf ,access-alias) (new-value index)
                (setf (aref ,vector-alias index) new-value)))
       (declare (ignore (function ,access-alias) (function (setf ,access-alias))))
       (progn ,@body))))


@export
(defmacro alist-let (bindings alist &body body)
  (with-gensyms (!alist)
    `(let ((,!alist ,alist))
         (let ,(destructuring-mapcar ((symbol alist-symbol) bindings)
                 `(,symbol (cdr (assoc ,alist-symbol ,!alist))))
           ,@body))))


@export
(defmacro with-optional-values (bindings &body body)
  `(progn ,@(destructuring-mapcar ((variable value) bindings)
              `(setf ,variable (or ,variable ,value)))
          ,@body))


@eval-always
(defun make-condition-copy-lambda-form (input)
  (declare (type list input))
  (alist-let ((from :from)
              (to :to)
              (into :into)
              (times :times)
              (upto :upto)
              (overwrite :overwrite)
              (skip :skip)
              (into-shift :into-shift)
              (from-shift :from-shift)) (group-to-alist input)
    (with-optional-values ((from 0)
                           (to array-total-size-limit)
                           (times array-total-size-limit)
                           (into 0)
                           (upto array-total-size-limit)
                           (overwrite nil)
                           (skip nil)
                           (into-shift 1)
                           (from-shift 1))
      `(let ((current-d 0)
             (current-s 0)
             (copied-times 0)
             (first-time t))
         (lambda (d s index-d index-s)
           (if first-time
               (progn (setf first-time nil)
                      (values (- ,into index-d)
                              (- ,from index-s)
                              :move))
               (progn (setf current-d index-d)
                      (setf current-s index-s)
                      (if (or (> current-d ,upto)
                              (> current-s ,to)
                              (>= copied-times ,times))
                          (values 0 0 :end)
                          (let* ((will-overwrite (or (eq ,overwrite nil) (funcall ,overwrite d)))
                                 (will-skip (and (not (eq nil ,skip)) (funcall ,skip s)))
                                 (will-copy (and will-overwrite (not will-skip)))
                                 (d-shift (cond (will-copy ,into-shift)
                                                ((not will-overwrite) ,into-shift)
                                                (will-skip 0)
                                                (t ,into-shift))))
                            (when will-copy
                              (incf copied-times))
                            (values d-shift
                                    ,from-shift
                                    (if will-copy :copy :move)))))))))))


@export
(defmacro condition-copy-lambda (&body body)
  (let ((stage-forms (mapcar 'make-condition-copy-lambda-form body)))
    `(let ((stages (list ,@stage-forms))
           (current-d 0)
           (current-s 0))
       (lambda (d s)
         (if (endp stages)
             (values 0 0 :end)
             (multiple-value-bind (deltad deltas operation)
                 (funcall (car stages) d s current-d current-s)
               (incf current-d deltad)
               (incf current-s deltas)
               (if (eq :end operation)
                   (progn (setf stages (cdr stages))
                          (values deltad deltas :move))
                   (values deltad deltas operation))))))))


@export
(defmacro copy-with-mask (destination source &rest forms)
  `(condition-copy ,destination ,source
                   (condition-copy-lambda
                     ,@forms)
                   0
                   0))


@export
(defmacro import-all-internal-package-symbols (from-package to-package)
  (let ((from-package (find-package from-package))
        (to-package (find-package to-package)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (do-symbols (symbol ,from-package)
         (when (nth-value 1 (find-symbol (string-upcase symbol) ,from-package))
           (handler-bind (#+sbcl(SB-EXT::NAME-CONFLICT
                                  #'(lambda (x)
                                      (invoke-restart (find-restart 'SB-IMPL::DONT-IMPORT-IT x)))))
             (import symbol ,to-package)))))))
