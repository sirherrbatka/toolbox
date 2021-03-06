(in-package :toolbox)
(annot:enable-annot-syntax)


@eval-always
(defun find-real-type (alias types)
  (cdr (find-if (lambda (x) (eq (car x) alias))
                types)))


@eval-always
(defun substitute-with-real-types (slots types)
  (mapcar (lambda (x) (if (atom x)
                          (list nil x)
                          (let ((initializer))
                            (do ((sub x (cdr sub))
                                 (type nil (when (eq :type (car sub)) (find-real-type (cadr sub) types)))
                                 (collected nil (cons (if type (car type) (car sub)) collected)))
                                ((endp sub) (list initializer (reverse collected)))
                              (when type
                                (setf initializer (cadr type)))))))
          slots))


@eval-always
(defun add-initializers (slots)
  (mapcar (lambda (x) (optima:match x
                        ((list nil it) it)
                        ((list initializer (optima:guard it (listp it)))
                         (append (list (car it) initializer)
                                 (cdr it)))
                        ((list initializer it) it)
                        (it it)))
          slots))


@eval-always
(defun get-struct-data-slots (types slots)
  (~> (substitute-with-real-types slots types)
      add-initializers))


(defmacro define-struct-functions (struct-type functions)
  (cons 'progn (mapcar (lambda (x) (destructuring-bind (function-symbol function-name lambda-list body) x
                                     (declare (ignore function-name))
                                     `(defun ,function-symbol ,(cons 'instance lambda-list)
                                        (declare (type ,struct-type instance))
                                        ,@body)))
                       functions)))


@eval-always
(defun get-functions (function-prefix variable functions body)
  (let ((symbols nil))
    `(labels ,(mapcar (lambda (x) (destructuring-bind (symbol name lambda-list body) x
                                     (declare (ignore body))
                                     `(,(let* ((symbolic-name (if (listp name)
                                                                  (symbol-name (cadr name))
                                                                  (symbol-name name)))
                                               (transformed-name (~> (string-upcase symbolic-name)
                                                                     (concatenate 'string (string-upcase function-prefix) _)
                                                                     (intern _))))
                                          (let ((label (if (listp name)
                                                           (list (car name) transformed-name)
                                                           transformed-name)))
                                            (push label symbols)
                                            label))
                                       ,lambda-list
                                       (,symbol ,variable ,@lambda-list))))
                 functions)
       (declare (ignore ,@(mapcar (lambda (x) (list 'function x))
                                  symbols)))
       ,@body)))


@eval-always
(defun get-with-struct-macro-name (name)
  (intern (string-upcase (with-output-to-string (out)
                           (format out "with-~a" (symbol-name name))))))


@eval-always
(defun struct-macro-expansion (functions name captured-variable function-prefix body)
  (with-gensyms (!variable)
    `(let ((,!variable ,captured-variable))
       (declare (type ,name ,!variable))
       ,(get-functions (or function-prefix "") !variable functions body))))


(defmacro define-with-struct-macro (name functions)
  `(defmacro ,(get-with-struct-macro-name name) ((captured-variable &optional function-prefix) &body body)
     (struct-macro-expansion '(,@functions) ',name captured-variable function-prefix body)))


@eval-always
(defun get-struct-name (name-and-options)
  (if (atom name-and-options)
      name-and-options
      (car name-and-options)))


(defmacro with-template-macrolets (struct-name &body body)
  (let ((function-name (~> (concatenate 'string "make-" (symbol-name struct-name))
                           string-upcase
                           (find-symbol _ (symbol-package struct-name)))))
    (assert function-name)
    `(macrolet ((make-another-instance (&rest arguments)
                  (let ((!function-name1 ',function-name))
                    `(,!function-name1 ,@arguments))))
       ,@body)))


@eval-always
(defun add-function-prefix (struct-name function-name)
  (assert (symbolp function-name))
  (~> (concat (symbol-name struct-name) "-" (symbol-name function-name))
      (string-upcase _)
      (intern _ *package*)))


(defmacro struct-definition-macro (name-and-options types slots functions)
  (let* ((struct-name (get-struct-name name-and-options))
         (prefixed-functions (mapcar (lambda (x) (cons (add-function-prefix struct-name (car x)) x))
                                     functions)))
    `(let ((success (defstruct ,name-and-options
                      ,@(get-struct-data-slots types slots))))
       (when success
         (with-template-macrolets ,struct-name
             (define-struct-functions ,struct-name ,prefixed-functions))
         (define-with-struct-macro ,struct-name ,prefixed-functions)
         success))))


@eval-always
(defun get-name-and-options (name)
  name)


@export
(defmacro define-struct-template (template-name (&rest types) (&rest slots) (&rest functions))
  (assert (symbolp template-name))
  (let ((!type-aliases types))
    `(defmacro ,template-name (name ,@types)
       (let ((!types1 (list ,@types))
             (!type-aliases1 ',!type-aliases)
             (!functions1 ',functions)
             (!slots1 ',slots))
         `(struct-definition-macro ,(get-name-and-options name)
                                   ,(mapcar 'cons !type-aliases1 !types1)
                                   ,!slots1
                                   ,!functions1)))))

;; (define-struct-template test ((:type (vector t))) (T1)
;;     ((x :type T1))
;;     ())
;; (test just-test (string (make-string 0)))
