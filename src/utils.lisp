(in-package :toolbox)
(annot:enable-annot-syntax)

@eval-always
@export
(defun collect-if (body fn)
  (declare (type list body)
           (ftype (function (T) symbol)))
  (let ((collected-forms nil))
    (walk-tree (lambda (subtree)
                 (when (funcall fn subtree)
                   (push subtree collected-forms)))
               body)
    collected-forms))


@eval-always
@export
(defun match-symbols (list)
  (declare (type list list))
  (mapcar (lambda (x) (list (gensym) x))
          list))


@eval-always
@export
(defun comparsion-select (a b fn)
  (if (funcall fn a b)
      a
      b))


@eval-always
@export
(defun list-is-structured-as (list &rest arguments)
  (let ((current-level-arg (car arguments)))
    (or (and (null list) (null arguments))
        (and (eq (null list) (null arguments))
             (funcall current-level-arg (car list))
             (list-is-structured-as (cdr list)
                                    arguments)
             (and (atomp (car arguments))
                  (list-is-structured-as (car list)
                                         (cdr arguments)))))))


@eval-always
@export
(defun group-to-alist (list)
  (declare (type list list))
  (mapcar (lambda (x) (list* (car x) (cadr x)))
          (batches list 2)))


@export
(defun in-bounds (x lower upper)
  (declare (number x lower upper))
  (and (<= x upper)
       (>= x lower)))


@export
(defun group-ordered-sequence (group-fn sequence)
  (labels ((new-group (item)
             (adjustable-vector item))
           (add-element-to-group (group item)
             (vector-push-extend item group)))
    (iterate
      (for el in-sequence sequence)
      (for prev-el previous el initially nil)
      (with result = nil)
      (let ((first-iteration-p (if-first-time t nil)))
        (if (or first-iteration-p
                (not (funcall group-fn
                              (let ((group (car result)))
                                (aref group (1- (length group))))
                              el)))
            (push (new-group el) result)
            (add-element-to-group (car result) el)))
      (finally (return (reverse result))))))


@export
(defun is-ordered (sequence fn)
  (iterate
   (for elt in-sequence sequence)
   (for pelt previous elt)
   (if-first-time
    t
    (always (funcall fn pelt elt)))))


(defun ordered-with-car (sequence)
  (is-ordered sequence
              (lambda (prev next) (<= (car prev)
                                      (car next)))))
