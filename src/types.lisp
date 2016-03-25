(in-package :toolbox)
(annot:enable-annot-syntax)


(defun byt-specifer-listp (list)
  (declare (type list list))
  (and (ordered-with-car list)
       (every (lambda (x)
                (and (non-negative-integer-p (car x))
                     (non-negative-integer-p (cdr x))))
              list)))


@export
(deftype byte-specifer-list ()
  `(and list
        (satisfies byte-specifer-listp)))


(defun vector-copy-listp (list)
  (declare (type list list))
  (every (lambda (x)
           (and (listp x)
                (= 3 (length x))
                (every #'non-negative-integer-p x)))
         list))


@export
(deftype vector-copy-list ()
  `(and list (satisfies vector-copy-listp)))


@export
(deftype index () `(integer 0 ,array-total-size-limit))
