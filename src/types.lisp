(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(deftype byte-specifer-list ()
  `(and list (satisfies ordered-with-car)))


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
(deftype index () `(unsigned-byte 32))
