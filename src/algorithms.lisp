(in-package :toolbox)
(annot:enable-annot-syntax)

@export
(defun lower-bound (vector element comparsion-fn)
  "Searches for the first element in the vector that when passed as first
   argument to comparsion-fn (along with element passed as second argument) will return non-nil value.
   Returns inedex pointing to the position of this element. If there is no such element, length of the vector is returned.
   Vector passed as a first argument should be ordered, since this function performs binary search."
  (declare (type vector vector))
  (cond ((zerop (length vector))
         0)
        ((not (funcall comparsion-fn (aref vector 0) element))
         0)
        ((funcall comparsion-fn (aref vector (1- (length vector))) element)
         (length vector))
        (t
         (iter
          (with start = 0)
          (with end = (1- (length vector)))
          (finding start such-that (= start end))
          (for middle = (+ start (floor (/ (- end start) 2))))
          (if (funcall comparsion-fn (aref vector middle) element)
              (setf start (1+ middle))
              (setf end middle))))))

@export
(defun order-by (sequence select-fn)
  "Sorts container in ascending order by element returned by select-fn. Mutates passed sequence."
  (declare (type sequence sequence))
  (sortf sequence
         (lambda (a b)
           (< (funcall select-fn a)
              (funcall select-fn b)))))
