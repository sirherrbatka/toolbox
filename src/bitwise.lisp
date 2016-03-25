(in-package :toolbox)
(annot:enable-annot-syntax)


(-> map-positive-byte-regions (integer) list)
@export
(defun map-positive-byte-regions (value)
  (iterate
    (for i from 0 below (integer-length value))
    (for current-bit = (ldb (byte 1 i) value))
    (for prev-bit previous current-bit initially 0)
    (with start = 0)
    (cond ((and (= current-bit 0)
                (= prev-bit 1))
           (collect (list* start (- i start)) into result))
          ((= current-bit 1)
           (setf start i)))
    (finally (return result))))


(-> move-byte-regions (byte-specifer-list integer) integer)
@export
(defun move-byte-regions (byte-specifers moved-value)
  (let ((length (integer-length moved-value)))
    (reduce (lambda (prev next)
              (let ((starting-region (byte (- length (car next)) 0))
                    (shift (- (cdr next) (car next))))
                (+ (ldb starting-region prev)
                   (dpb 0 starting-region (ash prev shift)))))
            byte-specifers
            :initial-value moved-value)))


@export
(defun print-as-binary (data &optional (stream t))
  (declare (type integer data))
  (format stream "~b" data)
  (terpri)
  data)
