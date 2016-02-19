(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defun map-positive-byte-regions (value)
  (declare (type integer value))
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


@export
(defun move-byte-regions (byte-specifers moved-value)
  (declare (type (unsigned-byte 32) moved-value)
           (type byte-specifer-list byte-specifers))
  (reduce (lambda (prev next)
            (let ((starting-region (byte (- 32 (car next)) 0))
                  (shift (- (cdr next) (car next))))
              (+ (ldb starting-region prev)
                 (dpb 0 starting-region (ash prev shift)))))
          byte-specifers
          :initial-value moved-value))


@export
(defun print-as-binary (data &optional (stream t))
  (declare (type integer data))
  (format stream "~b" data)
  (terpri)
  data)
