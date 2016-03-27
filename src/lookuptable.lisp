(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(define-condition lookuptable-does-not-contain-item (error)
  ((%index
    :initarg :index
    :type index
    :reader read-index)))


(-> copy-into-new-content (vector vector integer &key (:new-elements integer) (:removed-elements integer)) vector)
(defun copy-into-new-content (new-content existing-content old-mask &key (new-elements 0) (removed-elements 0))
  (let ((position 0))
    (declare (dynamic-extent position))
    (assert (~> (logcount new-elements)
                (+ (logcount old-mask) _)
                (- _ (logcount removed-elements))
                (= (array-dimension new-content 0)
                   _)))
    (assert (= (logcount old-mask)
               (array-dimension existing-content 0)))
    (assert (zerop (logand old-mask new-elements)))
    (assert (= removed-elements (logand old-mask removed-elements)))
    (assert (zerop (logand new-elements removed-elements)))
    (labels ((done ()
               (= (array-dimension existing-content 0) position))
             (copy-into (destination)
               (progn (setf (aref new-content destination)
                            (aref existing-content position))
                      (incf position)
                      (done))))
      (unless (zerop old-mask)
        (iterate
          (for mask-shift initially old-mask then (ash mask-shift -1))
          (for new-element-shift initially new-elements then (ash new-element-shift -1))
          (for removed-element-shift initially removed-elements then (ash removed-element-shift -1))
          (with destination = 0)
          (when (cond ((= (logand 1 removed-element-shift 1) 1)
                       (progn (incf position 1)
                              (done)))
                     ((= (logand 1 new-element-shift) 1)
                      (progn (incf destination 1)
                             nil))
                     ((= (logand 1 mask-shift) 1)
                      (prog1 (copy-into destination)
                        (incf destination 1)))
                     (t nil))
           (finish))))))
  new-content)


@export
(define-struct-template lookuptable-template (mask content)
    ((%mask :type mask)
     (%content :type content))
    ((access (index)
             ((declare (type index index))
              (let ((mask (slot-value instance '%mask)))
                (if (= 1 (ldb (byte 1 index)
                              mask))
                    (aref (slot-value instance '%content)
                          (logcount (ldb (byte index 0) mask)))
                    (error 'lookuptable-does-not-contain-item :index index)))))

     (set-value (index value)
                ((declare (type index index))
                 (let ((mask (slot-value instance '%mask)))
                   (if (= 1 (ldb (byte 1 index)
                                 mask))
                       (setf (aref (slot-value instance '%content)
                                   (logcount (ldb (byte index 0) mask)))
                             value)
                       (error 'lookuptable-does-not-contain-item :index index)))))

     (insert-into-copy (new-content-fn index-value-pairs)
                       ((let* ((mask (slot-value instance '%mask))
                               (new-mask (reduce (lambda (prev next) (dpb 1 (byte 1 (car next)) prev))
                                                 index-value-pairs
                                                 :initial-value mask))
                               (difference (logxor mask new-mask))
                               (new-content (funcall new-content-fn (logcount new-mask))))
                          (copy-into-new-content new-content
                                                 (slot-value instance '%content)
                                                 mask
                                                 :new-elements difference)
                          (map nil (lambda (index.value)
                                     (setf (aref new-content (logcount (ldb (byte (car index.value) 0) new-mask)))
                                           (cdr index.value)))
                               index-value-pairs)
                          (make-another-instance :%mask new-mask
                                                 :%content new-content))))

     (remove-from-copy (new-content-fn index-value-pairs)
                       ((let* ((mask (slot-value instance '%mask))
                               (new-mask (reduce (lambda (prev next) (dpb 0 (byte 1 (car next)) prev))
                                                 index-value-pairs
                                                 :initial-value mask))
                               (difference (logxor mask new-mask))
                               (new-content (funcall new-content-fn (logcount new-mask))))
                          (copy-into-new-content new-content
                                                 (slot-value instance '%content)
                                                 mask
                                                 :removed-elements difference)
                          (make-another-instance :%mask new-mask
                                                 :%content new-content))))

     (elements-count ()
                     ((array-dimension (slot-value instance '%content)
                                       0)))

     (contains (index)
               ((= 1 (ldb (byte 1 index)
                          (slot-value instance '%mask)))))

     (mapcar-elements (fn)
                      ((map 'vector fn (slot-value instance '%content))))

     (reduce-elements (fn)
                      ((reduce fn (slot-value instance '%content))))))


@export
(defun maplookuptable (result-type function lookuptable)
  (map result-type
       function
       (read-%content lookuptable)))
