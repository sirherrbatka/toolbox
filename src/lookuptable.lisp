(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defclass fixed-lookuptable ()
  ((%container
    :type vector-container
    :reader read-container
    :initarg :container)
   (%mask
    :type (unsigned-byte 64)
    :initarg :mask
    :reader read-mask))
  (:documentation "Lookuptable that can hold up to 64 elements, under 64 indexes (from 0 to 63)."))


(defmethod print-object ((object fixed-lookuptable) stream)
  (format stream "<Lookuptable ~b: ~a>"
          (read-mask object)
          (read-container object)))


@export
(defclass fixed-lookuptable-factory ()
  ((%replacer
    :type vector-replacer
    :initarg :replacer
    :reader read-replacer)))


@export
(defvar *lookuptable-factory*)


@export
(defmacro with-lookuptable-factory (factory &body body)
  `(with-vector-replacer (read-replacer ,factory)
     (let ((*lookuptable-factory* ,factory))
       ,@body)))


@export
(defgeneric make-lookuptable (factory &optional elements))


(defgeneric copy-lookuptable (factory lookuptable new-mask copy-mask))


@export
(defgeneric insert-into-copy (lookuptable &rest elements)
  (:documentation "Create copy of the lookuptable, next place elements (can overwrite existing content) into created copy. Finally return created copy."))


(defmethod make-lookuptable ((factory fixed-lookuptable-factory) &optional (elements nil))
  (let* ((mask (coerce (reduce (lambda (prev next) (+ prev (ash 1 (car next))))
                               elements
                               :initial-value 0)
                       '(unsigned-byte 64)))
         (size (logcount mask)))
    (make-instance 'fixed-lookuptable
                   :mask mask
                   :container (let ((container (make-instance 'vector-container)))
                                (resize-content container size nil)
                                (map nil
                                     (lambda (x) (destructuring-bind (index . element) x
                                                   (let ((new-index (apply-mask-to-index index mask)))
                                                     (setf (access-content container new-index)
                                                           element))))
                                     elements)
                                container))))


(defun to-container-mask (copy-mask old-lookuptable-mask new-lookuptable-mask)
  (declare (type index new-lookuptable-mask old-lookuptable-mask)
           (type list copy-mask))
  (destructuring-mapcar ((to from count) copy-mask)
    (list (1+ (apply-mask-to-index from new-lookuptable-mask))
          (apply-mask-to-index from old-lookuptable-mask)
          count)))


(defmethod copy-lookuptable ((factory fixed-lookuptable-factory)
                             (lookuptable fixed-lookuptable)
                             new-mask
                             copy-mask)
  (declare (type vector-copy-list copy-mask)
           (type index new-mask))
  (make-instance (type-of lookuptable)
                 :container (copy-vector-container (read-container lookuptable)
                                                   (logcount new-mask)
                                                   copy-mask)
                 :mask new-mask))


(defmethod insert-into-copy ((lookuptable fixed-lookuptable) &rest elements)
  (let* ((vector-of-elements (order-by (map 'vector (lambda (x) (cons (lookuptable-contains-item-under-index lookuptable (car x))
                                                                      x))
                                            elements)
                                       #'cadr))

         (new-mask (reduce (lambda (prev next)
                             (alter-mask (cadr next)
                                         prev))
                           vector-of-elements
                           :initial-value (read-mask lookuptable)))

         (new-areas (~> (remove-if #'car vector-of-elements)
                        (map 'vector (lambda (x)
                                       (destructuring-bind (in index . value) x
                                         (declare (ignore in value))
                                         (list (apply-mask-to-index index new-mask)
                                               (apply-mask-to-index index (read-mask lookuptable)))))
                             _)
                        (group-ordered-sequence (lambda (last next)
                                                  (= 1 (- (car next) (car last))))
                                                _)
                        (mapcar (lambda (group)
                                  (let ((start (aref group 0))
                                        (end (aref group (1- (length group)))))
                                    (list start end)))
                                _)))

         (copy-mask (iterate
                      (for ((area-start-to area-start-from) (area-end-to area-end-from)) in new-areas)
                      (with from = 0)
                      (with to = 0)
                      (with result = nil)
                      (when (= from area-start-from)
                        (setf to (1+ area-end-to)))
                      (for count = (- area-start-from from))
                      (unless (zerop count)
                        (push (list to from count) result))
                      (incf from count)
                      (setf to (1+ area-end-to))
                      (finally (let ((count (- (elements-count lookuptable) from)))
                                 (when (positive-integer-p count)
                                   (push (list to from count) result))
                                 (return (reverse result))))))

         (result (copy-lookuptable *lookuptable-factory* lookuptable new-mask copy-mask)))

    (iterate
      (for elt in-vector vector-of-elements)
      (for (index . value) = (cdr elt))
      (setf (access-content-of-lookuptable result index)
            value))

    result))


@export
(defun fixed-lookuptable= (a b)
  (declare (type fixed-lookuptable a b))
  (and (= (read-mask a)
          (read-mask b))
       (vector-container= (read-container a)
                          (read-container b))))


@export
(defun make-fixed-lookuptable-factory (replacer)
  (declare (type vector-replacer replacer))
  (make-instance 'fixed-lookuptable-factory :replacer replacer))


@export
(define-condition lookuptable-does-not-contain-item (error)
  ((%index
    :initarg :index
    :type index
    :reader read-index)))


(defun copy-into-new-content (new-content existing-content old-mask difference) ;;TODO implement
  (declare (type vector new-content existing-content)
           (type integer old-mask difference))
  (assert (~> (logcount difference)
              (+ (logcount old-mask) _)
              (= (array-dimension new-content 0)
                 _)))
  (assert (= (logcount old-mask)
             (array-dimension existing-content 0)))
  (assert (zerop (logand old-mask difference)))
  (let ((position 0))
    (flet ((copy-into (destination)
             (progn (setf (aref new-content destination)
                          (aref existing-content position))
                    (incf position)
                    (= (array-dimension existing-content 0) position))))
      (unless (zerop old-mask)
        (iterate
          (for mask-shift initially old-mask then (ash mask-shift -1))
          (for difference-shift initially difference then (ash difference-shift -1))
          (with destination = 0)
          (when (cond ((= (logand 1 difference-shift) 1)
                       (progn (incf destination 1)
                              nil))
                      ((= (logand 1 mask-shift) 1)
                       (prog1 (copy-into destination)
                         (incf destination 1)))
                      (t nil))
            (finish)))))))


@export
(define-struct-template lookuptable-template () (mask content)
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

     ((setf access) (value index)
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
                                                 difference)
                          (map nil (lambda (index.value)
                                     (setf (aref new-content (logcount (ldb (byte (car index.value) 0) new-mask)))
                                           (cdr index.value)))
                               index-value-pairs)
                          (make-another-instance :%mask new-mask
                                                 :%content new-content))))

     (elements-count ()
                     ((array-dimension (slot-value instance '%content)
                                       0)))

     (mapcar-elements (fn)
                      ((map 'vector fn (slot-value instance '%content))))

     (reduce-elements (fn)
                      ((reduce fn (slot-value instance '%content))))))

(lookuptable-template 8-lookuptable
                      ((unsigned-byte 8) 0)
                      ((vector (unsigned-byte 8)) (make-array 0 :element-type '(unsigned-byte 8))))


(defmacro with-lookuptable ((&key container-position access) lookuptable &body body)
  (with-gensyms (!lookuptable)
    (let ((!container-position (or container-position (gensym))))
      `(let ((,!lookuptable ,lookuptable))
         (labels ,(append
                   `((,!container-position (index) (apply-mask-to-index index (read-mask ,!lookuptable))))
                   (when access
                     `((,access (index) (if (lookuptable-contains-item-under-index ,!lookuptable index)
                                            (access-content (read-container ,!lookuptable) (,!container-position index))
                                            (error 'lookuptable-does-not-contain-item :index index)))
                       ((setf ,access) (value index) (let ((real-position (,!container-position index)))
                                                       (if (lookuptable-contains-item-under-index ,!lookuptable index)
                                                           (setf (access-content (read-container ,!lookuptable)
                                                                                 real-position)
                                                                 value)
                                                           (insert-into-content (read-container ,!lookuptable)
                                                                                value
                                                                                real-position))
                                                       (setf (slot-value ,!lookuptable '%mask)
                                                             (alter-mask index (read-mask ,!lookuptable))))))))
           (declare (ignore (function ,!container-position)))
           ,(when access
              `(declare (ignore (function ,access) (function (setf ,access)))))
           ,@body)))))


(defun apply-mask-to-index (index mask)
  (declare (type index index)
           (type (unsigned-byte 64) mask))
  (assert (< index 64))
  (logcount (ldb (byte index 0) mask)))


@export
(defun maplookuptable (result-type function lookuptable)
  (map result-type
       function
       (read-content (read-container lookuptable))))


@export
(defun lookuptable-contains-item-under-index (lookuptable index)
  (declare (type fixed-lookuptable lookuptable)
           (type index index))
  (= 1 (ldb (byte 1 index)
            (read-mask lookuptable))))


(defun alter-mask (index mask)
  (declare (type index index)
           (type (unsigned-byte 64) mask))
  (assert (< index 64))
  (dpb 1 (byte 1 index) mask))


@export
(defun access-content-of-lookuptable (lookuptable index)
  (declare (type index index)
           (type fixed-lookuptable lookuptable))
  (with-lookuptable (:access access) lookuptable
    (access index)))


@export
(defun (setf access-content-of-lookuptable) (value lookuptable index)
  (declare (type index index)
           (type fixed-lookuptable lookuptable))
  (with-lookuptable (:access access) lookuptable
    (setf (access index)
          value)
    lookuptable))


@export
(defun elements-count (lookuptable)
  (declare (type fixed-lookuptable lookuptable))
  (content-length (read-container lookuptable)))


@export
(defgeneric return-lookuptable (lookuptable)
  (:documentation "Return lookuptable container to the memory pool"))


(defmethod return-lookuptable ((lookuptable fixed-lookuptable))
  (return-container (read-container lookuptable))
  (setf (slot-value lookuptable '%mask) 0))
