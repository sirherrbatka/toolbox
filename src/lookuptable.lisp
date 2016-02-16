(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defclass fixed-lookuptable ()
  ((%container
    :type vector-container
    :reader read-container
    :initarg :container)
   (%mask
    :type (unsigned-byte 32)
    :initarg :mask
    :reader read-mask))
  (:documentation "Lookuptable that can hold up to 32 elements, under 32 indexes (from 0 to 31)."))


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
(defgeneric make-lookuptable (factory))


(defgeneric copy-lookuptable (factory lookuptable new-mask copy-mask))


@export
(defgeneric insert-into-copy (lookuptable &rest elements)
  (:documentation "Create copy of the lookuptable, next place elements (can overwrite existing content) into created copy. Finally return created copy."))


(defmethod make-lookuptable ((factory fixed-lookuptable-factory))
  (make-instance 'fixed-lookuptable
                 :mask 0
                 :container (let ((container (make-instance 'vector-container)))
                              container)))


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
  (make-instance 'fixed-lookuptable
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


(defmacro with-lookuptable ((&key container-position access) lookuptable &body body)
  (with-gensyms (!lookuptable)
    (let ((!container-position (or container-position (gensym))))
      `(let ((,!lookuptable ,lookuptable))
         (labels ,(append
                   `((,!container-position (index) (apply-mask-to-index index (read-mask ,!lookuptable))))
                   (when access
                     `((,access (index) (access-content (read-container ,!lookuptable) (,!container-position index)))
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


@export
(defun apply-mask-to-index (index mask)
  (declare (type index index)
           (type (unsigned-byte 32) mask))
  (assert (< index 32))
  (logcount (ldb (byte index 0) mask)))


@export
(defun lookuptable-contains-item-under-index (lookuptable index)
  (declare (type fixed-lookuptable lookuptable)
           (type index index))
  (= 1 (ldb (byte 1 index)
            (read-mask lookuptable))))


@export
(defun alter-mask (index mask)
  (declare (type index index)
           (type (unsigned-byte 32) mask))
  (assert (< index 32))
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
(defgeneric returun-lookuptable (lookuptable))


(defmethod return-lookuptable ((lookuptable fixed-lookuptable))
  (return-container (read-container lookuptable))
  (setf (slot-value lookuptable '%mask) 0))
