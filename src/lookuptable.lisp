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
    :reader read-mask)))


@export
(defclass fixed-lookuptable-factory ()
  ((%replacer
    :type vector-replacer
    :initarg :replacer
    :reader read-replacer)))


@export
(defgeneric make-lookuptable (factory))

@export
(defgeneric copy-lookuptable (factory lookuptable new-size new-mask copy-mask))


(defmethod make-lookuptable ((factory fixed-lookuptable-factory))
  (make-instance 'fixed-lookuptable
                 :mask 0
                 :content (make-array 0)
                 :container (let ((container (make-instance 'vector-container)))
                              (setf (access-replacer container)
                                    (read-replacer factory))
                              container)))


(defmethod copy-lookuptable ((factory fixed-lookuptable-factory)
                             (lookuptable fixed-lookuptable)
                             new-size
                             new-mask
                             copy-mask)
  (declare (type list copy-mask)
           (type (unsigned-byte 32) new-size new-mask))
  (make-instance 'fixed-lookuptable
                 :replacer (let ((new-buffer (get-buffer (read-replacer factory)
                                                         (read-container lookuptable)
                                                         new-size)))
                             (iterate
                               (for (to from count) in copy-mask)
                               (copy-with-mask new-buffer
                                               (slot-value (read-container lookuptable)
                                                           '%content)
                                               (:from from :into to :times count)))
                             (make-instance 'vector-container :content new-buffer)))
                 :mask new-mask)


@export
(defun make-fixed-lookuptable-factory (replacer)
  (declare (type vector-replacer replacer))
  (make-instance 'fixed-lookuptable-factory :replace replacer))


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
