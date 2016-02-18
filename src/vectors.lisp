(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defun adjustable-vector (&rest elements)
  "Creates and returns adjustable vector holding all elements"
  (let ((product (make-array '(0) :adjustable t :fill-pointer 0)))
    (dolist (el elements product)
      (vector-push-extend el product))))


@export
(defun choose (vector index &rest more-indexes)
  "Returns vector containing values stored in the vector under indexes."
  (declare (type vector vector)
           (type index index))
  (map 'vector
       (lambda (i) (aref vector i))
       (cons index more-indexes)))


@export
(defun range-sub-vector (vector start end)
  "Returns array displaced to the vector (starting with start, ending on end)"
  (declare (type vector vector)
           (type index start)
           (type index end))
  (make-array (- end start 1)
              :displaced-to vector
              :displaced-index-offset start))


@export
(defun fill-with (vector starting count fill-with)
  "Fills region of vector between starting and (+ starting count) with parameter passed with fill-with"
  (declare (type index count)
           (type index starting)
           (type vector vector))
  (iterate
    (with max = (min (+ count starting) (array-dimension vector 0)))
    (for i from starting below max)
    (setf (aref vector i) fill-with))
  vector)


@export
(defun left-shift-vector (vector starting-with shift fill-with)
  "Shifts elements of the vector from 0 to starting-with by shift towards smaller index. Elements may be removed from the vector,
   if shift would place them under negative index. Fills created gap with fill-with afterwards."
  (declare (type index shift)
           (type index starting-with)
           (type vector vector))
  (iterate
    (for i from 0 below starting-with)
    (setf (aref vector i)
          (aref vector (+ i shift))))
  (fill-with vector starting-with shift fill-with))


@export
(defun right-shift-vector (vector starting-with shift fill-with)
  "Shifts elements of the the vector for starting-with by shift toward higher index.
   Elements may be removed from the vector if shift would place them beyond length of the vector. Adjustable vectors are not resized.
   Fills created gap with fill-with afterwards."
  (declare (type (unsigned-byte 64) starting-with shift)
           (type vector vector))
  (let ((length (array-dimension vector 0)))
    (iterate
     (for i from (1- length) downto (+ starting-with shift))
     (setf (aref vector i)
           (aref vector (- i shift))))
    (fill-with vector starting-with shift fill-with)))


@export
(defun shift-vector-elements (vector starting-with shift fill-with)
  "Shifts elements either left or right (depending if shift is positive or negative)."
  (if (> shift 0)
      (right-shift-vector vector starting-with shift fill-with)
      (left-shift-vector vector starting-with (abs shift) fill-with)))


@export
(defun vector-insert (element index vector)
  "Inserts element in the adjustable vector"
  (declare (type vector vector)
           (type (integer 0) index))
  (when (> index (length vector))
    (error "index is to large to fit in the vector"))
  (if (adjustable-array-p vector)
      (if (array-has-fill-pointer-p vector)
          (vector-push-extend element vector)
          (vector-push element vector))
      (error "Array is not adjustable"))
  (shift-vector-elements vector index 1 element)
  vector)


(defun get-vector-fill-function (vector)
  (declare (type vector vector))
  (if (array-has-fill-pointer-p vector)
      (lambda (x) (vector-push-extend x vector))
      (let ((index -1))
        (lambda (x) (setf (aref vector (incf index))
                          x)))))


@export
(defun merge-ordered-vectors (comparsion-fn desired-size &rest vectors)
  "Merges ordered vectors into new ordered vector (according to the comparsion-fn). Returns new vector"
  (declare (type (function (t t) symbol) comparsion-fn))
  (assert (every (lambda (x) (is-ordered x comparsion-fn))
                 vectors))
  (iterate main-loop

    (with total-size = (reduce #'+
                               (mapcar (lambda (x)
                                         (array-dimension x 0))
                                       vectors)))
    (with product = (if (null desired-size)
                        (make-array total-size
                                    :adjustable t
                                    :fill-pointer 0)
                        (make-array (max total-size desired-size))))
    (with set-next-element = (get-vector-fill-function product))
    (with data = (mapcar (lambda (x) (list* 0 x))
                         vectors))

    (for current-position from 0 below total-size)
    (for prev = nil)
    (for next = nil)

    (for current = (iterate
                    (with minimum = nil)
                    (with product = nil)

                    (for el in data)
                    (for (index . vector) = el)
                    (when (and (< index (array-dimension vector 0))
                               (or  (null minimum)
                                    (funcall comparsion-fn (aref vector index) minimum)))
                      (setf minimum (aref vector index)
                            product el))
                    (finally (return product))))

    (funcall set-next-element (aref (cdr current) (car current)))
    (incf (car current))

    (finally (return-from main-loop product))))


@export
(defclass vector-replacer ()
  ()
  (:documentation "Fundamental class of vector replacer. Used to represent memory pools holding vector buffers."))


@export
(defvar *vector-replacer*)


@export
(defmacro with-vector-replacer (replacer &body body)
  `(let ((*vector-replacer* ,replacer))
     ,@body))


@export
(defgeneric resize-content (vector-container new-size copy-mask)
  (:documentation "Changes size of the content by taking new buffer from vector-replacer. Copies elements as described in the copy mask (nested list) afterwards.
                   May return old buffer to the vector-replacer afterwards."))


@export
(defgeneric copy-vector-container (vector-container new-size copy-mask))


@export
(defgeneric get-buffer (replacer container new-size)
  (:documentation "Returns vector buffer of lenght = new-size. It may contain garbage."))


@export
(defgeneric consume-buffer (replace old-buffer)
  (:documentation "Passes old-buffer to the replacer so it can be taken care of."))


@export
(defgeneric return-container (container))


@export
(defclass vector-container ()
  ((%content
    :type vector
    :initarg :content))
  (:documentation "Vector container is a class that acts as wrapper around vector-container
                   so It can delegate resizing of the vector to the separate object (that can act as a memory pool)"))


(defmethod return-container ((container vector-container))
  (when (slot-boundp container '%content)
    (let ((buffer (slot-value container '%content)))
      (slot-makunbound container '%content)
      (consume-buffer *vector-replacer* buffer)
      t)))


(defmethod copy-vector-container ((container vector-container) new-size copy-mask)
  (declare (type index new-size)
           (type vector-copy-list copy-mask))
  (let ((new-buffer (get-buffer *vector-replacer* container new-size)))
    (when (slot-boundp container '%content)
      (iterate
        (for (to from count) in copy-mask)
        (copy-with-mask new-buffer
                        (slot-value container '%content)
                        (:from from :into to :times count))))
    (let ((result (make-instance 'vector-container :content new-buffer)))
      result)))


(defmethod print-object ((object vector-container) stream)
  (format stream "<Vector-Container: ~a>" (if (slot-boundp object '%content)
                                              (slot-value object '%content)
                                              (make-array 0))))


@export
(defun content-length (container)
  (declare (type vector-container container))
  (if (slot-boundp container '%content)
      (array-dimension (slot-value container '%content) 0)
      0))


@export
(defun vector-container= (a b)
  (declare (type vector-container a b))
  (vector= (slot-value a '%content)
           (slot-value b '%content)))


@export
(defun access-content (vector-container index)
  "Accessor used to read values from vector-container (under index)"
  (declare (type vector-container vector-container)
           (type index index))
  (aref (slot-value vector-container '%content)
        index))


(defun ensure-buffer (container)
  (declare (type vector-container container))
  (unless (slot-boundp container '%content)
    (setf (slot-value container '%content)
          (get-buffer *vector-replacer* container 1))))


@export
(defun (setf access-content) (new-value
                              vector-container
                              index)
  "Writer for internal buffer"
  (declare (type vector-container vector-container)
           (type index index))
  (ensure-buffer vector-container)
  (with-vector ((va l v) (slot-value vector-container '%content))
    (when (>= index l)
      (resize-content vector-container
                      (1+ index)
                      (list  (list 0 0 index)
                             (list (1+ index) index array-total-size-limit))))
    (setf (va index) new-value)))


@export
(defun insert-into-content (vector-container new-value index)
  "First: resize content. Next copy content into new vector."
  (declare (type vector-container vector-container)
           (type index index))
  (let ((was-not-initialized (ensure-buffer vector-container)))
    (with-vector ((va l v) (slot-value vector-container '%content))
      (let ((new-size (1+ (max index (if was-not-initialized 0 l)))))
        (resize-content vector-container
                        new-size
                        (list (list 0 0 index)
                              (list (1+ index) index array-total-size-limit))))
      (setf (va index) new-value)
      vector-container)))


@export
(defclass vector-pool (vector-replacer)
  ()
  (:documentation "Fundamental class for all vector pools."))


@export
(defclass hash-vector-pool (vector-pool)
  ((%buffers
    :type hash-table
    :initform (make-hash-table)
    :accessor access-buffers))
  (:documentation "Vector pool that uses hash table to store vectors."))


@export
(defclass fixed-vector-pool (vector-pool)
  ((%buffers
    :type (vector :type 'list)
    :initarg :buffers
    :accessor access-buffers)
   (%smallest-buffer-size
    :type index
    :reader read-smallest-buffer-size
    :initarg :smallest-buffer-size)
   (%largest-buffer-size
    :type index
    :reader read-largest-buffer-size
    :initarg :largest-buffer-size))
  (:documentation "Vector pool that stores buffers in vector. Since vector needs to bounded. It can only supply buffers of certain length"))
(export '(read-smallest-buffer-size largest-buffer-size))


@export
(defun make-fixed-vector-pool (smallest-buffer largest-buffer)
  (declare (type index smallest-buffer largest-buffer))
  (make-instance 'fixed-vector-pool
                 :buffers (make-array (- largest-buffer smallest-buffer))
                 :smallest-buffer-size smallest-buffer
                 :largest-buffer-size largest-buffer))


(defrequirement get-buffer ((replace fixed-vector-pool) (container vector-container) new-size)
  "the new-size is in bounds passed to the fixed-vector-pool"
  (declare (ignore container))
  (with-accessors ((lower read-smallest-buffer-size)
                   (upper read-largest-buffer-size)
                   (buffers access-buffers)) replace
    (in-bounds new-size lower upper)))


(defmethod get-buffer ((replace fixed-vector-pool) (container vector-container) new-size)
  (declare (type index new-size))
  (with-accessors ((lower read-smallest-buffer-size)
                   (upper read-largest-buffer-size)
                   (buffers access-buffers)) replace
    (let ((position (- new-size lower)))
      (unless (aref buffers position)
        (push (make-array new-size)
              (aref buffers position)))
      (prog1
          (car (aref buffers position))
        (setf (aref buffers position)
              (cdr (aref buffers position)))))))


(defmethod get-buffer ((replacer hash-vector-pool) (container vector-container) new-size)
  (declare (type index new-size))
  (let ((buffers (gethash new-size (access-buffers replacer))))
    (unless buffers
      (push (make-array new-size) buffers))
    (setf (gethash new-size (access-buffers replacer))
          (cdr buffers))
    (car buffers)))


(defmethod consume-buffer ((replace hash-vector-pool) old-buffer)
  (declare (type vector old-buffer))
  (let ((size (array-dimension old-buffer 0)))
    (push old-buffer (gethash size (access-buffers replace)))
    replace))


(defmethod resize-content ((vector-container vector-container) new-size copy-indexes)
  (let ((new-content (get-buffer *vector-replacer* vector-container new-size)))
    (when (slot-boundp vector-container '%content)
      (let ((old-content (slot-value vector-container '%content)))
        (iterate
          (for current in copy-indexes)
          (when current
            (destructuring-bind (to from count) current
              (copy-with-mask new-content
                              (slot-value vector-container '%content)
                              (:from from :into to :times count)))))
        (consume-buffer *vector-replacer* old-content)))
    (setf (slot-value vector-container '%content)
          new-content)
    new-content))


@export
(defun condition-copy (destination source condition-fn destination-start source-start)
  "Universal vector copy function. Will copy elements from source to destination. Process is controled by condition-fn.
   Condition-fn takes two arguments (current destination element and current source element) and returns 3 values:
   first one describes how destination should change, second one describes how source position should change,
   third one is operation that will be performed.

   Position changes are described as changes to the index position. Those may be either positive or negative,
   for instance: 2 means increasing index by two, while -1 means decreasing index by one.

   Operations are keyword symbols :copy :move :move-copy :end. Those have the following meaning:
   :copy first copy source to destination, next move by positions returned.
   :move move by positions returned
   :move-copy first move by positions returned, next copy source to destination
   :end finish copying. This function will return."
  (declare (type vector destination source)
           (type index destination-start source-start))
  (block outer
    (with-vector ((ad ld d) destination)
      (with-vector ((as ls s) source)
        (let ((copy nil))
          (loop while (when (and (< destination-start ld)
                                 (< source-start ls))
                        (when copy
                          (setf (ad destination-start) (as source-start)
                                copy nil))
                        (multiple-value-bind (first second operation)
                            (funcall condition-fn (ad destination-start) (as source-start))
                          (case operation
                            (:copy (progn (setf (ad destination-start)
                                                (as source-start))
                                          (incf destination-start first)
                                          (incf source-start second)))
                            (:move (progn (incf destination-start first)
                                          (incf source-start second)))
                            (:move-copy (progn (incf destination-start first)
                                               (incf source-start second)
                                               (setf copy t)))
                            (:end (return-from outer))))))))))
  destination)
