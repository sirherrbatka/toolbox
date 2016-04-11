(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defun adjustable-vector (&rest elements)
  "Creates and returns adjustable vector holding all elements"
  (let ((product (make-array '(0) :adjustable t :fill-pointer 0)))
    (dolist (el elements product)
      (vector-push-extend el product))))


(-> choose (vector index &rest index) vector)
@export
(defun choose (vector index &rest more-indexes)
  "Returns vector containing values stored in the vector under indexes."
  (map 'vector
       (lambda (i) (aref vector i))
       (cons index more-indexes)))


(-> range-sub-vector (vector index index) vector)
@export
(defun range-sub-vector (vector start end)
  "Returns array displaced to the vector (starting with start, ending on end)"
  (make-array (- end start 1)
              :displaced-to vector
              :displaced-index-offset start))


(-> fill-with (vector index index t) vector)
@export
(defun fill-with (vector starting count fill-with)
  "Fills region of vector between starting and (+ starting count) with parameter passed with fill-with"
  (iterate
    (with max = (min (+ count starting) (array-dimension vector 0)))
    (for i from starting below max)
    (setf (aref vector i) fill-with))
  vector)

(-> left-shift-vector (vector index index t) vector)
@export
(defun left-shift-vector (vector starting-with shift fill-with)
  "Shifts elements of the vector from 0 to starting-with by shift towards smaller index. Elements may be removed from the vector,
   if shift would place them under negative index. Fills created gap with fill-with afterwards."
  (iterate
    (for i from 0 below starting-with)
    (setf (aref vector i)
          (aref vector (+ i shift))))
  (fill-with vector starting-with shift fill-with))


(-> right-shift-vector (vector index index t) vector)
@export
(defun right-shift-vector (vector starting-with shift fill-with)
  "Shifts elements of the the vector for starting-with by shift toward higher index.
   Elements may be removed from the vector if shift would place them beyond length of the vector. Adjustable vectors are not resized.
   Fills created gap with fill-with afterwards."
  (let ((length (array-dimension vector 0)))
    (iterate
     (for i from (1- length) downto (+ starting-with shift))
     (setf (aref vector i)
           (aref vector (- i shift))))
    (fill-with vector starting-with shift fill-with)))


(-> shift-vector-elements (vector index index t) vector)
@export
(defun shift-vector-elements (vector starting-with shift fill-with)
  "Shifts elements either left or right (depending if shift is positive or negative)."
  (if (> shift 0)
      (right-shift-vector vector starting-with shift fill-with)
      (left-shift-vector vector starting-with (abs shift) fill-with)))


(-> vector-insert (t index vector) vector)
@export
(defun vector-insert (element index vector)
  "Inserts element in the adjustable vector"
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


(-> merge-ordered-vectors ((function (t t) symbol) t &rest vector) vector)
@export
(defun merge-ordered-vectors (comparsion-fn desired-size &rest vectors)
  "Merges ordered vectors into new ordered vector (according to the comparsion-fn). Returns new vector"
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
(defclass vector-pool ()
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


(defmethod initialize-instance ((instance fixed-vector-pool)
                                &key smallest-buffer-size largest-buffer-size
                                &allow-other-keys)
  (setf (slot-value instance '%buffers)
        (make-array (- largest-buffer-size smallest-buffer-size)
                    :element-type 'list
                    :initial-element nil))
  (call-next-method))


(-> make-fixed-vector-pool (index index) fixed-vector-pool)
@export
(defun make-fixed-vector-pool (smallest-buffer largest-buffer)
  (make-instance 'fixed-vector-pool
                 :smallest-buffer-size smallest-buffer
                 :largest-buffer-size largest-buffer))


@export
(defgeneric get-buffer-returning-function (replacer))


@export
(defgeneric make-buffer (replacer size))


@export
(defgeneric get-buffer-consuming-function (replacer))


(defmethod get-buffer-returning-function ((replace fixed-vector-pool))
  (lambda (new-size)
    (declare (type index new-size))
    (with-accessors ((lower read-smallest-buffer-size)
                     (upper read-largest-buffer-size)
                     (buffers access-buffers)) replace
      (let ((position (- new-size lower)))
        (unless (aref buffers position)
          (push (make-buffer replace new-size)
                (aref buffers position)))
        (prog1
            (car (aref buffers position))
          (setf (aref buffers position)
                (cdr (aref buffers position))))))))


(defmethod make-buffer ((replacer vector-pool) size)
  (declare (type index size))
  (make-array size))


(defmethod get-buffer-returning-function ((replacer hash-vector-pool))
  (lambda (new-size)
    (declare (type index new-size))
    (let ((buffers (gethash new-size (access-buffers replacer))))
      (unless buffers
        (push (make-array (list new-size)) buffers))
      (setf (gethash new-size (access-buffers replacer))
            (cdr buffers))
      (car buffers))))


(defmethod get-buffer-consuming-function ((replace hash-vector-pool))
  (lambda (old-buffer)
    (declare (type vector old-buffer))
    (let ((size (array-dimension old-buffer 0)))
      (push old-buffer (gethash size (access-buffers replace)))
      replace)))


(defmethod get-buffer-consuming-function ((replace fixed-vector-pool))
  (lambda (old-buffer)
    (declare (type vector old-buffer))
    (let ((size (array-dimension old-buffer 0)))
      (push old-buffer (aref (access-buffers replace) size))
      replace)))


@export
(defclass typed-vector-pool-mixin ()
  ((%produce-vector-function
    :type (-> (index) vector)
    :reader read-produce-vector-function
    :initarg :produce-vector-function)))


(defmethod make-buffer ((replacer typed-vector-pool-mixin) size)
  (declare (type index size))
  (funcall (read-produce-vector-function replacer) size))


(-> condition-copy (vector vector function index index) vector)
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
  (let ((dstart destination-start)
        (sstart source-start))
    (block outer
      (with-vector ((ad ld d) destination)
        (with-vector ((as ls s) source)
          (let ((copy nil))
            (loop while (when (and (< dstart ld)
                                   (< sstart ls))
                          (when copy
                            (setf (ad dstart) (as sstart)
                                  copy nil))
                          (multiple-value-bind (first second operation)
                              (funcall condition-fn (ad dstart) (as sstart))
                            (case operation
                              (:copy (progn (setf (ad dstart)
                                                  (as sstart))
                                            (incf dstart first)
                                            (incf sstart second)))
                              (:move (progn (incf dstart first)
                                            (incf sstart second)))
                              (:move-copy (progn (incf dstart first)
                                                 (incf sstart second)
                                                 (setf copy t)))
                              (:end (return-from outer)))))))))))
  destination)
