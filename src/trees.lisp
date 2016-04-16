(in-package :toolbox)
(annot:enable-annot-syntax)

#|

Higher order utility functions.

|#

(-> descend (t (-> (t) t) (-> (t t) t)) list)
(defun descend (tree go-in-fn next-fn)
  (assure list (do ((segment (funcall go-in-fn tree)
                             (funcall go-in-fn current))
                    (current tree
                             (funcall next-fn current segment))
                    (path nil (cons (list* current segment)
                                    path)))
                   ((null current) path))))


(-> ascend-copy (list t (-> (t t t) t)) t)
(defun ascend-copy (path seed copy-fn)
  (reduce (lambda (prev next)
            (destructuring-bind (node . segment) next
              (funcall copy-fn prev segment node)))
          path
          :initial-value seed))

#|

K-tree, NOT balanced right now.

|#

@export
(defstruct k-node
  (coordinates #(0 0) :type simple-vector)
  (content nil :type list)
  (neighboors #(nil nil nil nil) :type simple-vector))


(-> get-segment (k-node simple-vector) index)
(defun get-segment (node coordinates)
  (with-accessors ((coordinates2 k-node-coordinates)) node
    (assert (= (length coordinates)
               (length coordinates2)))
    (if (vector= coordinates coordinates2)
        0
        (iterate
          (for a in-vector coordinates)
          (for b in-vector coordinates2)
          (for result initially 0 then
               (logior (ash result 1)
                       (if (< a b) 0 1)))
          (finally (return (1+ result)))))))


(-> descend-into-k-tree (k-node simple-vector) list)
(defun descend-into-k-tree (root coordinates)
  (flet ((go-in (x) (get-segment x coordinates))
         (next (current segment) (and (not (zerop segment))
                                      (aref (k-node-neighboors current)
                                            (1- segment)))))
    (descend root #'go-in #'next)))


(-> ascend-copy-k-tree (list simple-vector t) k-node)
(defun ascend-copy-k-tree (path coordinates element)
  (let ((rest (assure list (if (zerop (cdar path))
                               (cdr path)
                               path)))
        (seed (assure k-node
                (with-accessors ((content k-node-content)
                                 (neighboors k-node-neighboors)) (caar path)
                  (if (zerop (cdar path))
                      (make-k-node :coordinates coordinates
                                   :content (cons element content)
                                   :neighboors neighboors)
                      (make-k-node :coordinates coordinates
                                   :content (list element)
                                   :neighboors (make-array (array-dimensions neighboors)
                                                           :initial-element nil)))))))
    (flet ((copy-node (child segment copied)
             (with-accessors ((content k-node-content)
                              (coordinates k-node-coordinates)
                              (neighboors k-node-neighboors)) copied
               (make-k-node :coordinates coordinates
                            :content content
                            :neighboors (let ((content (copy-array neighboors)))
                                          (setf (aref content (1- segment))
                                                child)
                                          content)))))
      (assure k-node (ascend-copy rest seed #'copy-node)))))


(-> insert-into-k-tree (k-node simple-vector t) k-node)
@export
(defun insert-into-k-tree (root coordinates element)
  (ascend-copy-k-tree (descend-into-k-tree root coordinates)
                      coordinates
                      element))


(-> find-in-k-tree (k-node simple-vector) t)
@export
(defun find-in-k-tree (root coordinates)
  (iterate
   (for node initially root then
         (with-accessors ((neighboors k-node-neighboors)) node
           (let ((segment (get-segment node coordinates)))
             (if (zerop segment)
                 (leave node)
                 (aref neighboors (1- segment))))))
   (while node)))


#|

Persitant hash table (like in Clojure)

|#


(lookuptable-template hash-node
                      ((unsigned-byte 64) 0)
                      (vector (vector)))


(-> descend-into-hash-tree (hash-node t) list)
(defun descend-into-hash-tree (root hash)
  (let ((times 0))
    (declare (dynamic-extent times))
    (flet ((go-in (x)
             (let ((index (ldb (byte 6 times) hash)))
               (prog1
                   (list* (and (not (eq t x))
                               (hash-node-contains x index))
                          index)
                 (incf times))))
           (next (current next)
             (and (< times 11)
                  (or (not (car next))
                      (aref (hash-node-%content current)
                            (cdr next))))))
      (descend root #'go-in #'next))))


(-> ascend-copy-hash-tree (list t t) hash-node)
(defun ascend-copy-hash-tree (path key element)
  (multiple-value-bind (missing rest) (iterate
                                        (for x on path)
                                        (for (last-one . (present . index)) = (car x))
                                        (while (eq t last-one))
                                        (collect (make-hash-node :%mask (dpb 1 (byte 1 index) 0)
                                                                 :%content (make-array (list 1)))
                                          into missing)
                                        (finally (return (values missing x))))
    (assure hash-node
      (ascend-copy rest
                   (reduce (lambda (prev next)
                             (progn (setf (aref (hash-node-%content next) 0)
                                          prev)
                                    next))
                           missing
                           :initial-value (list (list* key element)))
                   (lambda (child segment node)
                     (hash-node-insert-into-copy node
                                                 (lambda (x) (make-array (list x)))
                                                 (list (list* (cdr segment) child))))))))


(-> insert-into-has-tree (hash-node t) hash-node)
@export
(defun insert-into-hash-tree (root key value)
  (let ((hash (sxhash key)))
    (ascend-copy-hash-tree (descend-into-hash-tree root hash)
                           key
                           value)))


(-> find-in-hash-tree (hash-node t) t)
@export
(defun find-in-hash-tree (root key)
  (let ((hash (sxhash key)))
    (iterate
      (for i from 0)
      (for hash-part = (ldb (byte 6 i) hash))
      (for current initially root then (hash-node-access current hash-part))
      (when (listp current)
        (leave (cdr (find-if (lambda (x) (equal (car x) key))
                             current)))))))
