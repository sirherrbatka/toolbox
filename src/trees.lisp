(in-package :toolbox)
(annot:enable-annot-syntax)

#|

K-tree, NOT balanced right now.

|#

@export
(defstruct k-node
  (coordinates #(0 0) :type simple-vector)
  (content nil :type list)
  (neighboors #(nil nil nil nil) :type simple-vector))


(-> get-segment (k-node simple-vector) index)
@export
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


(-> insert-into-k-tree (k-node simple-vector t) k-node)
@export
(defun insert-into-k-tree (root coordinates element)
  (let* ((path (assure list (do ((segment (get-segment root
                                                       coordinates)
                                          (get-segment current-node
                                                       coordinates))
                                 (prev-node nil current-node)
                                 (current-node root (and (not (zerop segment))
                                                         (aref (k-node-neighboors current-node)
                                                               (1- segment))))
                                 (path nil (cons (list* current-node
                                                        segment)
                                                 path)))
                                ((null current-node) path))))
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
                                                            :initial-element nil))))))
         (rest (assure list (if (zerop (cdar path))
                                (cdr path)
                                path))))

    (assure k-node (reduce (lambda (prev next)
                             (destructuring-bind (node . segment) next
                               (with-accessors ((content k-node-content)
                                                (coordinates k-node-coordinates)
                                                (neighboors k-node-neighboors)) node
                                 (make-k-node :coordinates coordinates
                                              :content content
                                              :neighboors (let ((content (copy-array neighboors)))
                                                            (setf (aref content (1- segment))
                                                                  prev)
                                                            content)))))
                           rest
                           :initial-value seed))))


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
