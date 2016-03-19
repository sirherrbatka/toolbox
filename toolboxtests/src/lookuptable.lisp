(in-package :toolboxtests)
(in-suite toolboxtests)


(def-fixture typed-lookuptable ()
  (flet ((fn (num) (make-array (list num) :element-type 'symbol :initial-element nil)))
    (let ((lookuptable (make-test-lookuptable)))
      (&body))))


(lookuptable-template test-lookuptable
                      ((unsigned-byte 32) 0)
                      ((vector symbol) (make-array 0 :element-type 'symbol :initial-element nil)))


(test typed-lookuptable-insert-random-fill
  (with-fixture typed-lookuptable ()
    (let* ((data (iterate
                   (for i from 0 below 32)
                   (collect (list* (gensym) i))))
           (random-data (shuffle data)))
      (iterate
        (for i from 1 to 32)
        (for (symbol . index) in random-data)
          (setf lookuptable
                (with-test-lookuptable (lookuptable)
                  (insert-into-copy #'fn
                                    (vector (list* index symbol)))))
          (is (= (with-test-lookuptable (lookuptable) (elements-count)) i)))
        (with-test-lookuptable (lookuptable)
          (iterate
            (for (symbol . index) in data)
            (is (eq (access index)
                    symbol)))))))


(test typed-lookuptable-insert-two-at-once-into-copy
  (with-fixture typed-lookuptable ()
    (let ((data (shuffle (iterate
                           (for i from 0 below 32)
                           (collect (list* i (gensym)))))))
      (iterate
        (for i from 0 below 14)
        (for (index . item) in data)
        (with-test-lookuptable (lookuptable)
          (setf lookuptable (insert-into-copy #'fn (vector (list* index item))))))
      (iterate
        (for i from 18 below 30)
        (for (index . item) in data)
        (with-test-lookuptable (lookuptable)
          (setf lookuptable (insert-into-copy #'fn (vector (list* index item))))))
      (with-test-lookuptable (lookuptable)
        (let ((copy (insert-into-copy #'fn (vector (list* 14 'a)
                                                   (list* 15 'b)
                                                   (list* 17 'c)))))
          (with-test-lookuptable (copy)
            (is (eq (access 14) 'a))
            (is (eq (access 15) 'b))
            (is (eq (access 17) 'c))))))))


(test typed-lookuptable-insert-random-four-at-once-into-copy
  (with-fixture typed-lookuptable ()
    (let ((data (batches (shuffle (iterate
                                    (for i from 0 below 32)
                                    (collect (list* i (gensym)))))
                         4)))
      (iterate

        (for batch in data)
        (for copy = (with-test-lookuptable (lookuptable)
                      (insert-into-copy #'fn (vector (first batch) (second batch) (third batch) (fourth batch)))))
        (iterate (for (index . item) in batch)
                 (is (eq (with-test-lookuptable (copy)
                           (access index))
                         item)))
        (setf lookuptable copy))
      (iterate
        (for batch in data)
        (with-test-lookuptable (lookuptable)
          (iterate (for (index . data) in batch)
                   (is (eq (access index) data))))))))


(test lookuptable-signals-error-when-trying-to-access-non-existing-element
  (let ((factory (make-instance 'fixed-lookuptable-factory :replacer (make-instance 'hash-vector-pool)))
        (container (make-instance 'vector-container)))
    (with-lookuptable-factory factory
      (with-fixture lookuptable-init (container 0)
        (fiveam:signals lookuptable-does-not-contain-item
          (access-content-of-lookuptable table 0))))))


(test lookuptable-creation
  (let ((factory (make-instance 'fixed-lookuptable-factory :replacer (make-instance 'hash-vector-pool))))
    (with-lookuptable-factory factory
      (let ((table (make-lookuptable *lookuptable-factory* '((1 . t) (2 . t) (5 . t)))))
        (is (eq t (access-content-of-lookuptable table 1)))
        (is (eq t (access-content-of-lookuptable table 2)))
        (is (eq t (access-content-of-lookuptable table 5)))
        (is (= 3 (elements-count table)))))))
