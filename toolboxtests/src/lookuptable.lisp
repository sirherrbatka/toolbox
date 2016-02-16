(in-package :toolboxtests)
(in-suite toolboxtests)


(def-fixture lookuptable-init (container mask)
  (let ((table (make-instance 'fixed-lookuptable
                              :container container
                              :mask mask)))
    (&body)))


(test lookuptable-insert
  (let ((container (make-instance 'vector-container)))
    (with-fixture lookuptable-init (container 0)
      (with-vector-replacer (make-instance 'hash-vector-pool)
        (setf (access-content-of-lookuptable table 5)
              'a)
        (is (eq (access-content-of-lookuptable table 5)
                'a))
        (is (eq (access-content (read-container table)
                                0)
                'a))
        (is (= (read-mask table)
               #b100000))
        (setf (access-content-of-lookuptable table 0)
              'b)
        (is (eq (access-content-of-lookuptable table 0)
                'b))
        (is (eq (access-content (read-container table)
                                0)
                'b))
        (is (= (content-length (read-container table))
               2))
        (is (eq (access-content (read-container table)
                                1)
                'a))
        (is (= (read-mask table)
               #b100001))
        (setf (access-content-of-lookuptable table 5)
              'c)
        (is (= (read-mask table)
               #b100001))
        (is (eq (access-content-of-lookuptable table 5)
                'c))
        (is (= (content-length (read-container table))
               2))))))


(test lookuptable-random-fill
  (let* ((container (make-instance 'vector-container))
         (data (iterate
                 (for i from 0 below 32)
                 (collect (list* (gensym) i))))
         (random-data (shuffle data)))
    (with-fixture lookuptable-init (container 0)
      (with-vector-replacer (make-instance 'hash-vector-pool)
        (iterate
          (for i from 1 to 32)
          (for (symbol . index) in random-data)
          (setf (access-content-of-lookuptable table index)
                symbol)
          (is (= (elements-count table)
                 i)))
        (iterate
          (for (symbol . index) in data)
          (is (eq (access-content-of-lookuptable table index)
                  symbol)))))))


(test lookuptable-insert-one-into-copy
  (let ((container (make-instance 'vector-container))
        (factory (make-instance 'fixed-lookuptable-factory :replacer (make-instance 'hash-vector-pool)))
        (data (shuffle (iterate
                         (for i from 0 below 32)
                         (collect (list* i (gensym)))))))
    (with-lookuptable-factory factory
      (with-fixture lookuptable-init (container 0)
        (iterate
          (for elt in data)
          (for (index . item) = elt)
          (let ((copy (insert-into-copy table elt)))
            (setf (access-content-of-lookuptable table index)
                  item)
            (is (fixed-lookuptable= copy table))))))))


(test lookuptable-insert-two-at-once-into-copy
  (let ((container (make-instance 'vector-container))
        (factory (make-instance 'fixed-lookuptable-factory :replacer (make-instance 'hash-vector-pool)))
        (data (shuffle (iterate
                         (for i from 0 below 32)
                         (collect (list* i (gensym)))))))
    (with-lookuptable-factory factory
      (with-fixture lookuptable-init (container 0)
        (iterate
          (for i from 0 below 14)
          (for (index . item) in data)
          (setf (access-content-of-lookuptable table i)
                item))
        (setf (access-content-of-lookuptable table 16)
              :test)
        (iterate
          (for i from 18 below 30)
          (for (index . item) in data)
          (setf (access-content-of-lookuptable table i)
                item))
        (let ((copy (insert-into-copy table
                                      (list* 14 'a)
                                      (list* 15 'b)
                                      (list* 17 'c))))
          (setf (access-content-of-lookuptable table 14) 'a)
          (setf (access-content-of-lookuptable table 15) 'b)
          (setf (access-content-of-lookuptable table 17) 'c)
          (is (fixed-lookuptable= table copy))
          (return-lookuptable factory copy))))))


(test lookuptable-insert-random-two-at-once-into-copy
  (let ((container (make-instance 'vector-container))
        (factory (make-instance 'fixed-lookuptable-factory :replacer (make-instance 'hash-vector-pool)))
        (data (batches (shuffle (iterate
                                 (for i from 0 below 32)
                                  (collect (list* i (gensym)))))
                       2)))
    (with-lookuptable-factory factory
      (with-fixture lookuptable-init (container 0)
        (iterate
          (for batch in data)
          (for copy = (insert-into-copy table (first batch) (second batch)))
          (iterate (for (index . item) in batch)
            (setf (access-content-of-lookuptable table index)
                  item))
          (is (fixed-lookuptable= copy table))
          (return-lookuptable factory copy))))))


(test lookuptable-insert-random-four-at-once-into-copy
      (let ((container (make-instance 'vector-container))
            (factory (make-instance 'fixed-lookuptable-factory :replacer (make-instance 'hash-vector-pool)))
            (data (batches (shuffle (iterate
                                     (for i from 0 below 32)
                                     (collect (list* i (gensym)))))
                           4)))
        (with-lookuptable-factory factory
          (with-fixture lookuptable-init (container 0)
            (iterate
              (for batch in data)
              (for copy = (insert-into-copy table (first batch) (second batch) (third batch) (fourth batch)))
              (iterate (for (index . item) in batch)
                (setf (access-content-of-lookuptable table index)
                      item))
              (is (fixed-lookuptable= copy table))
              (return-lookuptable factory copy))))))
