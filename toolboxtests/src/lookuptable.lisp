(in-package :toolboxtests)
(in-suite toolboxtests)


(def-fixture lookuptable-init (container mask)
  (let ((table (make-instance 'fixed-lookuptable
                              :container container
                              :mask mask)))
    (&body)))


(test lookuptable-insert
  (let ((container (make-instance 'vector-container :content (make-array 0))))
    (setf (access-replacer container)
          (make-instance 'hash-vector-pool))
    (with-fixture lookuptable-init (container 0)
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
             2)))))


(test lookuptable-random-fill
  (let* ((container (make-instance 'vector-container :content (make-array 0)))
         (data (iterate
                 (for i from 0 below 32)
                 (collect (list* (gensym) i))))
         (random-data (shuffle data)))
    (setf (access-replacer container)
          (make-instance 'hash-vector-pool))
    (with-fixture lookuptable-init (container 0)
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
                symbol))))))
