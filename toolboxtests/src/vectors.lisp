(in-package :toolboxtests)
(in-suite toolboxtests)


(test copy-with-mask-test
      (let ((source-array #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
        (let ((destination (make-array 3)))
          (is (vector= #(3 4 5) (copy-with-mask destination source-array
                                  (:from 2 :to 5)))))
        (let ((destination (make-array 5)))
          (is (vector= #(1 2 3 4 5) (copy-with-mask destination source-array
                                      (:from 0 :to 5)))))
        (let ((destination (make-array 5)))
          (is (vector= #(1 3 5 7 9) (copy-with-mask destination source-array
                                       (:from 0 :to 10 :skip 'evenp)))))
        (let ((destination (make-array 10)))
          (is (vector= #(1 0 3 0 5 0 7 0 9 0) (copy-with-mask destination source-array
                                                (:from 0 :to 10 :skip 'evenp :into-shift 2)))))
        (let ((destination (make-array 10)))
          (is (vector= #(1 3 5 7 9 11 13 15 0 0) (copy-with-mask destination source-array
                                                (:from-shift 2)))))
        (let ((destination (make-array 7)))
          (is (vector= #(1 2 3 4 5 0 0) (copy-with-mask destination source-array
                                                (:from 0 :times 5)))))
        (let ((destination (make-array 9)))
          (with-vector ((a l v) destination)
            (iterate
              (for i index-of-vector v)
              (setf (a i) (1+ i))))
          (is (vector= #(1 3 3 5 5 7 7 9 9) (copy-with-mask destination source-array
                                              (:from 0 :to 10 :overwrite 'evenp :skip 'evenp)))))))


(test vector-container-resize
  (let ((pool (make-instance 'hash-vector-pool)))
    (let ((container (make-instance 'vector-container)))
      (resize-content container pool 1 (list (list 0 1 1)
                                             (list 2 0 0)))
      (is (= 1 (hash-table-count (access-buffers pool))))
      (is (= 1 (array-dimension (slot-value container '%content)
                                0)))
      (resize-content container pool 4 nil)
      (is (= 4 (array-dimension (slot-value container '%content)
                                0)))
      (is (= 2 (hash-table-count (access-buffers pool))))
      (is (vector= #(0 0 0 0)
                   (slot-value container '%content)))
      (setf (access-replacer container) pool)
      (setf (access-content container 3) 'a)
      (is (= 2 (hash-table-count (access-buffers pool))))
      (is (eq (access-content container 3)
              'a))
      (setf (access-content container 8) 'b)
      (is (eq (access-content container 8)
              'b))
      (is (eq (access-content container 3)
              'a))
      (is (= 3 (hash-table-count (access-buffers pool))))
      (setf (access-content container 2) 'c)
      (is (eq (access-content container 2) 'c))
      (is (= 3 (hash-table-count (access-buffers pool))))
      (setf (access-content container 15) 'd)
      (is (eq (access-content container 3)
              'a))
      (is (eq (access-content container 8)
              'b))
      (is (eq (access-content container 15)
              'd))
      (is (eq (access-content container 2)
              'c))
      (is (= 4 (hash-table-count (access-buffers pool)))))))
