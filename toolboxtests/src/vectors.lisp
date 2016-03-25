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
