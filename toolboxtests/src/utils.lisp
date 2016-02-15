(in-package :toolboxtests)
(in-suite toolboxtests)


(test group-ordered-test
  (let* ((input #(1 2 3 8 9 11 12))
         (product (group-ordered-sequence (lambda (last new)
                                            (= 1
                                               (abs (- last
                                                       new))))
                                          input)))
    (is (= (length product)
           3))
    (is (vector= (first product)
                 #(1 2 3)))
    (is (vector= (second product)
                 #(8 9)))
    (is (vector= (third product)
                 #(11 12)))))
