(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defun compose-sequence (&rest functions)
  (lambda (&rest arguments)
    (reduce (lambda (prev current) (funcall current prev))
            functions
            :initial-value arguments)))
