(in-package :toolbox)
(annot:enable-annot-syntax)


@export
(defun read-stream-char (not-in-word-fn &optional (input-stream *standard-input*))
  (let* ((next-char (read-char input-stream nil nil nil))
         (done (funcall not-in-word-fn next-char)))
    (values next-char done)))


@export
(defun read-stream-word (not-in-word-fn &optional (input-stream *standard-input*))
  (values (with-output-to-string (out)
            (loop for (char done) = (multiple-value-list (read-stream-char not-in-word-fn input-stream))
                  until done
                  do (format out "~a" char)))
          (peek-char t input-stream nil nil)))


@export
(defun read-stream-words (separators &optional (input-stream *standard-input*))
  (loop for (word next) = (multiple-value-list (read-stream-word (lambda (x) (or (not x) (find x separators :test #'char=)))
                                                                 input-stream))
        collect word into words
        while next
        finally (return words)))
