(defpackage :toolbox
  (:use :common-lisp :iterate :alexandria :serapeum
        :cl-annot :annot.std :annot.eval-when :annot.slot
        :quid-pro-quo)
  (:shadowing-import-from :iterate :collecting :summing :in))
