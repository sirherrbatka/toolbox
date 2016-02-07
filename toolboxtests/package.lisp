(defpackage :toolboxtests
  (:use :common-lisp :iterate :alexandria :serapeum :fiveam :toolbox)
  (:export :run!)
  (:shadowing-import-from :iterate :collecting :summing :in))

(in-package :toolboxtests)

(import-all-internal-package-symbols :toolbox :toolboxtests)

(def-suite toolboxtests :description "test for toolbox")
