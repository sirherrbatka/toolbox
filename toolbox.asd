(in-package #:cl-user)

(asdf:defsystem toolbox
  :name "toolbox"
  :version "0.0.0"
  :license "MIT"
  :author "Marek Kochanowicz (aka shka)"
  :maintainer "Marek Kochanowicz (aka shka)"
  :depends-on (:iterate :alexandria :serapeum :cl-annot :optima :quid-pro-quo)
  :serial T
  :components ((:file "package")
               (:file "src/utils")
               (:file "src/types")
               (:file "src/bitwise")
               (:file "src/macros")
               (:file "src/streams")
               (:file "src/algorithms")
               (:file "src/vectors")
               (:file "src/typed-structs")
               (:file "src/lookuptable")
               (:file "src/functional")))
