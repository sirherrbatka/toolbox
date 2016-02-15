(in-package #:cl-user)

(asdf:defsystem toolboxtests
  :name "toolboxtests"
  :version "0.0.0"
  :license "MIT"
  :author "Marek Kochanowicz (aka shka)"
  :maintainer "Marek Kochanowicz (aka shka)"
  :depends-on (:iterate :alexandria :serapeum :toolbox :fiveam)
  :serial T
  :components ((:file "package")
               (:file "src/vectors")
               (:file "src/utils")
               (:file "src/lookuptable")))
