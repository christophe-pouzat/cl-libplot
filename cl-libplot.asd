(in-package #:cl-user)

(defpackage #:cl-libplot.asdf
  (:use #:cl
        #:asdf))

(in-package #:cl-libplot.asdf)

(defsystem #:cl-libplot
  :name "cl-libplot"
  :author "Christophe Pouzat <christophe.pouzat@gmail.com>"
  :version "0.1.0"
  :licence "GPL v3"
  :description "Interface to Gnu Libplot"
  :components ((:file "cl-libplot"))
  :depends-on (:cffi))
