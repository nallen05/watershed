
(defpackage :watershed-system
  (:use :cl))

(in-package :watershed-system)

(asdf:defsystem :watershed
  :depends-on (:trivial-utf-8)
  :components
  ((:module :bikesehd
            :components ((:file "watershed")))))