(in-package :cl-user)

(defpackage :cldbc-system
  (:use :cl :asdf))

(in-package :cldbc-system)

(defsystem :cldbc
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (:cl-mysql)
  :components ((:file "cldbc")))