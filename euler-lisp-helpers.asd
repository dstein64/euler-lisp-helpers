;;;
;;; euler-lisp-helpers system definition
;;;

(defpackage #:euler-lisp-helpers-asd
  (:use #:common-lisp #:asdf))

(in-package #:euler-lisp-helpers-asd)

(defsystem euler-lisp-helpers
  :author "Maintained by Daniel Steinberg"
  :components ((:file "packages")
	       (:file "euler-lisp-helpers" :depends-on ("packages"))))
