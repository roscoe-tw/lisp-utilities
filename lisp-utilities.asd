;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; tangled :lisp-utilities.org

(in-package :cl-user)

(in-package #:asdf-user)

(defsystem #:lisp-utilities
  :version "0.1.0"
  :author "Colin <every.push.colin@gmail.com>"
  ;; :depends-on (:cl-ppcre :asdf)
  :components ((:file "packages")
               (:file "lisp-utilities")))
