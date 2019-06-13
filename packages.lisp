
;;; tangled : lisp-utilities.org

(in-package :cl-user)

(defpackage :lisp-utilities
  (:use :common-lisp)
  (:export :last1
           :single
           :append1
           :conc1
           :mklist
           :filter
           :group
           :flatten
           :prune
           :find2
           :before
           :after
           :duplicate
           :split-if
           :most
           :best
           :mostn
           :mapa-b
           :map0-1
           :map0-n
           :map1-n
           :map->
           :mappend
           :mapacars
           :rmapcar
           :readlist
           :prompt
           :break-loop
           :mkstr
           :symb
           :reread
           :explode))
