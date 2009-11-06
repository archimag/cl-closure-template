;;;; closure-template.asd
;;;; -*- mode: lisp -*-

(defpackage #:closure-template-system
  (:use #:cl #:asdf))

(in-package #:closure-template-system)


(defsystem closure-template
  :depends-on (#:wiki-parser #:yacc)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "parser" :depends-on ("packages"))))))