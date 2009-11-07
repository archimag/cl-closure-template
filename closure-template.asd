;;;; closure-template.asd
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:closure-template-system
  (:use #:cl #:asdf))

(in-package #:closure-template-system)


(defsystem closure-template
  :depends-on (#:wiki-parser #:yacc)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "expression" :depends-on ("packages"))
                                     (:file "parser" :depends-on ("expression"))
                                     (:file "cl-backend" :depends-on ("parser"))))))