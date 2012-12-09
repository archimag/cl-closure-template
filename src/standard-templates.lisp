;;;; standard-templates.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(compile-template :common-lisp-backend
                  (merge-pathnames "standard-templates.tmpl"
                                   (asdf:component-pathname (asdf:find-system '#:closure-template))))
