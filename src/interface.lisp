;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defgeneric compile-template (backend template)
  (:documentation "Compile templates from TEMPLATE using BACKEND as target language"))

(defgeneric register-print-handler (backend directive &rest options)
  (:documentation "Register handler for custom print directive in BACKEND.
Directive syntax is defined by DIRECTIVE. Actual handler parameters are depend on
backend type."))

(import 'closure-template.parser:define-print-syntax :closure-template)
