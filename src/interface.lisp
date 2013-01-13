;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defgeneric compile-template (backend template)
  (:documentation "Compile templates from TEMPLATE using BACKEND as target language"))

(defgeneric register-print-handler (backend-type symbol &rest args)
  (:documentation "Register handler for custom print directive SYMBOL in BACKEND.
Exact parameters in ARGS are defined in implementations."))

(import 'closure-template.parser:register-print-handler :closure-template)
