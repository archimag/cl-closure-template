;;;; packages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


;; (defpackage #:closure-template.parser.expression
;;   (:use #:cl #:iter #:esrap)
;;   (:export #:parse-expression
;;            #:bad-expression-condition
;;            #:not-equal
;;            #:lispify-string
;;            #:lispify-name
;;            #:decimal-integer
;;            #:variable
;;            #:expression
;;            #:define-rule
;;            #:with-closure-template-rules
;;            #:closure-template-parse))

(defpackage #:closure-template.parser
  (:use #:cl #:iter #:esrap)
  (:export #:parse-template
           #:parse-expression
           #:closure-template-parse

           #:not-equal
           #:lispify-name
           #:lispify-string

           #:template-name
           
           #:foreach
           #:call
           #:param
           #:literal
           #:for-tag
           #:switch-tag
           #:namespace
           #:with
           #:if-tag
           #:template
           #:print-tag
           #:comment))

(defpackage #:closure-template
  (:use #:cl #:iter)
  (:import-from #:closure-template.parser #:parse-template  #:parse-expression #:not-equal #:lispify-string)
  (:export #:parse-template
           #:parse-expression
           #:translate-template
           #:compile-template
           #:*default-translate-package*
           #:*default-js-namespace*
           #:make-template-package
           #:escape-html
           #:encode-uri
           #:encode-uri-component
           #:decode-uri))
