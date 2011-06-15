;;;; packages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

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
  (:use #:cl #:iter #:alexandria)
  (:import-from #:closure-template.parser #:parse-template  #:parse-expression #:not-equal #:lispify-string)
  (:export #:parse-template
           #:parse-expression
           #:translate-template
           #:compile-template
           #:*default-closure-template-package*
           #:*default-js-namespace*
           #:make-namespace-package
           #:escape-html
           #:encode-uri
           #:encode-uri-component
           #:decode-uri

           ;; ttable
           #:*ttable*
           #:ttable
           #:ttable-find-template
           #:ttable-register-template
           #:ttable-call-template
           #:ttable-template-name-list
           #:ttable-clean-package
           #:ttable-extend-package
           #:ttable-sync-package
           #:ensure-ttable-package
           #:package-ttable))
