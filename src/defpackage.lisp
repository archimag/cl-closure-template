;;;; packages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:closure-template
  (:use #:cl #:iter #:alexandria #:closure-template.parser)
  ;;(:import-from #:closure-template.parser #:parse-template  #:parse-expression #:not-equal #:lispify-string)
  (:import-from #:closure-template.parser #:*uglify*)
  (:export #:*uglify*
           #:parse-template
           #:parse-expression

           ;; compile
           #:compile-template
           #:compile-js-templates
           #:compile-cl-templates
           #:*default-closure-template-package*
           #:*injected-data*

           ;; ttable
           #:*ttable*
           #:ttable
           #:ttable-find-template
           #:ttable-register-template
           #:ttable-call-template
           #:ttable-extend-package
           #:ensure-ttable-package
           #:package-ttable
           #:fetch-keys
           #:fetch-property

           ;; ASDF extension
           #:closure-template

           ;; Custom print directives
           #:define-print-syntax
           #:register-print-handler

           ;; misc
           #:escape-html
           #:encode-uri
           #:encode-uri-component
           #:decode-uri
           #:lispify-string
           #:lispify-name
           #:with-user-functions))
