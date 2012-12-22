;;;; packages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:closure-template
  (:use #:cl #:iter #:alexandria #:closure-template.parser)
  ;;(:import-from #:closure-template.parser #:parse-template  #:parse-expression #:not-equal #:lispify-string)
  (:export #:parse-template
           #:parse-expression
           #:translate-template
           #:*default-closure-template-package*
           #:*default-js-namespace*
           #:make-namespace-package
           #:escape-html
           #:encode-uri
           #:encode-uri-component
           #:decode-uri

           #:compile-template
           #:compile-cl-templates
           #:compile-js-templates

           ;; ttable
           #:*ttable*
           #:ttable
           #:ttable-clear
           #:ttable-find-template
           #:ttable-register-template
           #:ttable-call-template
           #:ttable-template-name-list
           #:ttable-clean-package
           #:ttable-extend-package
           #:ttable-sync-package
           #:ensure-ttable-package
           #:package-ttable
           #:fetch-keys
           #:fetch-property

           ;; ASDF extension
           #:closure-template))
