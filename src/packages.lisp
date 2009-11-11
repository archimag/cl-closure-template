;;;; packages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:closure-template.parser.expression
  (:use #:cl)
  (:export #:parse-expression
           #:bad-expression-condition))

(defpackage #:closure-template.parser
  (:use #:cl #:iter #:closure-template.parser.expression)
  (:import-from #:wiki-parser #:define-mode #:remake-lexer)
  (:export #:parse-template #:parse-single-template))


(defpackage #:closure-template
  (:use #:cl #:iter)
  (:import-from #:closure-template.parser #:parse-template #:parse-single-template)
  (:import-from #:closure-template.parser.expression #:parse-expression)
  (:export #:parse-template
           #:parse-single-template
           #:parse-expression
           #:translate-template
           #:compile-template
           #:*default-translate-package*
           #:make-template-package))
