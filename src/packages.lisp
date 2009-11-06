;;;; packages.lisp


(defpackage #:closure-template
  (:use #:cl))


(defpackage :closure-template.parser
  (:use #:cl #:iter)
  (:import-from #:wiki-parser #:define-mode #:remake-lexer))
