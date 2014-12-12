;;;; defpackage.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:closure-template.parser
  (:use #:cl #:iter #:esrap)
  (:export #:*uglify*
           #:parse-template
           #:parse-expression
           #:closure-template-parse
           ;; utils
           #:not-equal
           #:lispify-name
           #:lispify-string
           ;; expression
           #:add-possible-function
           #:ref-expr
           #:dotref
           #:dotref-name
           #:dotref-jsname
           #:arref
           #:arref-position
           #:injected-data
           #:var
           #:var-name
           #:var-jsname
           #:fcall
           #:fcall-name
           #:fcall-jsname
           #:fcall-args
           #:list-expr
           #:list-expr-values
           #:map-expr
           #:map-expr-items
           #:operator
           #:op-name
           #:op-args
           ;; commands
           #:comment
           #:comment-text
           #:comment-multiline-p
           #:literal
           #:literal-content
           #:print-command
           #:print-expression
           #:print-directives
           #:with
           #:with-vars
           #:with-body
           #:if-command
           #:if-command-options
           #:switch-command
           #:switch-expression
           #:switch-cases
           #:switch-default
           #:foreach
           #:foreach-varname
           #:foreach-expression
           #:foreach-code-block
           #:foreach-if-empty-code
           #:for-command
           #:for-varname
           #:for-range
           #:for-code-block
           #:call
           #:call-name
           #:call-namespace
           #:call-data
           #:call-params
           ;; template
           #:template
           #:template-name
           #:template-properties
           #:template-code-block
           ;; namespace
           #:namespace
           #:namespace-name
           #:namespace-templates
	   ;; user print directives
           #:define-print-syntax
           #:user-print-directive-p))

(in-package #:closure-template.parser)

(defvar *closure-template-rules* (make-hash-table))

(defmacro with-closure-template-rules (&body body)
  `(let ((esrap::*rules* *closure-template-rules*))
     ,@body))

(defun closure-template-parse (symbol text)
  (with-closure-template-rules
    (esrap:parse symbol text)))

(defmacro define-rule (symbol expression &body options)
  `(with-closure-template-rules
     (defrule ,symbol ,expression ,@options)))

(defun lispify-string (str)
  (coerce (iter (for ch in-string str)
                (for prev-char previous ch)
                (when (and (upper-case-p ch) prev-char (or (digit-char-p prev-char)
                                                           (lower-case-p prev-char)))
                  (collect #\-))
                (collect (char-upcase ch)))
          'string))

(defun lispify-name (str)
  (intern (lispify-string str) :keyword))

(defun not-equal (obj1 obj2)
  (not (equal obj1 obj2)))

(defparameter *uglify* t)

(define-rule whitespace (+ (or #\space #\tab #\newline))
  (:destructure (&rest wsp)
    (if *uglify*
        " "
        (apply #'concatenate 'string wsp))))
