;;;; defpackage.lisp
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
           ;; utils
           #:not-equal
           #:lispify-name
           #:lispify-string
           ;; expression
           #:ref-expr
           #:dotref
           #:dotref-name
           #:dotref-jsname
           #:arref
           #:arref-position
           #:var
           #:var-name
           #:var-jsname
           #:fcall
           #:fcall-name
           #:fcall-jsname
           #:fcall-args
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
	   #:*user-print-directives*
	   #:add-print-directive
	   #:define-rule))

(in-package #:closure-template.parser)

(defvar *closure-template-rules* (make-hash-table))

(defvar *user-print-directives* (make-hash-table))

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
                (when (upper-case-p ch)
                  (collect #\-))
                (collect (char-upcase ch)))
          'string))

(defun lispify-name (str)
  (intern (lispify-string str) :keyword))

(defun not-equal (obj1 obj2)
  (not (equal obj1 obj2)))

(define-rule whitespace (+ (or #\space #\tab #\newline))
  (:constant " "))
