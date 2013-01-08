;;;; suite.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:closure-template.test
  (:use #:cl #:iter #:lift #:closure-template #:closure-template.parser)
  ;;(:import-from #:closure-template.parser #:s-expr)
  (:export #:run-closure-template-tests))

(in-package #:closure-template.test)

(deftestsuite closure-template-test () ())

(defun run-closure-template-tests (&optional (test 'closure-template-test))
  "Run all closure-template tests"
  (run-tests :suite test :report-pathname nil))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system 'closure-template-test))))
  (let* ((test-results (run-closure-template-tests))
         (errors (lift:errors test-results))
         (failures (lift:failures test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))


(defgeneric s-expr (obj)
  (:documentation "Convert object to s-expression")
  (:method (obj)
    obj)
  (:method ((obj list))
    (mapcar #'s-expr obj))
  (:method ((obj dotref))
    (list :dotref
          (s-expr (ref-expr obj))
          (s-expr (dotref-name obj))))
  (:method ((obj arref))
    (list :aref
          (s-expr (ref-expr obj))
          (s-expr (arref-position obj))))
  (:method ((obj injected-data))
    :injected-data)
  (:method ((obj var))
    (list :variable
          (var-name obj)))
  (:method ((obj fcall))
    (list ':fcall
          (s-expr (fcall-name obj))
          (mapcar #'s-expr (fcall-args obj))))
  (:method ((obj operator))
    (list* :operator
           (s-expr (op-name obj))
           (mapcar #'s-expr (op-args obj))))
  (:method ((obj list-expr))
    (list* :list
           (mapcar #'s-expr (list-expr-values obj))))
  (:method ((obj map-expr))
    (list* :map
           (iter (for item in (map-expr-items obj))
                 (collect
                     (list (s-expr (first item))
                           (s-expr (second item)))))))
  (:method ((obj comment))
    (list :comment
          (comment-text obj)
          (comment-multiline-p obj)))
  (:method ((obj literal))
    (list :literal
          (literal-content obj)))
  (:method ((obj print-command))
    (list* :print
           (s-expr (print-expression obj))
           (print-directives obj)))
  (:method ((obj with))
    (list :with
          (mapcar #'s-expr (with-vars obj))
          (mapcar #'s-expr (with-body obj))))
  (:method ((obj if-command))
    (cons :if
          (s-expr (if-command-options obj))))
  (:method ((obj switch-command))
    (s-expr (list* :switch
                   (switch-expression obj)
                   (switch-default obj)
                   (switch-cases obj))))
  (:method ((obj foreach))
    (s-expr (list :foreach
                  (list (foreach-varname obj)
                        (foreach-expression obj))
                  (foreach-code-block obj))))
  (:method ((obj for-command))
    (s-expr (list :for
                  (list (for-varname obj)
                        (for-range obj))
                  (for-code-block obj))))
  (:method ((obj call))
    (s-expr (list* :call
                   (call-name obj)
                   (call-data obj)
                   (call-params obj))))
  (:method ((obj template))
    (list* :template
           (list* (s-expr (template-name obj))
                  (s-expr (template-properties obj)))
           (mapcar #'s-expr (template-code-block obj))))
  )
