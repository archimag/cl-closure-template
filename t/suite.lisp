;;;; suite.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:closure-template.test
  (:use #:cl #:iter #:lift #:closure-template)
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
