;;;; test.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:closure-template.test
  (:use #:cl #:iter #:lift)
  (:import-from #:closure-template.parser #:parse-expression #:parse-template #:parse-single-template)
  (:export #:run-closure-template-tests))

(in-package #:closure-template.test)


(deftestsuite closure-template-test () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite expression-parser-test (closure-template-test) ())

(addtest (expression-parser-test)
  expression-1
  (ensure-same '(:variable "var")
               (parse-expression " $var ")))

(addtest (expression-parser-test)
  expression-2
  (ensure-same '(+ (:variable "x") (:variable "y"))
               (parse-expression " $x + $y ")))

(addtest (expression-parser-test)
  expression-3
  (ensure-same '(:min (:variable "x") (:variable "y"))
               (parse-expression "min($x, $y)")))

(addtest (expression-parser-test)
  expression-4
  (ensure-same '(:min (:variable "x") (:max 5 (:variable "y")))
               (parse-expression "min($x, max(5, $y))")))


(addtest (expression-parser-test)
  expression-5
  (ensure-same "Hello world"
               (parse-expression "'Hello world'")))

(addtest (expression-parser-test)
  expression-6
  (ensure-same '(:variable "x" "y")
               (parse-expression "$x.y")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite template-parser-test (closure-template-test) ())

(addtest (template-parser-test)
  hello-name-template-1
  (ensure-same '(closure-template.parser:template ("helloName") "Hello "
                 (closure-template.parser:print-tag (:VARIABLE "name")))
               (parse-single-template "{template helloName}Hello {$name}{/template}")))

(addtest (template-parser-test)
  literal-1
  (ensure-same '(closure-template.parser:template ("literal-test") 
                 (closure-template.parser:literal "Test {$x} {foreach $foo in $bar}{$foo}{/foreach}"))
               (parse-single-template "{template literal-test}{literal}Test {$x} {foreach $foo in $bar}{$foo}{/foreach}{/literal}{/template}")))

(addtest (template-parser-test)
  foreach-1
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:foreach ((:variable "x") (:variable "y" "foo"))
                  ((closure-template.parser:print-tag (:variable "x")))))
               (parse-single-template "{template test}{foreach $x in $y.foo }{$x}{/foreach}{/template}")))

(addtest (template-parser-test)
  for-1
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:for-tag ((:variable "x") (:range 10)) " ! "))
               (parse-single-template "{template test}{for $x in range(10)} ! {/for}{/template}")))

(addtest (template-parser-test)
  for-2
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:for-tag ((:variable "x") (:range 4 10)) " ! "))
               (parse-single-template "{template test}{for $x in range(4, 10)} ! {/for}{/template}")))

(addtest (template-parser-test)
  for-3
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:for-tag ((:variable "x") (:range 4 10 2)) " ! "))
               (parse-single-template "{template test}{for $x in range(4, 10, 2)} ! {/for}{/template}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run all tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-closure-template-tests (&optional (test 'closure-template-test))
  "Run all closure-template tests"
  (run-tests :suite test :report-pathname nil))









