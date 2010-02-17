;;;; core-test.lisp
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite expression-parser-test (closure-template-test) ())

;;;; literal

(addtest (expression-parser-test)
  string-1
  (ensure-same "Hello world"
               (parse-expression "'Hello world'")))

(addtest (expression-parser-test)
  integer-1
  (ensure-same 5
               (parse-expression "5")))

(addtest (expression-parser-test)
  float-1
  (ensure-same 3.14
               (parse-expression "3.14")))

;;; vars

(addtest (expression-parser-test)
  var-1
  (ensure-same '(:variable :var)
               (parse-expression " $var ")))

(addtest (expression-parser-test)
  var-2
  (ensure-same '(getf (:variable :x) :y)
               (parse-expression "$x.y")))

(addtest (expression-parser-test)
  var-3
  (ensure-same '(getf (elt (:variable :x) 1) :y)
               (parse-expression "$x.1.y")))

(addtest (expression-parser-test)
  var-4
  (ensure-same '(getf (elt (:variable :x) 0) :y)
               (parse-expression "$x[0].y")))

(addtest (expression-parser-test)
  var-5
  (ensure-same '(getf (elt (:variable :x) (:variable :z)) :y)
               (parse-expression "$x[$z].y")))

(addtest (expression-parser-test)
  var-6
  (ensure-same '(elt (elt (:variable :x) 0) 1)
               (parse-expression "$x[0][1]")))

(addtest (expression-parser-test)
  var-7
  (ensure-same '(elt (elt (elt (:variable :x) 0) 1) (:variable :y))
               (parse-expression "$x[0][1][$y]")))

;;;; operators

(addtest (expression-parser-test)
  operator--unary
  (ensure-same '(- (:variable :x))
               (parse-expression "-$x")))

(addtest (expression-parser-test)
  operator-not
  (ensure-same '(not (:variable :x))
               (parse-expression "not $x")))

(addtest (expression-parser-test)
  operator-+-1
  (ensure-same '(+ (:variable :x) (:variable :y))
               (parse-expression " $x + $y ")))

(addtest (expression-parser-test)
  operator-+-2
  (ensure-same '(+ 2 2)
               (parse-expression "2 + 2")))

(addtest (expression-parser-test)
  operator--
  (ensure-same '(- (:variable :x) (:variable :y))
               (parse-expression " $x - $y ")))

(addtest (expression-parser-test)
  operator-*
  (ensure-same '(* (:variable :x) (:variable :y))
               (parse-expression " $x * $y ")))

(addtest (expression-parser-test)
  operator-/
  (ensure-same '(/ (:variable :x) (:variable :y))
               (parse-expression " $x/$y ")))

(addtest (expression-parser-test)
  operator-%
  (ensure-same '(rem (:variable :x) (:variable :y))
               (parse-expression " $x % $y ")))

(addtest (expression-parser-test)
  operator->
  (ensure-same '(> (:variable :x) (:variable :y))
               (parse-expression " $x > $y ")))

(addtest (expression-parser-test)
  operator-<
  (ensure-same '(< (:variable :x) (:variable :y))
               (parse-expression " $x < $y ")))

(addtest (expression-parser-test)
  operator->=
  (ensure-same '(>= (:variable :x) (:variable :y))
               (parse-expression " $x >= $y ")))

(addtest (expression-parser-test)
  operator-<=
  (ensure-same '(<= (:variable :x) (:variable :y))
               (parse-expression " $x <= $y ")))

(addtest (expression-parser-test)
  operator-==
  (ensure-same '(equal (:variable :x) (:variable :y))
               (parse-expression " $x == $y ")))

(addtest (expression-parser-test)
  operator-!=
  (ensure-same '(closure-template.parser.expression:not-equal (:variable :x) (:variable :y))
               (parse-expression " $x != $y ")))

(addtest (expression-parser-test)
  operator-and
  (ensure-same '(and (:variable :x) (:variable :y))
               (parse-expression " $x and $y ")))

(addtest (expression-parser-test)
  operator-or
  (ensure-same '(or (:variable :x) (:variable :y))
               (parse-expression " $x or $y ")))

(addtest (expression-parser-test)
  |operator-?:-ternary|
  (ensure-same '(:max 2
                 (if (:variable :x)
                     (:min (:variable :x)
                           (if (:variable :y)
                               3
                               (+ 5 4))
                           6)
                     4))
               (parse-expression "max(2, $x ? min($x, $y ? 3 : 5 + 4, 6) : 4)")))

;;;; functions

(addtest (expression-parser-test)
  ;; very important!!!
  function-1
  (ensure-same '(:has-data nil)
               (parse-expression "hasData()")))

(addtest (expression-parser-test)
  function-2
  (ensure-same '(:min (:variable :x) (:variable :y))
               (parse-expression "min($x, $y)")))

(addtest (expression-parser-test)
  function-3
  (ensure-same '(:min (:variable :x) (:max 5 (:variable :y)))
               (parse-expression "min($x, max(5, $y))")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-single-template (obj)
  (third (parse-template obj)))

(deftestsuite template-parser-test (closure-template-test) ())

;;;; template with paramas

(addtest (template-parser-test)
  template-1
  (ensure-same '(closure-template.parser:template ("test" :autoescape t))
               (parse-single-template "{template test autoescape=\"true\"}{/template}")))

(addtest (template-parser-test)
  template-2
  (ensure-same '(closure-template.parser:template ("test" :private nil))
               (parse-single-template "{template test private=\"false\"}{/template}")))

(addtest (template-parser-test)
  template-3
  (ensure-same '(closure-template.parser:template ("test" :autoescape nil :private t))
               (parse-single-template "{template test autoescape=\"false\" private=\"true\"}{/template}")))

(addtest (template-parser-test)
  template-4
  (ensure-same '(closure-template.parser:template ("test") "Hello")
               (parse-single-template "{template test}
    Hello
{/template}")))

;;;; substitions

(addtest (template-parser-test)
  substition-1
  (ensure-same '(closure-template.parser:template ("substitions")
                 closure-template.parser:space-tag
                 closure-template.parser:emptry-string
                 closure-template.parser:carriage-return
                 closure-template.parser:line-feed
                 closure-template.parser:tab
                 closure-template.parser:left-brace
                 closure-template.parser:right-brace)
               (parse-single-template "{template substitions}{sp}{nil}{\\r}{\\n}{\\t}{lb}{rb}{/template}")))

;;;; print

(addtest (template-parser-test)
  print-1
  (ensure-same '(closure-template.parser:template ("helloName") "Hello "
                 (closure-template.parser:print-tag (:VARIABLE :name)))
               (parse-single-template "{template helloName}Hello {$name}{/template}")))

(addtest (template-parser-test)
  print-2
  (ensure-same '(closure-template.parser:template ("test") 
                 (closure-template.parser:print-tag (+ 2 2)))
               (parse-single-template "{template test}{2 + 2}{/template}")))

(addtest (template-parser-test)
  print-3
  (ensure-same '(closure-template.parser:template ("test") 
                 (closure-template.parser:print-tag (+ 2 2)
                  :no-autoescape t))
               (parse-single-template "{template test}{2 + 2 |noAutoescape}{/template}")))

(addtest (template-parser-test)
  print-4
  (ensure-same '(closure-template.parser:template ("test") 
                 (closure-template.parser:print-tag (+ 2 2)
                  :id t))
               (parse-single-template "{template test}{2 + 2 |id}{/template}")))

(addtest (template-parser-test)
  print-5
  (ensure-same '(closure-template.parser:template ("test") 
                 (closure-template.parser:print-tag (+ 2 2)
                  :no-autoescape t
                  :id t
                  :escape-html t
                  :escape-uri t
                  :escape-js t
                  :insert-word-breaks 5))
               (parse-single-template "{template test}{2 + 2 |noAutoescape |id |escapeHtml |escapeUri |escapeJs  |insertWordBreaks:5}{/template}")))

;;;; literal

(addtest (template-parser-test)
  literal-1
  (ensure-same '(closure-template.parser:template ("literal-test") 
                 (closure-template.parser:literal "Test {$x} {foreach $foo in $bar}{$foo}{/foreach}"))
               (parse-single-template "{template literal-test}{literal}Test {$x} {foreach $foo in $bar}{$foo}{/foreach}{/literal}{/template}")))

;;;; if

(addtest (template-parser-test)
  if-1
  (ensure-same '(closure-template.parser:template ("if-test")
                 (closure-template.parser:if-tag
                  ((:variable :x) ("Hello "
                                    (closure-template.parser:print-tag (:variable :x))))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{/if}{/template}")))

(addtest (template-parser-test)
  if-2
  (ensure-same '(closure-template.parser:template ("if-test")
                 (closure-template.parser:if-tag
                  ((:variable :x) ("Hello " (closure-template.parser:print-tag (:variable :x))))
                  (t ("Hello world"))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{else}Hello world{/if}{/template}")))

(addtest (template-parser-test)
  if-3
  (ensure-same '(closure-template.parser:template ("if-test")
                 (closure-template.parser:if-tag
                  ((:variable :x) ("Hello " (closure-template.parser:print-tag (:variable :x))))
                  ((:variable :y) ("Hello " (closure-template.parser:print-tag (:variable :y))))
                  (t ("Hello world"))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{elseif $y}Hello {$y}{else}Hello world{/if}{/template}")))

(addtest (template-parser-test)
  if-4
  (ensure-same '(closure-template.parser:template ("if-test")
                 (closure-template.parser:if-tag
                  ((:variable :x) ("Hello " (closure-template.parser:print-tag (:variable :x))))
                  ((:variable :y) ("Hello " (closure-template.parser:print-tag (:variable :y))))
                  ((:variable :z) ("By!"))
                  (t ("Hello world"))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{elseif $y}Hello {$y}{elseif $z}By!{else}Hello world{/if}{/template}")))

;;;; switch

(addtest (template-parser-test)
  switch-1
  (ensure-same '(closure-template.parser:template ("switch-test")
                 (closure-template.parser:switch-tag (:variable :x)
                  nil
                  ((1) ("hello world"))
                  ((2 3 4) ("by-by"))))
               (parse-single-template "{template switch-test}{switch $x}{case 1}hello world{case 2, 3, 4}by-by{/switch}{/template}")))

(addtest (template-parser-test)
  switch-2
  (ensure-same '(closure-template.parser:template ("switch-test")
                 (closure-template.parser:switch-tag (:variable :x)
                  ("default value")
                  ((1) ("hello world"))
                  ((2 3 4) ("by-by"))))
               (parse-single-template "{template switch-test}{switch $x}{case 1}hello world{case 2, 3, 4}by-by{default}default value{/switch}{/template}")))

;;;; foreach

(addtest (template-parser-test)
  foreach-1
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:foreach ((:variable :x) (getf (:variable :y) :foo))
                  ((closure-template.parser:print-tag (:variable :x)))))
               (parse-single-template "{template test}{foreach $x in $y.foo }{$x}{/foreach}{/template}")))

;;;; for

(addtest (template-parser-test)
  for-1
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:for-tag ((:variable :x) (:range 10)) " ! "))
               (parse-single-template "{template test}{for $x in range(10)} ! {/for}{/template}")))

(addtest (template-parser-test)
  for-2
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:for-tag ((:variable :x) (:range 4 10)) " ! "))
               (parse-single-template "{template test}{for $x in range(4, 10)} ! {/for}{/template}")))

(addtest (template-parser-test)
  for-3
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:for-tag ((:variable :x) (:range 4 10 2)) " ! "))
               (parse-single-template "{template test}{for $x in range(4, 10, 2)} ! {/for}{/template}")))


;;;; call

(addtest (template-parser-test)
  call-1
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:call "hello-name" (:variable :x)))
               (parse-single-template "{template test}{call hello-name data=\"$x\" /}{/template}")))

(addtest (template-parser-test)
  call-2
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:call "hello-name" nil
                  (closure-template.parser:param (:variable :name) (:variable :x))))
               (parse-single-template "{template test}{call hello-name}{param name: $x /}{/call}{/template}")))


(addtest (template-parser-test)
  call-3
  (ensure-same '(closure-template.parser:template ("test")
                 (closure-template.parser:call "hello-name" (:variable :data)
                  (closure-template.parser:param (:variable :a) (:variable :x))
                  (closure-template.parser:param (:variable :b) nil
                   "Hello " (closure-template.parser:print-tag (:variable :y)))))
               (parse-single-template "{template test}{call hello-name data=\"$data\"}{param a: $x /}{param b}Hello {$y}{/param} {/call}{/template}")))

