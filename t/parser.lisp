;;;; parser.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; escape funtions tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite escape-test (closure-template-test) ())

(addtest (escape-test)
  encode-uri-1
  (ensure-same "~!@#$%25%5E&*()%7B%7D%5B%5D=:/,;?+'%22%5C"
               (encode-uri "~!@#$%^&*(){}[]=:/,;?+\'\"\\")))

(addtest (escape-test)
  encode-uri-component-1
  (ensure-same "~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B'%22%5C"
               (encode-uri-component "~!@#$%^&*(){}[]=:/,;?+\'\"\\")))

(addtest (escape-test)
  decode-url-1
  (ensure-same "~!@#$%^&*(){}[]=:/,;?+\'\"\\"
               (decode-uri "~!@#$%25%5E&*()%7B%7D%5B%5D=:/,;?+'%22%5C")))

(addtest (escape-test)
  decode-url-2
  (ensure-same "~!@#$%^&*(){}[]=:/,;?+\'\"\\"
               (decode-uri "~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B'%22%5C")))


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
               (s-expr (parse-expression " $var "))))

(addtest (expression-parser-test)
  var-2
  (ensure-same '(:dotref (:variable :x) :y)
               (s-expr (parse-expression "$x.y"))))

(addtest (expression-parser-test)
  var-3
  (ensure-same '(:dotref (:aref (:variable :x) 1) :y)
               (s-expr (parse-expression "$x.1.y"))))

(addtest (expression-parser-test)
  var-4
  (ensure-same '(:dotref (:aref (:variable :x) 0) :y)
               (s-expr (parse-expression "$x[0].y"))))

(addtest (expression-parser-test)
  var-5
  (ensure-same '(:dotref (:aref (:variable :x) (:variable :z)) :y)
               (s-expr (parse-expression "$x[$z].y"))))

(addtest (expression-parser-test)
  var-6
  (ensure-same '(:aref (:aref (:variable :x) 0) 1)
               (s-expr (parse-expression "$x[0][1]"))))

(addtest (expression-parser-test)
  var-7
  (ensure-same '(:aref (:aref (:aref (:variable :x) 0) 1) (:variable :y))
               (s-expr (parse-expression "$x[0][1][$y]"))))

(addtest (expression-parser-test)
  var-8
  (ensure-same '(:dotref :injected-data :y)
               (s-expr (parse-expression "$ij.y"))))

(addtest (expression-parser-test)
  var-9
  (ensure-same '(:aref (:dotref :injected-data :y) 0)
               (s-expr (parse-expression "$ij.y[0]"))))

;;; lists

(addtest (expression-parser-test)
  list-1
  (ensure-same '(:list 1 2 3)
               (s-expr (parse-expression "[1, 2, 3]"))))

(addtest (expression-parser-test)
  list-2
  (ensure-same '(:list (:variable :x) "hello")
               (s-expr (parse-expression "[$x, 'hello']"))))

(addtest (expression-parser-test)
  list-3
  (ensure-same '(:list)
               (s-expr (parse-expression "[]"))))

(addtest (expression-parser-test)
  list-4
  (ensure-same '(:list 1 (:list 2 (:list 3 4)))
               (s-expr (parse-expression "[1, [2, [3, 4]]]"))))

;;; map

(addtest (expression-parser-test)
  map-1
  (ensure-same '(:map ("a" 1) ("b" 2) ("c" 3))
               (s-expr (parse-expression "{'a': 1, 'b':2,'c' : 3}"))))

(addtest (expression-parser-test)
  map-2
  (ensure-same '(:map ((:variable :x) (:variable :y)) ("hello" "world"))
               (s-expr (parse-expression "{ $x : $y, 'hello': 'world'}"))))

(addtest (expression-parser-test)
  map-3
  (ensure-same '(:map)
               (s-expr (parse-expression "{:}"))))

(addtest (expression-parser-test)
  map-4
  (ensure-same '(:map ("a" (:map ("b" (:map ("c" 1))))))
               (s-expr (parse-expression "{'a': {'b': {'c': 1}}}"))))

;; ;;;; operators

(addtest (expression-parser-test)
  operator--unary
  (ensure-same '(:operator - (:variable :x))
               (s-expr (parse-expression "-$x"))))

(addtest (expression-parser-test)
  operator-not
  (ensure-same '(:operator not (:variable :x))
               (s-expr (parse-expression "not $x"))))

(addtest (expression-parser-test)
  operator-+-1
  (ensure-same '(:operator + (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x + $y "))))

(addtest (expression-parser-test)
  operator-+-2
  (ensure-same '(:operator + 2 2)
               (s-expr (parse-expression "2 + 2"))))

(addtest (expression-parser-test)
  operator--
  (ensure-same '(:operator - (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x - $y "))))

(addtest (expression-parser-test)
  operator-*
  (ensure-same '(:operator * (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x * $y "))))

(addtest (expression-parser-test)
  operator-/
  (ensure-same '(:operator / (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x/$y "))))

(addtest (expression-parser-test)
  operator-%
  (ensure-same '(:operator rem (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x % $y "))))

(addtest (expression-parser-test)
  operator->
  (ensure-same '(:operator > (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x > $y "))))

(addtest (expression-parser-test)
  operator-<
  (ensure-same '(:operator < (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x < $y "))))

(addtest (expression-parser-test)
  operator->=
  (ensure-same '(:operator >= (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x >= $y "))))

(addtest (expression-parser-test)
  operator-<=
  (ensure-same '(:operator <= (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x <= $y "))))

(addtest (expression-parser-test)
  operator-==
  (ensure-same '(:operator equal (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x == $y "))))

(addtest (expression-parser-test)
  operator-!=
  (ensure-same '(:operator closure-template.parser:not-equal (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x != $y "))))

(addtest (expression-parser-test)
  operator-and
  (ensure-same '(:operator and (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x and $y "))))

(addtest (expression-parser-test)
  operator-or
  (ensure-same '(:operator or (:variable :x) (:variable :y))
               (s-expr (parse-expression " $x or $y "))))

(addtest (expression-parser-test)
  |operator-?:-ternary|
  (ensure-same '(:fcall :max
                 (2
                  (:operator if (:variable :x)
                   (:fcall :min
                           ((:variable :x)
                            (:operator if (:variable :y)
                             3
                             (:operator + 5 4))
                            6))
                  4)))
               (s-expr (parse-expression "max(2, $x ? min($x, $y ? 3 : 5 + 4, 6) : 4)"))))

(addtest (expression-parser-test)
  operator-precedence-1
  (ensure-same '(:operator + 1 (:operator * 2 3))
               (s-expr (parse-expression "1 + 2 * 3"))))

;; ;;;; functions

(addtest (expression-parser-test)
  ;; very important!!!
  function-1
  (ensure-same '(:fcall :has-data nil)
               (s-expr (parse-expression "hasData()"))))

(addtest (expression-parser-test)
  function-2
  (ensure-same '(:fcall :min ((:variable :x) (:variable :y)))
               (s-expr (parse-expression "min($x, $y)"))))

(addtest (expression-parser-test)
  function-3
  (ensure-same '(:fcall :min ((:variable :x) (:fcall :max (5 (:variable :y)))))
               (s-expr (parse-expression "min($x, max(5, $y))"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-single-template (obj)
  (s-expr (car (namespace-templates (parse-template obj)))))

(deftestsuite template-parser-test (closure-template-test) ())

;; ;;;; template with paramas

(addtest (template-parser-test)
  template-1
  (ensure-same '(:template ("test" :autoescape t))
               (parse-single-template "{template test autoescape=\"true\"}{/template}")))

(addtest (template-parser-test)
  template-2
  (ensure-same '(:template ("test" :private nil))
               (parse-single-template "{template test private=\"false\"}{/template}")))

(addtest (template-parser-test)
  template-3
  (ensure-same '(:template ("test" :autoescape nil :private t))
               (parse-single-template "{template test autoescape=\"false\" private=\"true\"}{/template}")))

(addtest (template-parser-test)
  template-4
  (ensure-same '(:template ("test") "Hello")
               (parse-single-template "{template test}
    Hello
{/template}")))

;; ;;;; substitions

(addtest (template-parser-test)
  substition-1
  (ensure-same `(:template ("substitions")
                 ,(coerce #(#\Space
                            #\Return
                            #\Newline
                            #\Tab
                            #\{
                            #\})
                          'string))
               (parse-single-template "{template substitions}{sp}{nil}{\\r}{\\n}{\\t}{lb}{rb}{/template}")))

;;;; print

(addtest (template-parser-test)
  print-1
  (ensure-same '(:template ("helloName") "Hello "
                 (:print (:VARIABLE :name)))
               (parse-single-template "{template helloName}Hello {$name}{/template}")))

(addtest (template-parser-test)
  print-2
  (ensure-same '(:template ("test") (:print (:operator + 2 2)))
               (parse-single-template "{template test}{2 + 2}{/template}")))

(addtest (template-parser-test)
  print-3
  (ensure-same '(:template ("test") (:print (:operator + 2 2) :no-autoescape t))
               (parse-single-template "{template test}{2 + 2 |noAutoescape}{/template}")))

(addtest (template-parser-test)
  print-4
  (ensure-same '(:template ("test") (:print (:operator + 2 2) :id t))
               (parse-single-template "{template test}{2 + 2 |id}{/template}")))

(addtest (template-parser-test)
  print-5
  (ensure-same '(:template ("test") 
                 (:print (:operator + 2 2)
                  :no-autoescape t
                  :id t
                  :escape-html t
                  :escape-uri t
                  :escape-js t
                  :insert-word-breaks 5))
               (parse-single-template "{template test}{2 + 2 |noAutoescape |id |escapeHtml |escapeUri |escapeJs  |insertWordBreaks:5}{/template}")))

(addtest (template-parser-test)
  print-6
  (ensure-same '(:template ("test") 
                 (:print (:operator + 2 2)
                  :no-autoescape t
                  :id t
                  :escape-html t
                  :escape-uri t
                  :escape-js t
                  :insert-word-breaks 5))
               (parse-single-template "{template test}{2 + 2 |noAutoescape |id | escapeHtml | escapeUri| escapeJs  |insertWordBreaks:5}{/template}")))


;; ;;;; literal

(addtest (template-parser-test)
  literal-1
  (ensure-same '(:template ("literal-test") 
                 (:literal "Test {$x} {foreach $foo in $bar}{$foo}{/foreach}"))
               (parse-single-template "{template literal-test}{literal}Test {$x} {foreach $foo in $bar}{$foo}{/foreach}{/literal}{/template}")))

;; ;;;; with

(addtest (template-parser-test)
  with-1
  (ensure-same '(:template ("with-test")
                 (:with
                  (((:variable :str) "Hello"))
                  ((:print (:variable :str)) " world")))
               (parse-single-template "{template with-test}{with str=\"'Hello'\"}{$str} world{/with}{/template}")))

(addtest (template-parser-test)
  with-2
  (ensure-same '(:template ("with-test")
                 (:with
                  (((:variable :r) (:operator + (:variable :a) (:variable :b)))
                   ((:variable :msg) "Result"))
                  ((:print (:variable :msg))
                   "="
                   (:print (:variable :r)))))
               (parse-single-template "{template with-test}{with r=\"$a + $b\" msg=\"'Result'\"}{$msg}={$r}{/with}{/template}")))

;; ;;;; if

(addtest (template-parser-test)
  if-1
  (ensure-same '(:template ("if-test")
                 (:if
                  ((:variable :x)
                   ("Hello "  (:print (:variable :x))))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{/if}{/template}")))

(addtest (template-parser-test)
  if-2
  (ensure-same '(:template ("if-test")
                 (:if
                  ((:variable :x)
                   ("Hello " (:print (:variable :x))))
                  (t ("Hello world"))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{else}Hello world{/if}{/template}")))

(addtest (template-parser-test)
  if-3
  (ensure-same '(:template ("if-test")
                 (:if
                  ((:variable :x) ("Hello " (:print (:variable :x))))
                  ((:variable :y) ("Hello " (:print (:variable :y))))
                  (t ("Hello world"))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{elseif $y}Hello {$y}{else}Hello world{/if}{/template}")))

(addtest (template-parser-test)
  if-4
  (ensure-same '(:template ("if-test")
                 (:if
                  ((:variable :x) ("Hello " (:print (:variable :x))))
                  ((:variable :y) ("Hello " (:print (:variable :y))))
                  ((:variable :z) ("By!"))
                  (t ("Hello world"))))
               (parse-single-template "{template if-test}{if $x}Hello {$x}{elseif $y}Hello {$y}{elseif $z}By!{else}Hello world{/if}{/template}")))

;;;; switch

(addtest (template-parser-test)
  switch-1
  (ensure-same '(:template ("switch-test")
                 (:switch (:variable :x)
                  nil
                  ((1) ("hello world"))
                  ((2 3 4) ("by-by"))))
               (parse-single-template "{template switch-test}{switch $x}{case 1}hello world{case 2, 3, 4}by-by{/switch}{/template}")))

(addtest (template-parser-test)
  switch-2
  (ensure-same '(:template ("switch-test")
                 (:switch (:variable :x)
                  ("default value")
                  ((1) ("hello world"))
                  ((2 3 4) ("by-by"))))
               (parse-single-template "{template switch-test}{switch $x}{case 1}hello world{case 2, 3, 4}by-by{default}default value{/switch}{/template}")))

;;;; foreach

(addtest (template-parser-test)
  foreach-1
  (ensure-same '(:template ("test")
                 (:foreach ((:variable :x) (:dotref (:variable :y) :foo))
                  ((:print (:variable :x)))))
               (parse-single-template "{template test}{foreach $x in $y.foo }{$x}{/foreach}{/template}")))

;;;; for

(addtest (template-parser-test)
  for-1
  (ensure-same '(:template ("test")
                 (:for ((:variable :x) (:range 10)) ("!")))
               (parse-single-template "{template test}{for $x in range(10)} ! {/for}{/template}")))

(addtest (template-parser-test)
  for-2
  (ensure-same '(:template ("test")
                 (:for ((:variable :x) (:range 4 10)) ("!")))
               (parse-single-template "{template test}{for $x in range(4, 10)} ! {/for}{/template}")))

(addtest (template-parser-test)
  for-3
  (ensure-same '(:template ("test")
                 (:for ((:variable :x) (:range 4 10 2)) ("!")))
               (parse-single-template "{template test}{for $x in range(4, 10, 2)} ! {/for}{/template}")))

;;;; call

(addtest (template-parser-test)
  call-1
  (ensure-same '(:template ("test")
                 (:call "hello-name" (:variable :x)))
               (parse-single-template "{template test}{call hello-name data=\"$x\" /}{/template}")))

(addtest (template-parser-test)
  call-2
  (ensure-same '(:template ("test")
                 (:call "hello-name" nil
                  ((:variable :name) (:variable :x))))
               (parse-single-template "{template test}{call hello-name}{param name: $x /}{/call}{/template}")))

(addtest (template-parser-test)
  call-3
  (ensure-same '(:template ("test")
                 (:call "hello-name" (:variable :data)
                  ((:variable :a) (:variable :x))
                  ((:variable :b) nil
                   "Hello " (:print (:variable :y)))))
               (parse-single-template "{template test}{call hello-name data=\"$data\"}{param a: $x /}{param b}Hello {$y}{/param} {/call}{/template}")))

(addtest (template-parser-test)
  call-5
  (ensure-same '(:template ("test")
                 (:call (:variable :name) nil))
               (parse-single-template "{template test}{call name=\"$name\"  /}{/template}")))

(addtest (template-parser-test)
  call-6
  (ensure-same '(:template ("test")
                 (:call (:variable :name) :all))
               (parse-single-template "{template test}{call name=\"$name\" data=\"all\"  /}{/template}")))

;;;; whitespaces

(addtest (template-parser-test)
  whitespace-1
  (ensure-same '(:template ("test") "Hello World")
               (parse-single-template "{template test}
    Hello
    World
{/template}")))

(addtest (template-parser-test)
  whitespace-2
  (ensure-same '(:template ("test")
                 (:literal "
 Hello
   World
"))
               (parse-single-template "{template test}
    {literal}
 Hello
   World
{/literal}
{/template}")))

