;;;; cl-backend-test.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.test)

(deftestsuite common-lisp-backend-test (closure-template-test) ()
              (:run-setup :once-per-test-case )
              (:dynamic-variables *default-translate-package*)
              (:setup (setf *default-translate-package*
                            (make-template-package :closure-template.test.templates)))
              (:teardown (when (find-package *default-translate-package*)
                           (delete-package *default-translate-package*))))

(defun template-call (name &optional data)
  (funcall (find-symbol name *default-translate-package*)
           data))

;;;; simple

(addtest (common-lisp-backend-test)
  simple-1
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-world}Hello world{/template}")
                 (template-call "HELLO-WORLD"))))

(addtest (common-lisp-backend-test)
  simple-2
  (ensure-same "<Hello world>"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-world}<Hello world>{/template}")
                 (template-call "HELLO-WORLD"))))

;;;; print

(addtest (common-lisp-backend-test)
  print-1
  (ensure-same "&lt;&amp;&quot;&#039;&gt;"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{$arg}{/template}")
                 (template-call "TEST"
                                '(:arg "<&\"'>")))))

(addtest (common-lisp-backend-test)
  print-2
  (ensure-same "<&\"'>"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test autoescape=\"false\"}{$arg}{/template}")
                 (template-call "TEST"
                                '(:arg "<&\"'>")))))

(addtest (common-lisp-backend-test)
  print-3
  (ensure-same "<&\"'>"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{$arg|noAutoescape}{/template}")
                 (template-call "TEST"
                                '(:arg "<&\"'>")))))


;;;; comment

(addtest (common-lisp-backend-test)
  comment-1
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-world}//Hello world
Hello world{/template}")
                 (template-call "HELLO-WORLD"))))

(addtest (common-lisp-backend-test)
  comment-2
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-world}/*Hello world*/Hello world{/template}")
                 (template-call "HELLO-WORLD"))))

;;;; calculate

(addtest (common-lisp-backend-test)
  calculate-1
  (ensure-same "20"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{(2 + 3) * 4}{/template}")
                 (template-call "CALCULATE"))))

(addtest (common-lisp-backend-test)
  calculate-2
  (ensure-same "20"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{(2 + 3) * 4}{/template}")
                 (template-call "CALCULATE"))))


(addtest (common-lisp-backend-test)
  calculate-3
  (ensure-same "2"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{(20 - 3) %  5}{/template}")
                 (template-call "CALCULATE"))))


(addtest (common-lisp-backend-test)
  calculate-4
  (ensure-same '("Hello world" "10")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{hasData() ? 10 : 'Hello world'}{/template}")
                 (list (template-call "CALCULATE")
                       (template-call "CALCULATE" t)))))

(addtest (common-lisp-backend-test)
  calculate-5
  (ensure-null (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{randomInt(10)}{/template}")
                 (let ((fun (find-symbol "CALCULATE" *default-translate-package*)))
                   (iter (repeat 100)
                         (let ((i (parse-integer (funcall fun))))
                           (if (or (> i 9)
                                   (< i 0))
                               (collect i))))))))

(addtest (common-lisp-backend-test)
  calculate-6
  (ensure-same '("5" "Hello world" "Number: 6")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{$x + $y}{/template}")
                 (list (template-call "CALCULATE"
                                      '(:x 2 :y 3))
                       (template-call "CALCULATE"
                                      '(:x "Hello " :y "world"))
                       (template-call "CALCULATE"
                                      '(:x "Number: " :y 6))))))

(addtest (common-lisp-backend-test)
  calculate-7
  (ensure-same '("3" "2.72" "2.7183")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{not hasData() ? round(3.141592653589793) : round(2.7182817, $num)}{/template}")
                 (list (template-call "CALCULATE"
                                      nil)
                       (template-call "CALCULATE"
                                      '(:num 2))
                       (template-call "CALCULATE"
                                      '(:num 4))))))

(addtest (common-lisp-backend-test)
  calculate-8
  (ensure-same '("1" "16" "9" "36")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{$array[$index]}{/template}")
                 (list (template-call "CALCULATE"
                                      '(:array (0 1 4 9 16 25 36) :index 1))
                       (template-call "CALCULATE"
                                      '(:array (0 1 4 9 16 25 36) :index 4))
                       (template-call "CALCULATE"
                                      '(:array (0 1 4 9 16 25 36) :index 3))
                       (template-call "CALCULATE"
                                      '(:array (0 1 4 9 16 25 36) :index 6))))))

;;;; substitions

(addtest (common-lisp-backend-test)
  substition-1
  (ensure-same (coerce #(#\Space #\Return #\Newline #\Tab #\{ #\}) 'string)
               (progn
                 (compile-template :common-lisp-backend
                                   "{template substitions}{sp}{nil}{\\r}{\\n}{\\t}{lb}{rb}{/template}")
                 (template-call "SUBSTITIONS"))))

;;;; dotted variables

(addtest (common-lisp-backend-test)
  dotted-vars-1
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template dotted}{$obj.first} {$obj.second}{/template}")
                 (template-call "DOTTED"
                                '(:obj (:first "Hello" :second "world"))))))

(addtest (common-lisp-backend-test)
  dotted-vars-2
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template dotted}{$obj.msg.first} {$obj.msg.second}{/template}")
                 (template-call "DOTTED"
                                '(:obj (:msg (:first "Hello" :second "world")))))))

;;;; if

(addtest (common-lisp-backend-test)
  if-1
  (ensure-same '("Hello Andrey" "")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{if $name}Hello {$name}{/if}{/template}")
                 (list (template-call "TEST"
                                      '(:name "Andrey"))
                       (template-call "TEST"
                                      nil)))))

(addtest (common-lisp-backend-test)
  if-2
  (ensure-same '("Hello Andrey" "Hello Guest")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}Hello {if $name}{$name}{else}Guest{/if}{/template}")
                 (list (template-call "TEST"
                                      '(:name "Andrey"))
                       (template-call "TEST"
                                      nil)))))

(addtest (common-lisp-backend-test)
  if-3
  (ensure-same '("Hello Andrey" "By Masha" "Thank Vasy" "Guest?")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{if $hello}Hello {$hello}{elseif $by}By {$by}{elseif $thank}Thank {$thank}{else}Guest?{/if}{/template}")
                 (list (template-call "TEST"
                                      '(:hello "Andrey"))
                       (template-call "TEST"
                                      '(:by "Masha"))
                       (template-call "TEST"
                                      '(:thank "Vasy"))
                       (template-call "TEST"
                                      nil)))))

;;;; switch

(addtest (common-lisp-backend-test)
  switch-1
  (ensure-same '("Variant 1: 0" "Variant 2: Hello" "Miss!" "Variant 2: 2")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{switch $var}{case 0}Variant 1: {$var}{case 1, 'Hello', 2}Variant 2: {$var}{default}Miss!{/switch}{/template}")
                 (list (template-call "TEST"
                                      '(:var 0))
                       (template-call "TEST"
                                      '(:var "Hello"))                       
                       (template-call "TEST"
                                      nil)
                       (template-call "TEST"
                                      '(:var 2))))))

(addtest (common-lisp-backend-test)
  switch-2
  (ensure-same '("Variant 1: 0" "Variant 2: Hello" "" "Variant 2: 2")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{switch $var}{case 0}Variant 1: {$var}{case 1, 'Hello', 2}Variant 2: {$var}{/switch}{/template}")
                 (list (template-call "TEST"
                                      '(:var 0))
                       (template-call "TEST"
                                      '(:var "Hello"))                       
                       (template-call "TEST"
                                      nil)
                       (template-call "TEST"
                                      '(:var 2))))))
               
;;;; foreach

(addtest (common-lisp-backend-test)
  foreach-1
  (ensure-same " alpha beta gamma"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands} {$opernand}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-2
  (ensure-same '(" alpha beta gamma"  "Hello world")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands} {$opernand}{ifempty}Hello world{/foreach}{/template}")
                 (list (template-call "TEST"
                                      '(:opernands ("alpha" "beta" "gamma")))
                       (template-call "TEST"
                                      nil)))))

(addtest (common-lisp-backend-test)
  foreach-3
  (ensure-same '"012"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{index($opernand)}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-4
  (ensure-same "alpha + beta + gamma"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{if not isFirst($opernand)} + {/if}{$opernand}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-5
  (ensure-same "alpha + beta + gamma"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{$opernand}{if not isLast($opernand)} + {/if}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))
;;;; for

(addtest (common-lisp-backend-test)
  for-1
  (ensure-same " 0 1 2 3 4"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{for $i in range(5)} {$i}{/for}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  for-2
  (ensure-same "456789"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{for $i in range(4, 10)}{$i}{/for}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  for-3
  (ensure-same "147"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{for $i in range($from, $to, $by)}{$i}{/for}{/template}")
                 (template-call "TEST"
                                '(:from 1 :to 10 :by 3)))))

;;;; call

(addtest (common-lisp-backend-test)
  call-1
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-world}Hello world{/template}
{template test}{call hello-world /}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  call-2
  (ensure-same "Hello Andrey"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-name}Hello {$name}{/template}
{template test}{call hello-name}{param name: 'Andrey'/}{/call}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  call-3
  (ensure-same "Hello Andrey"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-name}Hello {$name}{/template}
{template test}{call hello-name}{param name}Andrey{/param}{/call}{/template}")
                 (template-call "TEST"))))


(addtest (common-lisp-backend-test)
  call-4
  (ensure-same "Hello Masha"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-name}Hello {$name}{/template}
{template test}{call hello-name data=\"all\"/}{/template}")
                 (template-call "TEST"
                                '(:name "Masha")))))

(addtest (common-lisp-backend-test)
  call-5
  (ensure-same "<strong>Hello world</strong>"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello}<strong>Hello world</strong>{/template}
{template test}{call hello /}{/template}")
                 (template-call "TEST"))))
