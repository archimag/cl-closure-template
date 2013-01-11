;;;; cl-backend-test.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.test)

(deftestsuite common-lisp-backend-test (closure-template-test) ()
              (:run-setup :once-per-test-case )
              (:dynamic-variables *default-closure-template-package*)
              (:setup (setf *default-closure-template-package*
                            (ensure-ttable-package "CLOSURE-TEMPLATE.TEST.TEMPLATES")))
              (:teardown (when (find-package *default-closure-template-package*)
                           (delete-package *default-closure-template-package*))))

(defun template-call (name &optional data)
  (funcall (find-symbol name *default-closure-template-package*)
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

(addtest (common-lisp-backend-test)
  print-4
  (ensure-same "~!@#$%25%5E&*()%7B%7D%5B%5D=:/,;?+'%22%5C"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{$arg|escapeUri}{/template}")
                 (template-call "TEST"
                                '(:arg "~!@#$%^&*(){}[]=:/,;?+\'\"\\")))))

(addtest (common-lisp-backend-test)
  print-5
  (ensure-same "~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B'%22%5C"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{$arg|id}{/template}")
                 (template-call "TEST"
                                '(:arg "~!@#$%^&*(){}[]=:/,;?+\'\"\\")))))


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
                 (let ((fun (find-symbol "CALCULATE" *default-closure-template-package*)))
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

(addtest (common-lisp-backend-test)
  calculate-9
  (ensure-same '("false" "true")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{if $val == 5}true{else}false{/if}{/template}")
                 (list (template-call "CALCULATE"
                                      '(:val 6))
                       (template-call "CALCULATE"
                                      '(:val 5))))))

(addtest (common-lisp-backend-test)
  calculate-10
  (ensure-same '("true" "false")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template calculate}{if $val != 5}true{else}false{/if}{/template}")
                 (list (template-call "CALCULATE"
                                      '(:val 6))
                       (template-call "CALCULATE"
                                      '(:val 5))))))

(addtest (common-lisp-backend-test)
  calculate-11
  (ensure-same '("True" "False" "True" "False" "True")
               (progn
                (compile-template :common-lisp-backend
                                  "{template calculate}{if strContains($str1, $str2)}True{else}False{/if}{/template}")
                (list (template-call "CALCULATE" '(:str1 "Blue Whale" :str2 "Blue"))
                      (template-call "CALCULATE" '(:str1 "Blue Whale" :str2 "Blute"))
                      (template-call "CALCULATE" '(:str1 "Blue Whale" :str2 "Wha"))
                      (template-call "CALCULATE" '(:str1 "Blue Whale" :str2 "What"))
                      (template-call "CALCULATE" '(:str1 "Blue Whale" :str2 ""))))))

(addtest (common-lisp-backend-test)
  calculate-12
  (ensure-same '("True" "False")
               (progn
                (compile-template :common-lisp-backend
                                  "{template calculate}{if isNonnull($x)}True{else}False{/if}{/template}")
                (list (template-call "CALCULATE" '(:x 1))
                      (template-call "CALCULATE" '(:y 1))))))
                      

;;;; substitions

(addtest (common-lisp-backend-test)
  substition-1
  (ensure-same (coerce #(#\Space #\Return #\Newline #\Tab #\{ #\}) 'string)
               (progn
                 (compile-template :common-lisp-backend
                                   "{template substitions}{sp}{nil}{\\r}{\\n}{\\t}{lb}{rb}{/template}")
                 (template-call "SUBSTITIONS"))))

;;;; injected-data

(addtest (common-lisp-backend-test)
  injected-data-1
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template helloWorld}{$foo} {$ij.foo}{/template}")
                 (let ((*injected-data* '(:foo "world")))
                   (template-call "HELLO-WORLD" '(:foo "Hello"))))))

(addtest (common-lisp-backend-test)
  injected-data-2
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template helloWorld}{$foo} {$ij.foo}{/template}
                                    {template callHelloWorld}{call helloWorld}{param foo: $foo /}{/call}{/template}")
                 (let ((*injected-data* '(:foo "world")))
                   (template-call "CALL-HELLO-WORLD" '(:foo "Hello"))))))
  

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

;;;; local variables handling

(addtest (common-lisp-backend-test)
  local-vars-1
  (ensure-same "56"
	       (progn
		 (compile-template :common-lisp-backend
				   "{template testLocal}{foreach $b in $c}{$b.d}{/foreach}{/template}")
		 (template-call "TEST-LOCAL"
				'(:c ((:d 5) (:d 6)))))))

(addtest (common-lisp-backend-test)
  local-vars-2
  (ensure-same "56"
	       (progn
		 (compile-template :common-lisp-backend
				   "{template testLocal}{foreach $b in $c}{$b.d.a}{/foreach}{/template}")
		 (template-call "TEST-LOCAL"
				'(:c ((:d (:a 5)) (:d (:a 6))))))))

;;;; literal

(addtest (common-lisp-backend-test)
  literal-1
  (ensure-same "&{$x}{}"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{literal}&{$x}{}{/literal}{/template}")
                 (template-call "TEST"))))

;;;; with

(addtest (common-lisp-backend-test)
  with-1
  (ensure-same "Hello Andrey"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{with greetingWord=\"'Hello'\" name=\"'Andrey'\"}{$greetingWord} {$name}{/with}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  with-2
  (ensure-same "Hello Andrey"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{let $greetingWord=\"'Hello'\" $name=\"'Andrey'\"}{$greetingWord} {$name}{/let}{/template}")
                 (template-call "TEST"))))

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
                                   "{template test}{foreach $opernand in $opernands}{sp}{$opernand}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-2
  (ensure-same '(" alpha beta gamma"  "Hello world")
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{sp}{$opernand}{ifempty}Hello world{/foreach}{/template}")
                 (list (template-call "TEST"
                                      '(:opernands ("alpha" "beta" "gamma")))
                       (template-call "TEST"
                                      nil)))))

(addtest (common-lisp-backend-test)
  foreach-3
  (ensure-same "012"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{index($opernand)}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-4
  (ensure-same "alpha+beta+gamma"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{if not isFirst($opernand)}+{/if}{$opernand}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-5
  (ensure-same "alpha+beta+gamma"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{$opernand}{if not isLast($opernand)}+{/if}{/foreach}{/template}")
                 (template-call "TEST"
                                '(:opernands ("alpha" "beta" "gamma"))))))

(addtest (common-lisp-backend-test)
  foreach-6
  (ensure-same "0a1a2a"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $opernand in $opernands}{index($opernand)}{$char}{/foreach}{/template}")
                 (template-call "TEST"
                                (alexandria:plist-hash-table '(:opernands ("alpha" "beta" "gamma") :char "a"))))))

;;;; list

(addtest (common-lisp-backend-test)
  list-1
  (ensure-same "123"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $x in [1, 2, 3]}{$x}{/foreach}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  list-2
  (ensure-same "AB"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{foreach $x in [$a, $b]}{$x}{/foreach}{/template}")
                 (template-call "TEST" '(:a "A" :b "B")))))

;;;; map

(addtest (common-lisp-backend-test)
  map-1
  (ensure-same "123"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}
    {let $obj=\"{'a': 1, 'b': 2, 'c': 3}\"}
         {$obj.a}{$obj.b}{$obj.c}
    {/let}
{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  map-2
  (ensure-same "Hello Masha"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template helloName}Hello {$name}{/template}
{template test}{call helloName data=\"{'name': 'Masha'}\"/}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  map-3
  (ensure-same "ABC"
               (progn
                 (compile-template :common-lisp-backend
                                   "
{template test}
    {foreach $key in keys({'a': 1, 'b': 2, 'c': 3})}
         {$key}
    {/foreach}
{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  map-4
  (ensure-same "423"
               (progn
                 (compile-template :common-lisp-backend
                                   "
{template test}
    {let $obj=\"augmentMap({'a': 1, 'b': 2, 'c': 3}, {'a': 4})\"}
         {$obj.a}{$obj.b}{$obj.c}
    {/let}
{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  map-5
  (ensure-same "ABCD"
               (progn
                 (compile-template :common-lisp-backend
                                   "
{template test}
    {let $obj=\"augmentMap({'a': 1, 'b': 2, 'c': 3}, {'a': 4, 'd': 5})\"}
         {foreach $key in keys($obj)}
             {$key}
         {/foreach}
    {/let}
{/template}")
                 (sort (template-call "TEST") #'char<))))

;;;; for

(addtest (common-lisp-backend-test)
  for-1
  (ensure-same "01234"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{for $i in range(5)}{$i}{/for}{/template}")
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

(addtest (common-lisp-backend-test)
  for-4
  (ensure-same "5152 6162 7172 "
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}
    {for $i in range(5, 8)}
        {nil}{for  $j in range(1, 3)}{$i}{$j}{/for}{sp}
    {/for}
{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  for-6
  (ensure-same "0b1b2b3b4b"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template test}{for $i in range(5)}{$i}{$char}{/for}{/template}")
                 (template-call "TEST"
                                (alexandria:plist-hash-table (list :char "b"))))))

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

(addtest (common-lisp-backend-test)
  call-6
  (ensure-same "Hello Hello Ivan"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-name}Hello {$name}{/template}
{template test}{call hello-name}{param name}{call hello-name data=\"all\"/}{/param}{/call}{/template}")
                 (template-call "TEST"
                                '(:name "Ivan")))))

(addtest (common-lisp-backend-test)
  call-7
  (ensure-same "Hello Masha"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-name}Hello {$name}{/template}
{template test}{call hello-name data=\"$author\"/}{/template}")
                 (template-call "TEST"
                                '(:author (:name "Masha"))))))

(addtest (common-lisp-backend-test)
  call-8
  (ensure-same "Hello Andrey from Krasnodar"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template hello-name}Hello {$name} from {$city}{/template}
{template test}
   {call hello-name data=\"$author\"}
       {param name: 'Andrey' /}
   {/call}
{/template}")
                 (template-call "TEST"
                                '(:author (:name "Masha"
                                           :city "Krasnodar"))))))

(addtest (common-lisp-backend-test)
  call-9
  (ensure-same "Hello world"
               (progn
                 (compile-template :common-lisp-backend
                                   "{template helloWorld}Hello world{/template}
{template test}{call name=\"'hello' + 'World'\" /}{/template}")
                 (template-call "TEST"))))

(addtest (common-lisp-backend-test)
  call-10
  (ensure-same "Hello world"
               (let ((pkg1 (ensure-ttable-package '#:closure-template.test.pkg1))
                     (pkg2 (ensure-ttable-package '#:closure-template.test.pkg2)))
                 (unwind-protect
                      (progn
                        (let ((*default-closure-template-package* pkg1))
                          (compile-template :common-lisp-backend
                                            "{template helloWorld}Hello world{/template}"))
                        (let ((*default-closure-template-package* pkg2))
                          (compile-template :common-lisp-backend
                                            "{template callHelloWorld}{call closureTemplate.test.pkg1.helloWorld/}{/template}")
                          (template-call "CALL-HELLO-WORLD")))
                   (delete-package pkg1)
                   (delete-package pkg2)))))
;;;; warnings

(deftestsuite common-lisp-backend-warnings-test (common-lisp-backend-test) ())

(addtest (common-lisp-backend-warnings-test)
  warning-1
  (ensure-no-warning
    (compile-template :common-lisp-backend
                      "{template hello-world}Hello world{/template}")))

(addtest (common-lisp-backend-warnings-test)
  warning-2
  (ensure-no-warning
    (compile-template :common-lisp-backend
                      "{template hello-world}{hasData()}{/template}")))

(addtest (common-lisp-backend-warnings-test)
  warning-3
  (ensure-no-warning
    (compile-template :common-lisp-backend
                      "{template hello-world}Hello world{/template}
{template test}{call hello-world data=\"all\" /}{/template}")))
