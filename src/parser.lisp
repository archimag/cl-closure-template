;;;; parser.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey

(in-package #:closure-template.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *closure-template-rules* (make-hash-table)))

(defmacro with-closure-template-rules (&body body)
  `(let ((esrap::*rules* *closure-template-rules*))
     ,@body))

(defmacro define-rule (symbol expression &body options)
  `(with-closure-template-rules
     (defrule ,symbol ,expression ,@options)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lispify-string (str)
    (coerce (iter (for ch in-string str)
                  (when (upper-case-p ch)
                    (collect #\-))
                  (collect (char-upcase ch)))
            'string))

  (defun lispify-name (str)
    (intern (lispify-string str) :keyword))

  (defun not-equal (obj1 obj2)
    (not (equal obj1 obj2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; whitespace

(define-rule whitespace (+ (or #\space #\tab #\newline))
  (:constant " "))

;;; string literal

(defmacro define-escape-sequence (name char &optional (result char))
  `(define-rule ,name (and #\\ ,char)
     (:constant ,result)))

(define-escape-sequence %reverse-solidus #\\)
(define-escape-sequence %apostrophe #\')
(define-escape-sequence %quotation-mark #\")
(define-escape-sequence %newline #\n #\Newline)
(define-escape-sequence %carriage-return #\r #\Return)
(define-escape-sequence %tab #\t #\Tab)
(define-escape-sequence %backspace #\b #\Backspace)
(define-escape-sequence %form-feed #\f #\Page)

(define-rule hex-char (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                      "A" "B" "C" "D" "E" "F"
                      "a" "b" "c" "d" "e" "f"))

(define-rule %unicode-char-sequence (and #\\ #\u hex-char hex-char hex-char hex-char)
  (:lambda (list)
    (code-char (parse-integer  (concat (cdr (cdr list))) :radix 16))))

(defun not-quote-p (char)
  (not (eql #\' char)))

(define-rule string-char
    (or %reverse-solidus
        %apostrophe
        %quotation-mark
        %newline
        %carriage-return
        %tab
        %backspace
        %form-feed
        %unicode-char-sequence
        (not-quote-p character)))

(define-rule string (and #\' (* string-char) #\')
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (concat string)))

;;; number literal

(define-rule decimal-integer (+ (digit-char-p character))
  (:lambda (list)
    (parse-integer (concat list))))

(define-rule hexadecimal-integer (and "0x" (+ hex-char))
  (:destructure (s hexs)
    (declare (ignore s))
    (parse-integer (concat hexs) :radix 16)))

(define-rule integer (or hexadecimal-integer decimal-integer))

(define-rule float (and (+ (digit-char-p character))
                    #\. (* (digit-char-p character))
                    (? (and (or "e" "E" ) (? (or "-" "+")) (* (digit-char-p character)))))
  (:lambda (list)
    (parse-number:parse-number (concat list))))

(define-rule number (or float integer))

;;; null and boolean

(define-rule null "null" (:constant :nil))
(define-rule true "true" (:constant :t))
(define-rule false "false" (:constant :nil))
(define-rule boolean (or true false))

;;; literal

(define-rule expression-literal (or number null boolean  string))

;;; variable

(define-rule alpha-char (alpha-char-p character))

(define-rule alphanumeric (alphanumericp character))

(define-rule simple-name (and alpha-char (* (or alphanumeric #\_ )))
  (:lambda (list)
    (concat list)))

(define-rule dotref (or (and #\. simple-name)
                    (and "['" simple-name "']"))
  (:lambda (list)
    (list :dot (lispify-name (second list)))))
  
(define-rule aref (or (and #\[ expression #\])
                  (and #\. integer))
  (:lambda (list)
    (list 'elt (second list))))

(define-rule variable (and "$" simple-name)
  (:lambda (list)
    (list :variable (lispify-name (second list)))))

;;; funcall

(defparameter *possible-functions*
  '("isFirst" "isLast" "index"
    "hasData"
    "randomInt"
    "length"
    "round"
    "floor" "ceiling"
    "min" "max"
    "range"))

(define-rule funcall (and simple-name (? whitespace) #\( (? (and expression (* (and #\, expression )))) #\))
  (:destructure (name w b1 args b2)
    (declare (ignore w b1 b2))
    (unless (find name *possible-functions* :test #'string-equal)
      (error "Bad function name: ~A" name))
    (cons (lispify-name name)
          (if (second args)
              (cons (first args)
                    (mapcar #'second (second args)))
              (list (first args))))))

;;; parenthesis

(define-rule parenthesis (and #\( expression  #\))
  (:destructure (b1 expr b2)
    (declare (ignore b1 b2))
    expr))

;;; expression

(defun replace-subseq (sequence start length new)
  (nconc (subseq sequence 0 start) (list new)
                    (subseq sequence (+ start length))))


(defun reduce-ref (expr)
  (let* ((pos (position-if #'(lambda (i) 
                               (and (consp i)
                                    (not (third i))
                                    (member (car i) '(elt :dot))))
                           expr))
         (op (if pos (elt expr pos))))
    (if (and pos
             (> pos 0))
        (replace-subseq expr (1- pos) 2
                        (list (if (eql (car op) :dot)
                                  'getf
                                  'elt)
                              (elt expr (1- pos))
                              (second op))))))

(define-rule expression-part (and (? whitespace)
                                  (+ (or expression-literal variable dotref aref funcall parenthesis))
                                  (? whitespace))
  (:destructure (w1 expr w2)
    (declare (ignore w1 w2))
    (labels ((ref-p (i)
               (and (consp i)
                    (not (third i))
                    (member (car i) '(elt :dot))))
             (reduce-ref (expr)
               (let* ((pos (position-if #'ref-p expr))
                      (op (if pos (elt expr pos))))
                 (when (and pos (> pos 0))
                   (replace-subseq expr
                                   (1- pos)
                                   2
                                   (list (if (eql (car op) :dot) 'getf 'elt)
                                         (elt expr (1- pos))
                                         (second op)))))))
      (iter
        (unless (cdr expr)
          (return (first expr)))
        (setf expr (reduce-ref expr))))))

(defmacro define-operator (name &optional (val (string-downcase (symbol-name name))))
  `(define-rule ,name (and (? whitespace) ,val (? whitespace))
     (:constant ',name)))

(define-operator -)
(define-operator not)
(define-operator *)
(define-operator /)
(define-operator rem "%")
(define-operator +)
(define-operator <)
(define-operator >)
(define-operator <=)
(define-operator >=)
(define-operator equal "==")
(define-operator not-equal "!=")
(define-operator and "and")
(define-operator or "or")
(define-operator ?)
(define-operator |:|)

(define-rule operator (or - not
                          * / rem
                          +
                          <= >= < >
                          equal not-equal
                          and or
                          ? |:|))

(defparameter *infix-ops-priority* 
  '(* / rem
    + -
    < > <= >=
    equal  not-equal
    and not
    or
    ))


(defun reduce-infix (infix)
  "Find the highest-precedence operator in INFIX and reduce accordingly."
  (or
   ;; - (unary)
   (if (eql (first infix) '-)
       (replace-subseq infix 0 2
                       (list '-
                             (second infix))))
   
   ;;?: ternary
   (let* ((pos1 (position '? infix))
          (pos2 (if pos1 (position '|:| infix :start pos1)))
          (len (length infix)))
     (if pos2
         (replace-subseq infix 0 len
                         (list 'if
                               (->prefix (subseq infix 0 pos1))
                               (->prefix (subseq infix (1+ pos1) pos2))
                               (->prefix (subseq infix (1+ pos2)))))))
   
   ;; binary and unary
   (let* ((pos (iter (for op in *infix-ops-priority*)
                     (for pos = (position op infix))
                     (finding pos such-that pos)))
          (op (if pos
                  (find (elt infix pos) *infix-ops-priority*))))
     (if pos
         (cond
           ((eql op 'not)
            (replace-subseq infix pos 2 
                            (list 'not 
                                  (elt infix (+ pos 1)))))
           (t
            (replace-subseq infix (- pos 1) 3
                            (list op
                                  (elt infix (- pos 1)) 
                                  (elt infix (+ pos 1))))))))))

(defun ->prefix (infix)
  "Convert an infix expression to prefix."
  (iter
     (unless (> (length infix) 1)
       (return (first infix)))
     (setf infix (reduce-infix infix))))
   

(define-rule expression (and (? (or - not)) expression-part (* (and operator expression-part)))
  (:destructure (u expr rest)
    (let ((infix (cons expr
                       (iter (for i in rest)
                             (collect (first i))
                             (collect (second i))))))
      (->prefix (if u
                    (cons u infix)
                    infix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; simple text

(defun not-brace (character)
  (not (member character '(#\{ #\} #\Space #\Tab #\Return #\Newline))))

(define-rule simple-text (+ (not-brace character))
  (:lambda (list)
    (concat list)))

;;;; commment

(define-rule simple-comment (and  "//" (*  (and (! #\Newline) character)) whitespace)
  (:lambda (list)
    (list 'comment (concat list))))

(define-rule multiline-comment (and (? whitespace) "/*" (*  (and (! "*/") character)) "*/")
  (:lambda (list)
    (list 'comment (concat list))))

(define-rule comment (or simple-comment multiline-comment))

;;;; substition

(defvar *substitiotn-map* (make-hash-table))

(defmacro define-substitions (name str value)
  `(progn
       (setf (gethash ',name *substitiotn-map*)
             ,(if (characterp  value)
                         (string value)
                         value))
       (define-rule ,name (and (? whitespace) ,str (? whitespace))
         (:constant ',name))))

(define-substitions space-tag "{sp}" #\Space)
(define-substitions emptry-string "{nil}" "")
(define-substitions carriage-return "{\\r}" #\Return)
(define-substitions line-feed "{\\n}" #\Newline)
(define-substitions tag "{\\t}" #\Tab)
(define-substitions left-brace "{lb}" #\{)
(define-substitions right-brace "{rb}" #\})

(define-rule substition (or space-tag emptry-string carriage-return line-feed tag left-brace right-brace))

;;;; literag tag

(define-rule literal (and "{literal}" (*  (and (! "{/literal}") character)) "{/literal}")
  (:destructure (start any end)
    (declare (ignore start end))
    (list ' literal (concat any))))

;;;; print

(defmacro define-print-directive (name value)
  `(define-rule ,name (and (? whitespace) "|" (? whitespace) ,value (? whitespace))
     (:constant '(,(lispify-name value) t))))

(define-print-directive no-autoescape-d "noAutoescape")
(define-print-directive id-d "id")
(define-print-directive escape-html-d "escapeHtml")
(define-print-directive escape-uri-d "escapeUri")
(define-print-directive escape-js-d "escapeJs")

(define-rule insert-word-breaks-d (and (? whitespace) "|" (? whitespace) "insertWordBreaks:" decimal-integer (? whitespace))
  (:destructure (w1 d w2 i value w3)
    (declare (ignore w1 d w2 i w3))
    (list :insert-word-breaks value)))
  
(define-rule print-directive (or no-autoescape-d id-d escape-html-d escape-uri-d escape-js-d insert-word-breaks-d))

(define-rule print-tag (and (or "{print " "{")  expression (* print-directive) "}")
  (:destructure (start expr directives end)
    (declare (ignore start end))
    (list* 'print-tag
           expr
           (iter (for (key value) in directives)
                 (collect key)
                 (collect value)))))

;;; if-tag

(define-rule else (and "{else}" code-block)
  (:destructure (else code)
    (declare (ignore else))
    (list t code)))

(define-rule elseif (and "{elseif" whitespace expression "}" code-block)
  (:destructure (start w expr end code)
    (declare (ignore start end w))
    (list expr code)))

(define-rule if-tag (and "{if" whitespace expression "}" code-block (* elseif) (? else) "{/if}")
  (:destructure (start w expr rb block elseif else end)
    (declare (ignore start w rb end))
    (cons 'if-tag
          (remove nil
                  `((,expr ,block) ,@elseif ,@(list else))))))

;;; switch

(define-rule case (and (? whitespace) "{case" whitespace expression (* (and #\, expression)) "}" code-block)
  (:destructure (w1 start w2 expr rest end code)
    (declare (ignore start w1 w2 end))
    (list (cons expr (mapcar #'second rest))
          code)))

(define-rule default (and (? whitespace) "{default}" code-block)
  (:destructure (w start code)
    (declare (ignore w start))
    code))

(define-rule switch (and "{switch" whitespace expression "}" (* case) (? default) (? whitespace) "{/switch}")
  (:destructure (start w1 expr rb cases default w2 end)
    (declare (ignore start w1 rb w2 end))
    (list* 'switch-tag
           expr
           default
           cases)))

;;; foreach

(define-rule ifempty (and "{ifempty}" code-block)
  (:destructure (start code)
    (declare (ignore start))
    code))

(define-rule foreach (and "{foreach" (+ whitespace) variable (+ whitespace) "in" expression "}" code-block (? ifempty) "{/foreach}")
  (:destructure (start w1 var w2 in expr rb code ifempty end)
    (declare (ignore start w1 w2 in rb end))
    (if ifempty
        (list 'foreach
              (list var expr)
              code
              ifempty)
        (list 'foreach
              (list var expr)
              code))))

;;; for

(define-rule range (and "range" (* whitespace) "("
                        (and expression (? (and #\, expression)) (? (and #\, expression)))
                        (* whitespace) ")")
  (:destructure (start w1 lb args w2 rb)
    (declare (ignore start w1 lb w2 rb))
    (cons :range    
          (remove nil
                  (cons (first args)
                        (mapcar #'second (cdr args)))))))
        

(define-rule for (and "{for" (+ whitespace) variable (+ whitespace) "in"
                      (+ whitespace) range (* whitespace) "}"
                      code-block "{/for}")
  (:destructure (start w1 var w2 in w3 range w4 rb code end)
    (declare (ignore start w1 w2 in w3 w4 rb end))
    (list* 'for-tag
           (list var range)
           code)))

;;; call

(define-rule short-param (and "{param" whitespace simple-name (? whitespace) #\: whitespace expression "/}")
  (:destructure (start w1 name w2 c w3 expr end)
    (declare (ignore start w1 w2 c w3 end))
    (list 'param
          (list :variable (lispify-name name))
          expr)))

(define-rule full-param (and "{param" whitespace simple-name (? whitespace) "}"
                             code-block "{/param}")
  (:destructure (start w1 name w2 rb code end)
    (declare (ignore start w1 w2 rb end))
    (list* 'param
           (list :variable (lispify-name name))
           nil
           code)))

(define-rule param (and (? whitespace) (or short-param full-param) (? whitespace))
  (:lambda (list)
    (second list)))

(define-rule call-data-all (and whitespace "data=\"all\"")
  (:constant :all))

(define-rule call-data-expr (and whitespace "data=\"" expression "\"")
  (:destructure (w1 d expr q)
    (declare (ignore w1 d q))
    expr))

(define-rule call-data (or call-data-all call-data-expr))

(define-rule template-name (and alpha-char (*  (or alphanumeric #\_ #\-)))
  (:destructure (first rest)
    (concat first rest)))

(define-rule call-template-name (or (and "name=\"" expression "\"") template-name)
  (:lambda (name)
    (if (consp name)
        (second name)
        name)))
                
(define-rule short-call (and "{call" whitespace call-template-name (? call-data) (? whitespace) "/}")
  (:destructure (start w1 name data w2 end)
    (declare (ignore start w1 w2 end))
    (list 'call name data)))

(define-rule full-call (and "{call" whitespace call-template-name (? call-data) (? whitespace) "}"
                            (* param) "{/call}")
  (:destructure (start w1 name data w2 rb params end)
    (declare (ignore start w1 w2 rb end))
    (list* 'call name data params)))

(define-rule call (or short-call full-call))

;;; code-block
        
(define-rule code-block
    (+ (or substition
           literal
           if-tag
           switch
           foreach
           for
           call

           comment
           whitespace
           simple-text

           print-tag)))

;;; namespace

(define-rule namespace (and "{namespace" whitespace (and template-name (* (and #\. template-name))) (? whitespace) #\})
  (:destructure (start w1 name w2 end)
    (declare (ignore start w1 w2 end))
    (concat name)))

;;; template

(defun check-expression-p (obj)
  (and (consp obj)
       (or (keywordp (car obj))
           (find (car obj)
                 '(- not + * / rem > < >= <= equal not-equal and or if)))))
     
(defun concat-neighboring-strings (obj)
  (if (and (consp obj)
           (not (check-expression-p obj)))
      (iter (for x on obj)
            (for item = (car x))
            (with tmp-string)
            (cond
              ((stringp item) (setf tmp-string
                                    (concatenate 'string
                                                 tmp-string
                                                 item)))
              (tmp-string (collect tmp-string)
                          (setf tmp-string nil)
                          (collect (concat-neighboring-strings item)))
              (t (collect (concat-neighboring-strings item))))
            (when (and (null(cdr x))
                       tmp-string)
              (collect tmp-string)))
      obj))

(defun replace-substition (code)
  (cond
    ((symbolp code)
     (or (gethash code *substitiotn-map*)
         code))
    ((consp code)
     (iter (for item in code)
           (collect (replace-substition item))))
    (t code)))

(defun trim-whitespace (code)
  (let ((result code))
    (when (equal (car result) " ")
      (setf result (cdr code)))
    (when (equal (car (last result)) " ")
      (setf result
            (subseq result 0 (1- (length result)))))
    result))

(defun simplify-code (code)
  (concat-neighboring-strings
   (replace-substition
    (trim-whitespace
     code))))

(define-rule template (and "{template" whitespace template-name
                           (? (and whitespace "autoescape=\"" boolean "\""))
                           (? (and whitespace "private=\"" boolean "\""))
                           (? whitespace) "}" (? whitespace)
                           (? code-block)
                           "{/template}")
  (:destructure (start w1 name autoescape private w2 rb w3 code end)
    (declare (ignore start w1 w2 rb w3 end))
    (let ((traits (cons name
                        (concatenate 'list
                                     (if autoescape (list :autoescape (eql (third autoescape) :t)))
                                     (if private  (list :private (eql (third private) :t)))))))
      (if code
          (list* 'template
                 traits
                 (simplify-code code))
          (list 'template traits)))))
                
;;; toplevel 

(define-rule toplevel (and (? (and (* (or comment whitespace)) namespace))
                           (* (or comment whitespace template)))
  (:destructure (header items)
    (list* 'namespace
           (second header)
           (iter (for item in items)
                 (when (and (consp item) (eql (car item) 'template))
                   (collect item))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun closure-template-parse (symbol text)
  (with-closure-template-rules
    (esrap:parse symbol text)))

(defun parse-expression (text)
  (closure-template-parse 'expression text))

(defun parse-template (obj)
  (closure-template-parse 'toplevel
                          (typecase obj
                            (string obj)
                            (pathname (alexandria:read-file-into-string obj)))))
