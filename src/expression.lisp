;;;; expression.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey

(in-package #:closure-template.parser.expression)

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *closure-template-rules* (make-hash-table)))

(defmacro with-closure-template-rules (&body body)
  `(let ((esrap::*rules* *closure-template-rules*))
     ,@body))

(defmacro define-rule (symbol expression &body options)
  `(with-closure-template-rules
     (defrule ,symbol ,expression ,@options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule whitespace (+ (or #\space #\tab #\newline))
  (:constant #\Space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string literal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-rule string-char (or %reverse-solidus
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; number literal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; null and boolean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule null "null" (:constant :nil))
(define-rule all "all" (:constant :all))

(define-rule true "true" (:constant :t))
(define-rule false "false" (:constant :nil))
(define-rule boolean (or true false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; literal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule literal (or number null boolean all string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule alphanumeric (alphanumericp character))

(define-rule simple-name (and (alpha-char-p character) (* (or alphanumeric #\_ )))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; funcall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parenthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule parenthesis (and #\( expression  #\))
  (:destructure (b1 expr b2)
    (declare (ignore b1 b2))
    expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                  (+ (or literal variable dotref aref funcall parenthesis))
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

(define-rule ternary (and expression #\? expression #\: expression)
  (:destructure (condition q expr1 c expr2)
    (declare (ignore q c))
    (list 'if condition expr1 expr2)))


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
;;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun closure-template-parse (text symbol)
  (with-closure-template-rules
    (let ((end (length text)))
      (esrap::process-parse-result
       (let ((esrap::*cache* (esrap::make-cache)))
         (esrap::eval-expression symbol text 0 end))
       text
       end
       nil))))

(defun parse-expression (text)
  (closure-template-parse text 'expression))