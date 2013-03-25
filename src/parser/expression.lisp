;;;; expression.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.parser)

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
    (code-char (parse-integer  (text (cdr (cdr list))) :radix 16))))

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
    (text string)))

;;; number literal

(define-rule decimal-integer (+ (digit-char-p character))
  (:lambda (list)
    (parse-integer (text list))))

(define-rule hexadecimal-integer (and "0x" (+ hex-char))
  (:destructure (s hexs)
    (declare (ignore s))
    (parse-integer (text hexs) :radix 16)))

(define-rule integer (or hexadecimal-integer decimal-integer))

(define-rule float (and (+ (digit-char-p character))
                    #\. (* (digit-char-p character))
                    (? (and (or "e" "E" ) (? (or "-" "+")) (* (digit-char-p character)))))
  (:lambda (list)
    (parse-number:parse-number (text list))))

(define-rule number (or float integer))

;;; null and boolean

(define-rule null "null" (:constant :nil))
(define-rule true "true" (:constant :t))
(define-rule false "false" (:constant :nil))
(define-rule boolean (or true false))

;;; literal

(define-rule expression-literal (or number null boolean string))

;;; variable

(define-rule alpha-char (alpha-char-p character))

(define-rule alphanumeric (alphanumericp character))

(define-rule simple-name (and alpha-char (* (or alphanumeric #\_ )))
  (:text t))

(define-rule variable (and "$" simple-name)
  (:lambda (list)
    (make-instance 'var
                   :jsname (second list)
                   :name (lispify-name (second list)))))

(defclass var ()
  ((name :initarg :name :reader var-name)
   (jsname :initarg :jsname :reader var-jsname)))

;;; injected data

(defclass injected-data () ())

(define-rule injected-data (and "$ij")
  (:constant (make-instance 'injected-data)))

;;; dotref

(defclass ref ()
  ((expression :initform nil :initarg :expr :reader ref-expr)))

(define-rule dotref (or (and #\. simple-name)
                    (and "['" simple-name "']"))
  (:lambda (list)
    (make-instance 'dotref
                   :jsname (second list)
                   :name (lispify-name (second list)))))

(defclass dotref (ref)
  ((name :initarg :name :reader dotref-name)
   (jsname :initarg :jsname :reader dotref-jsname)))

;;; arref

(define-rule aref (or (and #\[ expression #\])
                  (and #\. integer))
  (:lambda (list)
    (make-instance 'arref :position (second list))))

(defclass arref (ref)
  ((position :initarg :position :reader arref-position)))

;;; list

(define-rule list-expr (and #\[ (? (and expression (* (and #\, expression)))) #\])
  (:destructure (lsb expr rsb)
    (declare (ignore lsb rsb))
    (make-instance 'list-expr
                   :values (remove "," (alexandria:flatten expr) :test #'equal))))

(defclass list-expr ()
  ((values :initarg :values :reader list-expr-values)))

;;; map-expr

(define-rule map-pair (and expression #\: expression)
  (:destructure (key i value)
    (declare (ignore i))
    (list key value)))

(define-rule empty-map-expr (and #\{ (* whitespace) #\: (* whitespace)  #\})
  (:lambda (_)
    (declare (ignore _))
    (make-instance 'map-expr :pairs nil)))

(define-rule real-map-expr (and #\{ (and map-pair (* (and #\, map-pair)))  #\})
  (:destructure (lsb expr rsb)
    (declare (ignore lsb rsb))
    (make-instance 'map-expr
                   :pairs (cons (car expr)
                                (mapcar #'second (cadr expr))))))

(define-rule map-expr (or empty-map-expr real-map-expr))

(defclass map-expr ()
  ((pairs :initarg :pairs :reader map-expr-items)))

;;; funcall

(defparameter *possible-functions*
  '("isFirst" "isLast" "index"
    "isNonnull"
    "hasData"
    "length"
    "keys" "augmentMap"
    "round"
    "floor" "ceiling"
    "min" "max"
    "randomInt"
    "strContains"))

(defun add-possible-function (name)
  (pushnew name *possible-functions* :test #'string-equal))

(define-rule funcall (and simple-name (? whitespace) #\( (? (and expression (* (and #\, expression )))) #\))
  (:destructure (name w b1 args b2)
    (declare (ignore w b1 b2))
    (unless (find name *possible-functions* :test #'string-equal)
      (error "Bad function name: ~A" name))
    (make-instance 'fcall
                   :jsname name
                   :name (lispify-name name)
                   :args (cond
                           ((second args)
                            (cons (first args)
                                  (mapcar #'second (second args))))
                           ((first args)
                            (list (first args)))
                           (t nil)))))

(defclass fcall ()
  ((name :initarg :name :reader fcall-name)
   (jsname :initarg :jsname :reader fcall-jsname)
   (args :initarg :args :reader fcall-args)))

;;; parenthesis

(define-rule parenthesis (and #\( expression  #\))
  (:destructure (b1 expr b2)
    (declare (ignore b1 b2))
    expr))

;;; expression

(defun replace-subseq (sequence start length new)
  (nconc (subseq sequence 0 start)
         (list new)
         (subseq sequence (+ start length))))

(defun reduce-ref (expr)
  (iter (unless (cdr expr)
          (return (first expr)))
        (iter (for pos from 0)
              (for item in expr)
              (for prev previous item)
              (when (and (typep item 'ref)
                         (null (ref-expr item)))
                (setf (slot-value item 'expression)
                      prev)
                (setf expr
                      (replace-subseq expr (1- pos) 2 item))
                (return)))))

(define-rule expression-part (and (? whitespace)
                                  (+ (and (or expression-literal injected-data variable funcall parenthesis list-expr map-expr)
                                          (* (or aref dotref))))
                                  (? whitespace))
  (:destructure (w1 expr w2)
    (declare (ignore w1 w2))
    (reduce-ref (alexandria:flatten expr))))

(defclass operator ()
  ((name :initarg :name :reader op-name)
   (args :initform nil :initarg :args :reader op-args)))

(defmacro define-operator (name &optional (val (string-downcase (symbol-name name))))
  `(define-rule ,name (and (? whitespace) ,val (? whitespace))
     (:lambda (list)
       (declare (ignore list))
       (make-instance 'operator :name ',name))))

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

(define-rule |ternary-?:| (and (? whitespace) #\? expression #\: (? whitespace))
  (:destructure (w1 question expr colon w2)
    (declare (ignore w1 w2 question colon))
    (list (make-instance 'operator :name '?)
          expr
          (make-instance 'operator :name '|:|))))

(define-rule operator
  (or - not
      * / rem
      +
      <= >= < >
      equal not-equal
      and or
      |ternary-?:|
      ;;? |:|
      ))

(defparameter *infix-ops-priority*
  '((not)
    (* / rem)
    (+ -)
    (< > <= >=)
    (equal  not-equal)
    (and not)
    (or)))

(defun reduce-infix (infix)
  "Find the highest-precedence operator in INFIX and reduce accordingly."
  (labels ((is-operator (obj opname)
             (and (typep obj 'operator)
                  (eql (op-name obj) opname)))
           (op-predicate (opname)
             (alexandria:named-lambda is-operator-predicate (item)
               (is-operator item opname))))
    ;; - (unary)
    (let ((op (first infix)))
      (when (and (is-operator op '-)
                 (null (op-args op)))
        (setf (slot-value op 'args)
              (list (second infix)))
        (return-from reduce-infix 
          (cons op (cddr infix)))))

    ;; ?: ternary
    (let* ((pos1 (position-if (op-predicate '?) infix))
           (pos2 (if pos1
                     (position-if (op-predicate '|:|) infix :start pos1)))
           (len (if pos2 (length infix))))
      (when len
          (return-from reduce-infix
            (list
             (make-instance 'operator
                            :name 'if
                            :args (list (->prefix (subseq infix 0 pos1))
                                        (->prefix (subseq infix (1+ pos1) pos2))
                                        (->prefix (subseq infix (1+ pos2)))))))))

     ;; binary operators
     (iter (for ops in *infix-ops-priority*)
           (iter (for tail-infix on infix)
                 (for item = (first tail-infix))
                 (for prev previous item)
                 (for pos from 0)
                 (when (and (typep item 'operator)
                            (null (op-args item))
                            (member (op-name item) ops))
                   (cond
                     ((eql (op-name item) 'not)
                      (setf (slot-value item 'args)
                            (list (second tail-infix)))
                      (return-from reduce-infix
                        (replace-subseq infix pos 2 item)))
                     (t
                      (setf (slot-value item 'args)
                            (list prev
                                  (second tail-infix)))
                      (return-from reduce-infix
                        (replace-subseq infix (- pos 1) 3 item)))))))))
                        
(defun ->prefix (infix)
  "Convert an infix expression to prefix."
  (iter (unless (> (length infix) 1)
          (return (first infix)))
        (setf infix (reduce-infix infix))))

(define-rule expression (and (? (or - not)) expression-part (* (and operator expression-part)))
  (:destructure (u expr rest)
    (let ((infix (alexandria:flatten (cons expr rest))))
      (->prefix (if u
                    (cons u infix)
                    infix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (text)
  (closure-template-parse 'expression text))
