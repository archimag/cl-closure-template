;;;; expression.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; This is modified version of the AIMA source code (path/to/aima/logic/algorithms/infix.lisp)
;;;; 
;;;; Author: Peter Norvig
;;;; Author: Moskvitin Andrey

(in-package #:closure-template.parser.expression)

(defun not-equal (obj1 obj2)
  (not (equal obj1 obj2)))

(define-condition bad-expression-condition (simple-error) ())

(defun bad-expression (format-control &rest format-args)
  (error 'bad-expression-condition
         :format-control format-control
         :format-arguments format-args))

(defun op (exp) "Operator of an expression" (if (listp exp) (first exp) exp))
(defun args (exp) "Arguments of an expression" (if (listp exp) (rest exp) nil))
(defun arg1 (exp) "First argument" (first (args exp)))
(defun arg2 (exp) "Second argument" (second (args exp)))

(defun replace-subseq (sequence start length new)
  (nconc (subseq sequence 0 start) (list new)
         (subseq sequence (+ start length))))

(defun operator-char? (x) (find x "<=>&^|*/,%!"))

(defun symbol-char? (x)
  (or (alphanumericp x)
      (find x "$.")))

(defun string-delimiter-char? (x) (char= x #\'))

(defun function-symbol? (x) 
  (and (symbolp x) (not (member x '(and or not ||)))
       (alphanumericp (char (string x) 0))))

(defun whitespace? (ch)
  (find ch #(#\Space #\Tab #\Newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *infix-ops* 
  '((([ list match ]) ({ elts match }) (|(| nil match |)|))
    ((*) (/) (% rem))
    ((+) (-))
    ((<) (>) (<=) (>=) (=) (/=))
    ((not not unary) (~ not unary))
    ((and) (& and) (^ and))
    ((or) (\| or))
    ((== equal) (!= not-equal))
    ((=>))
    ((<=>))
    ((|,|)))
  "A list of lists of operators, highest precedence first.")

(defun op-token (op) (first op))
(defun op-name (op) (or (second op) (first op)))
(defun op-type (op) (or (third op) 'BINARY))
(defun op-match (op) (fourth op))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun ->prefix (infix)
  "Convert an infix expression to prefix."
  (when (stringp infix) (setf infix (string->infix infix)))
  ;; INFIX is a list of elements; each one is in prefix notation.
  ;; Keep reducing (most tightly bound first) until there is only one 
  ;; element left in the list.  Example: In two reductions we go:
  ;; (a + b * c) => (a + (* b c)) => ((+ a (* b c)))ppp
  (loop 
    (when (not (> (length infix) 1)) (RETURN (first infix)))
    (setf infix (reduce-infix infix))))

(defun reduce-infix (infix)
  "Find the highest-precedence operator in INFIX and reduce accordingly."
  (if (eql (first infix) '-)
      (replace-subseq infix 0 2
                      (list '-
                            (second infix)))
      (dolist (ops *infix-ops* (bad-expression "Bad syntax for infix expression: ~S" infix))
        (let* ((pos (position-if #'(lambda (i) (assoc i ops)) infix
                                 :from-end (eq (op-type (first ops)) 'MATCH)))
               (op (when pos (assoc (elt infix pos) ops))))
          (when pos
            (RETURN
              (case (op-type op)
                (MATCH (reduce-matching-op op pos infix))
                (UNARY (replace-subseq infix pos 2 
                                       (list (op-name op) 
                                             (elt infix (+ pos 1)))))
                (BINARY (replace-subseq infix (- pos 1) 3
                                        (list (op-name op)
                                              (elt infix (- pos 1)) 
                                              (elt infix (+ pos 1))))))))))))


(defun reduce-matching-op (op pos infix)
  "Find the matching op (paren or bracket) and reduce."
  ;; Note we don't worry about nested parens because we search :from-end
  (let* ((end (position (op-match op) infix :start pos ))
         (len (+ 1 (- end pos)))
         (inside-parens (remove-commas (->prefix (subseq infix (+ pos 1) end)))))
    (cond ((not (eq (op-name op) '|(|)) ;; handle {a,b} or [a,b]
           (replace-subseq infix pos len 
                           (cons (op-name op) inside-parens))) ; {set}
          ((and (> pos 0)  ;; handle f(a,b)
                (function-symbol? (elt infix (- pos 1))))
           (handle-quantifiers
            (replace-subseq infix (- pos 1) (+ len 1)
                           (cons (elt infix (- pos 1)) inside-parens))))
          (t ;; handle (a + b)
           (assert (= (length inside-parens) 1))
           (replace-subseq infix pos len (first inside-parens))))))
                    
(defun remove-commas (exp)
  "Convert (|,| a b) to (a b)."
  (cond ((eq (op exp) '|,|) (nconc (remove-commas (arg1 exp) )
                                   (remove-commas (arg2 exp))))
        (t (list exp))))

(defun handle-quantifiers (exp)
  "Change (FORALL x y P) to (FORALL (x y) P)."
  (if (member (op exp) '(FORALL EXISTS))
    `(,(op exp) ,(butlast (rest exp)) ,(first (last exp)))
    exp))

;;;; Tokenization: convert a string to a sequence of tokens
      
(defun string->infix (string &optional (start 0))
  "Convert a string to a list of tokens."
  (multiple-value-bind (token i) (parse-infix-token string start)
    (cond ((null token) nil)
          ((null i) (list token))
          (t (cons token (string->infix string i))))))

(defun parse-infix-token (string start)
  "Return the first token in string and the position after it, or nil."
    (let* ((i (position-if-not #'whitespace? string :start start))
           (ch (if i (char string i))))
      (cond ((null i) (values nil nil))
            ((find ch "+-~()[]{},") (values (intern (string ch)) (+ i 1)))
            ;;((operator-char? ch) (parse-span string #'operator-char? i))
            ((operator-char? ch) (parse-operator string i))
            ((find ch "0123456789") (parse-integer string :start i :junk-allowed t))
            ((symbol-char? ch) (parse-span string #'symbol-char? i))
            ((string-delimiter-char? ch) (parse-string string (1+ i)))
            (t (bad-expression "unexpected character: ~C" ch)))))

(defun parse-span (string pred i)
  (let ((j (position-if-not pred string :start i)))
    (values (make-logic-symbol (subseq string i j)) j)))

(defun parse-operator (string i)
  (let ((j (position-if-not #'operator-char? string :start i)))
    (values (intern (string-upcase (subseq string i j)))
            j)))


(defun parse-string (string i)
  (let ((j (position #\' string :start i)))
    (if j
        (values (subseq string i j) (1+ j)))))


(defun make-logic-symbol (string)
  "Convert string to symbol, preserving case, except for AND/OR/NOT/FORALL/EXISTS."
  (cond ((find string '(and or not) :test #'string-equal))
        ((char= #\$ (char string 0)) (cons :variable
                                           (split-sequence:split-sequence #\.
                                                                          (subseq string 1))))
        ((equal string "null") :nil)
        (t (intern (string-upcase string) :keyword))))
            

(defun parse-expression (str)
  (unless str
    (bad-expression "NIL is not expression"))
  (let ((*package* (find-package '#:closure-template.parser.expression)))
    (->prefix (string->infix str))))
