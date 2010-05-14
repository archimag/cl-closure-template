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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tokenization: convert a string to a sequence of tokens      
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

(defun lispify-string (str)
  (coerce (iter (for ch in-string str)
		(when (upper-case-p ch)
		  (collect #\-))
		(collect (char-upcase ch)))
	  'string))

(defun lispify-name (str)
  (intern (lispify-string str) :keyword))

(defun make-expression-symbol (string)
  "Convert string to symbol, preserving case, except for AND/OR/NOT/FORALL/EXISTS."
  (cond ((find string '(and or not) :test #'string-equal))
        ((char= #\$ (char string 0)) (list :variable
                                           (lispify-name (subseq string 1))))
        ((char= #\. (char string 0)) (list :dot
                                           (if (not (find-if-not #'digit-char-p string :start 1))
                                               (parse-integer string :start 1)
                                               (lispify-name (subseq string 1)))))
        ((equal string "null") :nil)
        ((equal string "true") :t)
        ((equal string "false") :nil)
        ((equal string "all") :all)
        ((find string *possible-functions* :test #'string-equal) (lispify-name string))
        (t (bad-expression "Bad symbol: ~A" string))))

(defun operator-char-p (x)
  (find x "<=>&^|*/,%!?:"))

(defun parse-operator (string i)
  (let ((j (position-if-not #'operator-char-p string :start i)))
    (values (intern (string-upcase (subseq string i j)))
            j)))

(defun parse-span (string pred i)
  (let ((j (position-if-not pred string :start i)))
    (values (make-expression-symbol (subseq string i j)) j)))

(defun parse-string (string i)
  (let ((j (position #\' string :start i)))
    (if j
        (values (subseq string i j) (1+ j)))))

(defun parse-number (string &key (start 0))
  (multiple-value-bind (i1 pos1) (parse-integer string :start start :junk-allowed t)
    (if (and i1
             (> (length string) pos1)
             (char= (char string pos1) #\.))
        (multiple-value-bind (i2 pos2) (parse-integer string :start (1+ pos1) :junk-allowed t)
          (if i2
              (values (read-from-string (format nil "~A.~A" i1 i2))
                      pos2)
              (values i1 pos1)))
        (values i1 pos1))))

(defun whitespace-char-p (ch)
  (find ch #(#\Space #\Tab #\Newline)))

(defun symbol-char-p (x)
  (or (alphanumericp x)
      (find x "$_")))

(defun string-delimiter-char-p (x) (char= x #\'))

(defun parse-var (string i)
  (let ((j (position-if-not #'symbol-char-p string :start (1+ i))))
    (values (make-expression-symbol (subseq string i j)) j)))


(defun parse-infix-token (string start)
  "Return the first token in string and the position after it, or nil."
  (let* ((i (position-if-not #'whitespace-char-p string :start start))
         (ch (if i (char string i))))
    (cond ((null i) (values nil nil))
          ((find ch "+-~()[]{},") (values (intern (string ch)) (+ i 1)))
          ((operator-char-p ch) (parse-operator string i))
          ((digit-char-p ch) (parse-number string :start i))
          ((char= #\$ ch) (parse-var string i))
          ((char= #\. ch) (parse-var string i))
          ((symbol-char-p ch) (parse-span string #'symbol-char-p i))
          ((string-delimiter-char-p ch) (parse-string string (1+ i)))
          (t (bad-expression "unexpected character: ~C" ch)))))


(defun string->infix (string &optional (start 0))
  "Convert a string to a list of tokens."
  (multiple-value-bind (token i) (parse-infix-token string start)
    (cond ((null token) nil)
          ((null i) (list token))
          (t (cons token (string->infix string i))))))            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Convert infix to prefix notation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *infix-match-ops*
  '(
    (|(| nil match |)|)
    ([ nil match ])
    ))
  
(defparameter *infix-ops* 
  '((*) (/) (% rem)
    (+) (-)
    (<) (>) (<=) (>=)
    (== equal) (!= not-equal)
    (and)
    (not not unary)
    (or)
    (|,|))
  "A list of lists of operators, highest precedence first.")

(defun op-token (op) (first op))
(defun op-name (op) (or (second op) (first op)))
(defun op-type (op) (or (third op) 'BINARY))
(defun op-match (op) (fourth op))

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

(defun replace-subseq (sequence start length new)
  (nconc (subseq sequence 0 start) (list new)
                    (subseq sequence (+ start length))))

(defun reduce-infix (infix)
  "Find the highest-precedence operator in INFIX and reduce accordingly."
  (or
   ;; - (unary)
   (if (eql (first infix) '-)
       (replace-subseq infix 0 2
                       (list '-
                             (second infix))))
   ;; ()
   (let ((pos (position '|(| infix :from-end t)))
     (if pos
         (reduce-matching-op '(|(| nil match |)|) 
                             pos
                             infix)))
   ;; []
   (let ((pos (position '|[| infix :from-end nil)))
     (if pos
         (reduce-matching-op '([ nil match ]) 
                             pos
                             infix)))
   ;; .x 
   (let* ((pos (position-if #'(lambda (i) (and (consp i) (eql (car i) :dot))) infix))
          (op (if pos (elt infix pos))))
     (if (and pos
              (> pos 0))
         (replace-subseq infix (1- pos) 2
                         (list (typecase (second op)
                                 (keyword 'getf)
                                 (integer 'elt)
                                 (otherwise (bad-expression "Bad algoright with dot operation: ~A" infix)))
                               (elt infix (1- pos))
                               (second op)))))
   ;; ?: ternary
   (let* ((pos1 (position '? infix))
          (pos2 (if pos1 (position '|:| infix :start pos1)))
          (left (if pos2 (1+ (or (position '|,| infix :from-end t :end pos1)
                                 -1))))
          (right (if pos2 (or (position '|,| infix :start pos2)
                              (length infix)))))
     (if pos2
         (replace-subseq infix left (- right left)
                         (list 'if
                               (->prefix (subseq infix left pos1))
                               (->prefix (subseq infix (1+ pos1) pos2))
                               (->prefix (subseq infix (1+ pos2) right))))))
   ;; binary and unary
   (let* ((pos (iter (for op in *infix-ops*)
                         (for pos = (position (car op) infix))
                         (finding pos such-that pos)))
          (op (if pos
                  (assoc (elt infix pos) *infix-ops*))))
     (if pos
         (case (op-type op)
           (UNARY (replace-subseq infix pos 2 
                                  (list (op-name op) 
                                        (elt infix (+ pos 1)))))
           (BINARY (replace-subseq infix (- pos 1) 3
                                   (list (op-name op)
                                         (elt infix (- pos 1)) 
                                         (elt infix (+ pos 1))))))))
   ;; bad expression
   (bad-expression "Bad syntax for infix expression: ~S" infix)))

(defun function-symbol-p (x) 
  (and (symbolp x) (not (member x '(and or not ||)))
       (alphanumericp (char (string x) 0))))

(defun function-call-or-variable-p (x)
  (and (consp x)
       (symbolp (car x))))

(defun reduce-matching-op (op pos infix)
  "Find the matching op (paren or bracket) and reduce."
  ;; Note we don't worry about nested parens because we search :from-end
  (let* ((end (position (op-match op) infix :start pos ))
         (len (+ 1 (- end pos)))
         (inside-parens (remove-commas (->prefix (subseq infix (+ pos 1) end)))))
    (cond ((and (> pos 0) ;; handle f[a]
                (= 1 (length inside-parens))
                (eq (op-name op) '[)
                (function-call-or-variable-p (elt infix (- pos 1)))
                )
           (replace-subseq infix (- pos 1) (+ len 1)
                           (list* 'elt (elt infix (- pos 1)) inside-parens)))
          ((not (eq (op-name op) '|(|)) ;; handle  [a,b]
           (replace-subseq infix pos len 
                           (cons 'list inside-parens))) ; {set}
          ((and (> pos 0) ;; handle f(a,b)
                (function-symbol-p (elt infix (- pos 1))))
           (replace-subseq infix (- pos 1) (+ len 1)
                           (cons (elt infix (- pos 1)) inside-parens)))
          (t ;; handle (a + b)
           (assert (= (length inside-parens) 1))
           (replace-subseq infix pos len (first inside-parens))))))
                   
(defun remove-commas (exp)
  "Convert (|,| a b) to (a b)."
  (cond ((eq (op exp) '|,|) (nconc (remove-commas (arg1 exp) )
                                   (remove-commas (arg2 exp))))
        (t (list exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-expression (str)
  (unless str
    (bad-expression "NIL is not expression"))
  (let ((*package* (find-package '#:closure-template.parser.expression)))
    (->prefix str)))
