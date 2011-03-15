;;;; template.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

;; (require :esrap)

;; (defpackage #:closure-template.peg-parser
;;   (:use #:cl #:esrap #:iter))

;; (in-package #:closure-template.peg-parser)

(in-package #:closure-template.parser)

(define-rule whitespace (+ (or #\space #\tab #\newline))
  (:constant " "))

(define-rule alpha-char (alpha-char-p character))

(define-rule alphanumeric (alphanumericp character))

(define-rule single-name (and alpha-char (*  (or alphanumeric #\_ #\-)))
  (:destructure (first rest)
    (concat first rest)))

(defun not-brace (character)
  (not (member character '(#\{ #\} #\Space #\Tab #\Return #\Newline))))

(define-rule simple-text (+ (and (! (and whitespace "//")) (! "/*") (not-brace character)))
  (:lambda (list)
    (ppcre:regex-replace-all "\\s{2,}"
                             (remove #\Newline (concat list))
                             " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; commment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule simple-comment (and  "//" (*  (and (! #\Newline) character)) whitespace)
  (:lambda (list)
    (list 'comment (concat list))))

(define-rule multiline-comment (and (? whitespace) "/*" (*  (and (! "*/") character)) "*/")
  (:lambda (list)
    (list 'comment (concat list))))

(define-rule comment (or simple-comment multiline-comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; substition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; literag tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule literal (and "{literal}" (*  (and (! "{/literal}") character)) "{/literal}")
  (:destructure (start any end)
    (declare (ignore start end))
    (list ' literal (concat any))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if-tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foreach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule short-param (and "{param" whitespace single-name (? whitespace) #\: whitespace expression "/}")
  (:destructure (start w1 name w2 c w3 expr end)
    (declare (ignore start w1 w2 c w3 end))
    (list 'param
          (list :variable (lispify-name name))
          expr)))

(define-rule full-param (and "{param" whitespace single-name (? whitespace) "}"
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

(define-rule call-data (and whitespace "data=\"" expression "\"")
  (:destructure (w1 d expr q)
    (declare (ignore w1 d q))
    expr))

(define-rule call-template-name (or (and "name=\"" expression "\"") single-name)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code-block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
(define-rule code-block (+ (or substition
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule namespace (and "{namespace" whitespace (and single-name (* (and #\. single-name))) (? whitespace) #\})
  (:destructure (start w1 name w2 end)
    (declare (ignore start w1 w2 end))
    (concat name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-expression-p (obj)
  (and (consp obj)
       (or (keywordp (car obj))
           (find (car obj)
                 '(- not + * / rem > < >= <= equal closure-template.parser.expression:not-equal and or if)))))
     
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

(define-rule template (and "{template" whitespace single-name
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
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule toplevel (and (? (and (* (or comment whitespace)) namespace))
                           (* (or comment whitespace template)))
  (:destructure (header items)
    (list* 'namespace
           (second header)
           (iter (for item in items)
                 (when (and (consp item) (eql (car item) 'template))
                   (collect item))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-template (obj)
  (closure-template-parse (format nil
                                  "~%~A"
                                  (typecase obj
                                    (string obj)
                                    (pathname (alexandria:read-file-into-string obj))))
                          'toplevel))