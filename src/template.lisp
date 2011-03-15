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
  (not (or (eql character #\{)
           (eql character #\}))))

(define-rule simple-text (+ (and (! "//") (! "/*") (not-brace character)))
  (:lambda (list)
    (ppcre:regex-replace-all "\\s{2,}"
                             (remove #\Newline (concat list))
                             " ")))

(define-rule code-block (+ (or substition
                               literal
                               if-tag
                               switch
                               foreach
                               for
                               call

                               comment
                               simple-text
                               
                               print-tag
                               ))
  (:lambda (list)
    (if (not (cdr list))
        (car list)
        list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; commment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule simple-comment (and (? whitespace) "//" (*  (and (! #\Newline) character)) (? whitespace))
  (:lambda (list)
    (list 'comment (concat list))))

(define-rule multiline-comment (and (? whitespace) "/*" (*  (and (! "*/") character)) "*/"  (? whitespace))
  (:lambda (list)
    (list 'comment (concat list))))

(define-rule comment (or simple-comment multiline-comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; substition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

(defmacro define-substitions (name str value)
  `(define-rule ,name (and (? whitespace) ,str (? whitespace))
     (:constant ,(if (characterp  value)
                     (string value)
                     value))))

(define-substitions space-tag "{sp}" #\Space)
(define-substitions emptry-string "{nil}" "")
(define-substitions carriage-return "{\\r}" #\Return)
(define-substitions line-feed "{\\n}" #\Newline)
(define-substitions tag "\\t" #\Tab)
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

(define-rule print-directive (and (? whitespace) "|" (or "noAutoescape" "id" "escapeHtml" "escapeUri" "escapeJs") (? whitespace))
  (:destructure (d w1 name w2)
    (declare (ignore d w1 w2))
    (cond
      ((string= name "noAutoescape") :no-autoescape)
      ((string= name"id") :id)
      ((string= name "escapeHtml") :escape-html)
      ((string= name "escapeUri") :escape-uri)
      ((string= name "escapeJs") :escape-js))))

(define-rule print-tag (and (or "{print " "{")  expression (* print-directive) "}")
  (:destructure (start expr directives end)
    (declare (ignore start end))
    (list* 'print-tag
           expr
           (iter (for d in directives)
                 (collect d)
                 (collect t)))))

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

(define-rule case (and "{case" whitespace expression (* (and #\, expression)) "}" code-block)
  (:destructure (start w expr rest end code)
    (declare (ignore start w end))
    (list (cons expr (mapcar #'second rest))
          code)))

(define-rule default (and "{default}" code-block)
  (:destructure (start code)
    (declare (ignore start))
    code))

(define-rule switch (and "{switch" whitespace expression "}" (* case) (? default) "{/switch}")
  (:destructure (start w expr rb cases default end)
    (declare (ignore start w rb end))
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
    (list 'foreach
          (list var expr)
          (if ifempty
              (cons code ifempty)
              (list code)))))

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
    (list 'for-tag
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
           code)))

(define-rule param (and (? whitespace) (or short-param full-param) (? whitespace))
  (:lambda (list)
    (second list)))

(define-rule call-data (and whitespace "data=\"" expression "\"")
  (:destructure (w1 d expr q)
    (declare (ignore w1 d q))
    expr))
                
(define-rule short-call (and "{call" whitespace (or single-name expression) (? call-data) (? whitespace) "/}")
  (:destructure (start w1 name data w2 end)
    (declare (ignore start w1 w2 end))
    (list 'call name data)))

(define-rule full-call (and "{call" whitespace (or single-name expression) (? call-data) (? whitespace) "}"
                            (* param) "{/call}")
  (:destructure (start w1 name data w2 rb params end)
    (declare (ignore start w1 w2 rb end))
    (list* 'call name data params)))

(define-rule call (or short-call full-call))

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
          (list* 'template traits code)
          (list 'template traits)))))
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule toplevel (and (? (and (* (or comment whitespace)) namespace))
                           (* (or comment whitespace template)))
  (:destructure (header items)
    (list* 'toplevel
           (second header)
           (iter (for item in items)
                 (when (and (consp item) (eql (car item) 'template))
                   (collect item))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-template (obj)
  (typecase obj
    (string (closure-template-parse obj 'toplevel))
    (pathname (closure-template-parse (alexandria:read-file-into-string obj) 'toplevel))
    (otherwise "bad obj")))