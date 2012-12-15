;;;; command.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.parser)

;;; simple text

(defun simple-text-char-p (character)
  (not (member character '(#\{ #\} #\Space #\Tab #\Return #\Newline))))

(define-rule simple-text (+ (simple-text-char-p character))
  (:text t))

;;;; commment

(define-rule simple-comment (and  "//" (*  (and (! #\Newline) character)) whitespace)
  (:lambda (list)
    (make-instance 'comment
                   :text (text list))))

(define-rule multiline-comment (and  "/*" (*  (and (! "*/") character)) "*/" (? whitespace))
  (:lambda (list)
    (make-instance 'comment
                   :text (text list)
                   :multiline t)))

(define-rule comment (or simple-comment multiline-comment))

(defclass comment ()
  ((text :initarg :text :reader comment-text)
   (multiline :initarg :multiline :initform nil :reader comment-multiline-p)))

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

;;;; literag command

(define-rule literal (and "{literal}" (*  (and (! "{/literal}") character)) "{/literal}")
  (:destructure (start any end)
    (declare (ignore start end))
    (make-instance 'literal
                   :content (text any))))

(defclass literal ()
  ((content :initarg :content :reader literal-content)))

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

(define-rule print-command (and (or "{print " "{")  expression (* print-directive) "}")
  (:destructure (start expr directives end)
    (declare (ignore start end))
    (make-instance 'print-command
                   :expr expr
                   :directives (iter (for (key value) in directives)
                                     (collect key)
                                     (collect value)))))
(defclass print-command ()
  ((expr :initarg :expr :reader print-expression)
   (directives :initarg :directives :reader print-directives)))

(defmacro wrap-user-print-directive (d-symbol-cl-templ d-symbol-orig)
  `(define-rule ,d-symbol-cl-templ (and (? whitespace) "|" (? whitespace) ,d-symbol-orig (? whitespace))
     (:destructure (w1 d w2 d-params w3)
                   (declare (ignore w1 d w2 w3))
                   (list ',d-symbol-cl-templ d-params))))

(defmacro register-print-directive (d-symbol d-handler) ;; TODO user will have to specify the result list transformation according to backend type
  (alexandria:with-gensyms (rl-expr)
    (let ((local-d-symbol (intern (gensym (symbol-name d-symbol)))))
      `(progn
         (wrap-user-print-directive ,local-d-symbol ,d-symbol)
         (with-closure-template-rules
           (let ((,rl-expr (rule-expression (find-rule 'print-directive))))
             (change-rule 'print-directive (append ,rl-expr (list ',local-d-symbol)))))
         (setf (gethash ',local-d-symbol *user-print-directives*) ,d-handler)))))

;;; witch

(define-rule with-variable (and whitespace  simple-name "=\"" expression #\")
  (:destructure (w var e expr q)
    (declare (ignore w e q))
    (list (make-instance 'var
                         :name (lispify-name var)
                         :jsname var)
          expr)))
                   
(define-rule with (and "{with" (+ with-variable) (? whitespace) "}" (? code-block) "{/with}")
  (:destructure (start vars w rb code end)
    (declare (ignore start w rb end))
    (make-instance 'with
                   :vars vars
                   :body code)))

(defclass with ()
  ((vars :initarg :vars :reader with-vars)
   (body :initarg :body :reader with-body)))

;;; if-command

(define-rule else (and "{else}" code-block)
  (:destructure (else code)
    (declare (ignore else))
    (list t code)))

(define-rule elseif (and "{elseif" whitespace expression "}" code-block)
  (:destructure (start w expr end code)
    (declare (ignore start end w))
    (list expr code)))

(define-rule if-command (and "{if" whitespace expression "}" code-block (* elseif) (? else) "{/if}")
  (:destructure (start w expr rb block elseif else end)
    (declare (ignore start w rb end))
    (make-instance 'if-command
                   :options (remove nil
                                    `((,expr ,block) ,@elseif ,@(list else))))))

(defclass if-command ()
  ((options :initarg :options :reader if-command-options)))

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
    (make-instance 'switch-command
                   :expr expr
                   :cases cases
                   :default default)))

(defclass switch-command ()
  ((expr :initarg :expr :reader switch-expression)
   (cases :initarg :cases :reader switch-cases)
   (default :initarg :default :reader switch-default)))

;;; foreach

(define-rule ifempty (and "{ifempty}" code-block)
  (:destructure (start code)
    (declare (ignore start))
    code))

(define-rule foreach (and "{foreach" (+ whitespace) variable (+ whitespace) "in" expression "}" code-block (? ifempty) "{/foreach}")
  (:destructure (start w1 var w2 in expr rb code ifempty end)
    (declare (ignore start w1 w2 in rb end))
    (make-instance 'foreach
                   :varname var
                   :expr expr
                   :code-block code
                   :if-empty-code ifempty)))

(defclass foreach ()
  ((varname :initarg :varname :reader foreach-varname)
   (expression :initarg :expr :reader foreach-expression)
   (codeblock :initarg :code-block :reader foreach-code-block)
   (if-empty-code :initarg :if-empty-code :reader foreach-if-empty-code)))

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
    (make-instance 'for-command
                   :varname var
                   :range range
                   :code-block code)))

(defclass for-command ()
  ((varname :initarg :varname :reader for-varname)
   (range :initarg :range :reader for-range)
   (code-block :initarg :code-block :reader for-code-block)))

;;; call

(define-rule short-param (and "{param" whitespace simple-name (? whitespace) #\: whitespace expression "/}")
  (:destructure (start w1 name w2 c w3 expr end)
    (declare (ignore start w1 w2 c w3 end))
    (list (make-instance 'var
                         :name (lispify-name name)
                         :jsname name)
          expr)))

(define-rule full-param (and "{param" whitespace simple-name (? whitespace) "}"
                             code-block "{/param}")
  (:destructure (start w1 name w2 rb code end)
    (declare (ignore start w1 w2 rb end))
    (list* (make-instance 'var
                          :name (lispify-name name)
                          :jsname name)
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
  (:text t))

(define-rule call-template-name (or (and "name=\"" expression "\"") template-name)
  (:lambda (name)
    (if (consp name)
        (second name)
        name)))
                
(define-rule short-call (and "{call" whitespace call-template-name (? call-data) (? whitespace) "/}")
  (:destructure (start w1 name data w2 end)
    (declare (ignore start w1 w2 end))
    (make-instance 'call
                   :name name
                   :data data)))

(define-rule full-call (and "{call" whitespace call-template-name (? call-data) (? whitespace) "}"
                            (* param) "{/call}")
  (:destructure (start w1 name data w2 rb params end)
    (declare (ignore start w1 w2 rb end))
    (make-instance 'call
                   :name name
                   :data data
                   :params params)))

(define-rule call (or short-call full-call))

(defclass call ()
  ((name :initarg :name :reader call-name)
   (data :initarg :data :initform nil :reader call-data)
   (params :initarg :params :initform nil :reader call-params)))

;;; code-block
        
(define-rule code-block
    (+ (or substition
           literal
           if-command
           switch
           foreach
           for
           call
           with

           comment
           whitespace
           simple-text

           print-command))
  (:lambda (list)
    (simplify-code list)))

;;; template

(defun check-expression-p (obj)
  (and (consp obj)
       (or (keywordp (car obj))
           (find (car obj)
                 '(- not + * / rem > < >= <= equal not-equal and or if)))))
     
(defun text-neighboring-strings (obj)
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
                          (collect (text-neighboring-strings item)))
              (t (collect (text-neighboring-strings item))))
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
  (text-neighboring-strings
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
    (make-instance 'template
                   :name name
                   :properties (concatenate 'list
                                            (if autoescape (list :autoescape (eql (third autoescape) :t)))
                                            (if private  (list :private (eql (third private) :t))))
                   :code-block code)))

(defclass template ()
  ((name :initarg :name :reader template-name)
   (properties :initarg :properties :reader template-properties)
   (code-block :initarg :code-block :reader template-code-block)))
                
;;; namespace

(define-rule namespace-name (and "{namespace" whitespace (and template-name (* (and #\. template-name))) (? whitespace) #\})
  (:destructure (start w1 name w2 end)
    (declare (ignore start w1 w2 end))
    (text name)))

(define-rule namespace (and (? (and (* (or comment whitespace)) namespace-name))
                           (* (or comment whitespace template)))
  (:destructure (header items)
    (make-instance 'namespace
                   :name (second header)
                   :templates (iter (for item in items)
                                    (when (typep item 'template)
                                      (collect item))))))

(defclass namespace ()
  ((name :initarg :name :reader namespace-name)
   (templates :initarg :templates :reader namespace-templates)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse-template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-template (obj)
  (closure-template-parse 'namespace
                          (typecase obj
                            (string obj)
                            (pathname (alexandria:read-file-into-string obj)))))
