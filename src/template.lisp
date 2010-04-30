;;;; template.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.parser)

;;;; utils

(defun discard-parse-element ()
  (error 'wiki-parser:bad-element-condition))

(defun parse-expr-or-discard (expr)
  (handler-case
      (parse-expression expr)
    (error ()
      (discard-parse-element))))

(defun check-simple-variable (expr)
  (unless (and (consp expr)
               (eql :variable (car expr))
               (= (length expr) 2)
               (keywordp (second expr)))
    (discard-parse-element))
  expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; closure template syntax
;;;; see http://code.google.com/intl/ru/closure/templates/docs/commands.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-toplevel-mode (:allowed :baseonly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode comment (0 :all)
  (:special "\\s+//[^\\n]*(?=\\n)"
            "^//[^\\n]*(?=\\n)")
  (:entry "/\\*")
  (:exit "\\*/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; namespace tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun namespace-post-handler (item)
  (list 'namespace
        (string-trim #(#\Space #\Tab) (subseq (second item)
                                              (length "{namespace")
                                              (1- (length (second item)))))))

(define-mode namespace (10 :baseonly)
  (:special "{namespace\\s+[\\w\\.\\-]+\\s*}")
  (:post-handler namespace-post-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; template tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-template-name (str)
  (or (ppcre:register-groups-bind (name params) ("^{template\\s+([\\w-]+)((?:\\s+\\w+=\"\\w+\")*)\\s*}\\s*$" str)
        (cons name
              (iter (for param in (ppcre:all-matches-as-strings "\\w+=\"\\w+\"" params))
                    (let* ((pos (position #\= param))
                           (name (subseq param 0 pos))
                           (value (subseq param (+ pos 2) (1- (length param)))))
                      (collect (intern (string-upcase name) :keyword))
                      (collect (cond
                                 ((string= value "true") t)
                                 ((string= value "false") nil)
                                 (t (discard-parse-element))))))))
      (discard-parse-element)))

(define-mode template (10 :baseonly)
  (:allowed :all)
  (:entry "{template[^}]*}\\s*(?=.*{/template})")
  (:entry-attribute-parser parse-template-name)
  (:exit "\\s*{/template}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; substition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

(define-mode space-tag (20 :all)
  (:single "\\s*{sp}\\s*"))

(define-mode emptry-string (20 :all)
  (:single "\\s*{nil}\\s*"))

(define-mode carriage-return (20 :all)
  (:single "\\s*{\\\\r}\\s*"))

(define-mode line-feed (20 :all)
  (:single "\\s*{\\\\n}\\s*"))

(define-mode tab (20 :all)
  (:single "\\s*{\\\\t}\\s*"))

(define-mode left-brace (20 :all)
  (:single "{lb}"))

(define-mode right-brace (20 :all)
  (:single "{rb}"))

;; (define-mode eol (30 :all)
;;   (:single "\\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; literag tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode literal (10 :all)
  (:entry "{literal}(?=.*{/literal})")
  (:exit "{/literal}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-print-directive (str)
  (cond
    ((string= str "noAutoescape") '(:no-autoescape . t))
    ((string= str "id") '(:id . t))
    ((string= str "escapeHtml") '(:escape-html . t))
    ((string= str "escapeUri") '(:escape-uri . t))
    ((string= str "escapeJs") '(:escape-js . t))
    (t (or (ppcre:register-groups-bind (count) ("^insertWordBreaks:(\\d+)$" str)
             (if count
                 (cons :insert-word-breaks (parse-integer count))))
           nil))))

(defun print-tag-post-handler (obj)
  (or (ppcre:register-groups-bind (expr directives) ("^{(?:print\\s+)?([^\\|]+)((?:\\s*\\|\\s*[\\w:]*)*)\\s*}$" (second obj))
        (list* (car obj)              
               (parse-expr-or-discard expr)
               (alexandria:alist-plist
                (iter (for directive in (cdr (split-sequence:split-sequence #\| directives)))
                      (let ((pd (parse-print-directive (string-trim #(#\Space #\Tab #\Newline #\Return) directive))))
                        (collect pd))))))
      (discard-parse-element)))

(define-mode print-tag (200 :all)
  (:special "{print[^}]*}"
            "{[^}]*}")
  (:post-handler print-tag-post-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; msg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-mode msg (30 :all)
;;   (:entry "{msg[^}]*}(?=.*{/msg})")
;;   (:exit "{/msg}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-if-condition (str)
  (or (ppcre:register-groups-bind (expr) ("^{(?:else)?if\\s+([^}]*)}$" str)
        (parse-expr-or-discard expr))
      (discard-parse-element)))

(defun if-post-handler (item)
  (let* ((e-parts (split-sequence:split-sequence 'else-tag (cddr item))))
    (when (or (> (length e-parts) 2)
              (find-if (lambda (s)
                         (and (consp s)
                              (eql 'elseif (first s))))
                       (second e-parts)))
      (discard-parse-element))
    (cons 'if-tag
          (iter (with expr = (second item))
                (with body)
                (for i on (first e-parts))
                (if (and (consp (car i))
                         (eql (first (car i))
                              'elseif))
                    (progn
                      (collect (list expr
                                     (nreverse body)))
                      (setf body nil)
                      (setf expr
                            (parse-if-condition (second (car i)))))
                    (push (car i) body))
                (unless (cdr i)
                  (collect (list expr
                                 (nreverse body)))
                  (if (second e-parts)
                      (collect (list t
                                     (second e-parts)))))))))


(define-mode elseif (10 :if)
  (:special "{elseif\\s+[^}]*}"))

(define-mode else-tag (10 :if)
  (:single "{else}"))

(define-mode if-tag (40 :all)
  (:allowed :all elseif else-tag)
  (:entry "{if[^}]*}(?=.*{/if})")
  (:entry-attribute-parser parse-if-condition)
  (:exit "{/if}")
  (:post-handler if-post-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-switch-expression (str)
  (or (ppcre:register-groups-bind (expr) ("^{switch\\s+([^}]*)}$" str)
        (parse-expr-or-discard expr))
      (discard-parse-element)))

(defun switch-post-handler (item)
  (flet ((parse-case-expression (str)
           (or (ppcre:register-groups-bind (expr) ("^{case\\s+([^}]*)}$" str)
                 (cdr (parse-expr-or-discard (format nil "[~A]" expr))))
               (discard-parse-element)))
         (case-p (obj)
           (and (consp obj)
                (eql (first obj) 'case-tag))))
    (let* ((d-parts (split-sequence:split-sequence 'default-tag (cddr item))))
      (when (or (> (length d-parts) 2)
                (find-if #'case-p
                         (second d-parts))
                (not (find-if #'case-p
                              (first d-parts))))
        (discard-parse-element))
      (list* 'switch-tag
             (second item)
             (second d-parts)
             (iter (with keys)
                   (with form)
                   (for i on (first d-parts))
                   (if (and (consp (car i))
                            (case-p (first i)))
                       (progn
                         (when keys
                           (collect (list keys
                                          (nreverse form))))
                         (setf form nil)
                         (setf keys
                               (parse-case-expression (second (car i)))))
                       (when keys
                         (push (car i) form)))
                   (unless (cdr i)
                     (collect (list keys
                                    (nreverse form)))))))))


(define-mode case-tag (10 :switch)
  (:special "{\\s*case[^}]*}"))

(define-mode default-tag (10 :switch)
  (:single "{\\s*default\\s*}"))

(define-mode switch-tag (50 :all)
  (:allowed :all case-tag default-tag)
  (:entry "{switch[^}]*}(?=.*{/switch})")
  (:entry-attribute-parser parse-switch-expression)
  (:exit "{/switch}")
  (:post-handler switch-post-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foreach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-foreach-attributes (str)
  (or (ppcre:register-groups-bind (loop-var list-var) ("^{foreach\\s+([^\\s}]+)\\s*in\\s*([^\\s}]+)\\s*}$" str)
        (list (check-simple-variable (parse-expr-or-discard loop-var))
              (parse-expr-or-discard list-var))) 
      (discard-parse-element)))

(defun foreach-post-handler (item)
  (let ((parts (split-sequence:split-sequence 'ifempty
                                              (cddr item))))
    (when (> (length parts) 2)
      (discard-parse-element))
    (list* (car item)
           (second item)
           parts)))

(define-mode ifempty (10)
  (:single "{ifempty}"))

(define-mode foreach (60 :all)
  (:allowed :all ifempty)
  (:entry "{foreach\\s+[^}]*}(?=.*{/foreach})")
  (:entry-attribute-parser parse-foreach-attributes)
  (:exit "{/foreach}")
  (:post-handler foreach-post-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-for-attributes (str)
  (or (ppcre:register-groups-bind (loop-var expr) ("^{for\\s+([^\\s]+)\\s+in\\s+([^}]*)}$" str)
        (let ((p-expr (parse-expr-or-discard expr)))
          (unless (and (eql :range (car p-expr))
                       (second p-expr)
                       (< (length p-expr) 5))
            (discard-parse-element))
          (list (check-simple-variable (parse-expr-or-discard loop-var))
                p-expr)))
      (discard-parse-element)))

(define-mode for-tag (70 :all)
  (:allowed :all)
  (:entry "{for\\s+[^}]*}(?=.*{/for})")
  (:entry-attribute-parser parse-for-attributes)
  (:exit "{/for}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun entry-parse-param-name (str)
  (or (ppcre:register-groups-bind (name) ("^{param\\s+(.*)\\s*}$" str)
        (check-simple-variable (parse-expr-or-discard (format nil "$~A" name))))
      (discard-parse-element)))

(defun param-post-handler (item)
  (if (and (= (length item) 2)
           (stringp (second item)))
      (or (ppcre:register-groups-bind (name expr) ("^{param\\s+([\\w-]*):\\s*(.*)/}$" (second item))
            (list 'param
                  (check-simple-variable (parse-expr-or-discard (format nil "$~A" name)))
                  (parse-expr-or-discard expr)))
          (discard-parse-element))
      (list* 'param
             (second item)
             nil
             (cddr item))))

(define-mode param (10)
  (:allowed :all)
  (:special "{param\\s+[^}]+/}")
  (:entry "{param\\s+[^}/]+}(?=.*{/param})")
  (:entry-attribute-parser entry-parse-param-name)
  (:exit "{/param}")
  (:post-handler param-post-handler))

(defun parse-call-name-and-data (str)
  (or (ppcre:register-groups-bind (name) ("^{call\\s+([\\w-\\.]+)\\s*/?}$" str)
        (list name
              nil))
      (ppcre:register-groups-bind (name expr) ("^{call\\s+([\\w-\\.]*)\\s+data=\"([^}]+)\"\\s*/?}$" str)
        (list name
              (parse-expr-or-discard expr)))
      (discard-parse-element)))

(defun call-post-handler (item)
  (unless (cdr item)
    (discard-parse-element))
  (if (and (= (length item) 2)
           (stringp (second item)))
      (let ((name-and-data (parse-call-name-and-data (second item))))
        (list 'call
              (first name-and-data)
              (second name-and-data)))
      (cons 'call
            (concatenate 'list
                         (second item)
                         (iter (for param in (cddr item))
                               (if (and (consp param)
                                        (eql 'param (car param)))
                                   (collect param)))))))


(define-mode call (80 :all)
  (:allowed param )
  (:special "{call\\s+[^}^/]+/}")
  (:entry "{call\\s+[^}^/]+}(?=.*{/call})")
  (:entry-attribute-parser parse-call-name-and-data)
  (:exit "{/call}")
  (:post-handler call-post-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-mode css-tag (20 :all)
;;   (:special "{css[^}]*}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-parser)

;; remove-whitespaces

(defun remove-whitespaces (obj)
  (typecase obj
    (string (ppcre:regex-replace-all "\\s{2,}"  (remove #\Newline obj) " "))
    (cons (if (eql (car obj)
                   'literal)
              obj
              (iter (for item in obj)
                    (collect (remove-whitespaces item)))))
    (otherwise obj)))

;; remove-substition

(defparameter *substitions*
  '(emptry-string
    space-tag tab
    line-feed carriage-return
    left-brace right-brace))

(defun substition-to-string (symbol)
  (if (eql symbol 'emptry-string)
      ""
      (string (case symbol
                (space-tag #\Space)
                (carriage-return #\Return)
                (line-feed #\Newline)
                (tab #\Tab)
                (left-brace #\{)
                (right-brace #\})))))

(defun remove-substition (obj)
  (cond
    ((consp obj) (iter (for item in obj)
                       (collect (remove-substition item))))
    ((and (symbolp obj)
          (find obj *substitions*)) (substition-to-string obj))
    (t obj)))

     
(defun concat-neighboring-strings (obj)
  (if (consp obj)
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

;;; simplify-template

(defun simplify-template (obj)
  (list* (first obj)
         (second obj)
         (concat-neighboring-strings (remove-substition (remove-whitespaces (cddr obj))))))
  

(defmethod wiki-parser:parse ((markup (eql :closure-template.parser)) (obj string))
  (simplify-template
   (let* ((res (call-next-method markup obj))
          (namespace (iter (for item in res)
                           (finding (second item)
                                    such-that (and (consp item)
                                                   (eql (car item) 'namespace)))))
          (templates (iter (for item in res)
                           (if (and (consp item)
                                    (eql (car item) 'template))
                               (collect item)))))
     (list* 'namespace
            namespace
            templates))))
           
(defun parse-template (obj)
  (wiki-parser:parse :closure-template.parser obj))

