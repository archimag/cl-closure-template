;;;; parser.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template.parser)

;;(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *lexer* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *symbols-category* (make-hash-table)))


;;;; utils

(defun parse-arguments/impl (str)
  (let ((args nil))
    (ppcre:do-matches-as-strings (bind "\\w*=\"\\w*\"" str)
      (let ((eq-pos (position #\= bind)))
        (push (intern (string-upcase (subseq bind 0 eq-pos)) :keyword)
              args)
        (push (subseq bind (+ eq-pos 2) (1- (length bind)))
              args)))
    (nreverse args)))

(defun parse-arguments (str)
  (ppcre:register-groups-bind (args) ("^{\\w*\\s*((?:\\s*\\w*=\"\\w*\")*)\\s*}$" str)
    (parse-arguments/impl args)))

(defun parse-name-and-arguments (str)
  (ppcre:register-groups-bind (name args) ("^{\\w*\\s*([\\w-\\.]*)((?:\\s*\\w*=\"\\w*\")*)\\s*}$" str)
    (cons name
          (parse-arguments/impl args))))

(defun make-tag-post-handler (tag)
  (eval `(lambda (obj)
     (ppcre:register-groups-bind (expr) (,(format nil "^{(?:~A )?([^}]*)}$" tag) (second obj))
       (list (car obj)
             (parse-expression expr))))))

;;;; closure template syntax
;;;; see http://code.google.com/intl/ru/closure/templates/docs/commands.html

(define-mode toplevel (0)
  (:allowed :baseonly eol one-line-comment multi-line-comment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode one-line-comment (20 :all)
  (:special "\\n//[^\\n]*(?=\\n)"))

(define-mode multi-line-comment (20 :all)
  (:entry "/\\*")
  (:exit "\\*/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; substition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode space-tag (20 :all)
  (:single "{sp}"))

(define-mode emptry-string (20 :all)
  (:single "{nil}"))

(define-mode carriage-return (20 :all)
  (:single "{\\\r}"))

(define-mode line-feed (20 :all)
  (:single "{\\\n}"))

(define-mode tab (20 :all)
  (:single "{\\\t}"))

(define-mode left-brace (20 :all)
  (:single "{lb}"))

(define-mode right-brace (20 :all)
  (:single "{rb}"))

(define-mode eol (30 :all)
  (:single "\\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; template tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode template (10 :baseonly)
  (:allowed :all)
  (:entry "{template[^}]*}(?=.*{/template})")
  (:entry-attribute-parser parse-name-and-arguments)
  (:exit "{/template}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; literag tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode literal (10 :all)
  (:entry "{literal}(?=.*{/literal})")
  (:exit "{/literal}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tag-post-handler (obj)
  (ppcre:register-groups-bind (expr) ("^{(?:print )?([^}]*)}$" (second obj))
    (list (car obj)
          (parse-expression expr))))


(define-mode print-tag (200 :all)
  (:special "{print[^}]*}"
            "{[^}]*}")
  (:post-handler print-tag-post-handler))

(define-mode msg (30 :all)
  (:entry "{msg[^}]*}(?=.*{/msg})")
  (:entry-attribute-parser parse-arguments)
  (:exit "{/msg}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode elseif (10 :if)
  (:special "{elseif[^}]*}"))

(define-mode else-expr (10 :if)
  (:special "{else}"))

(define-mode if-expr (40 :all)
  (:allowed elseif else-expr)
  (:entry "{if[^}]*}(?=.*{/if})")
  (:exit "{/if}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode case-expr (10 :switch)
  (:special "{\\s*case[^}]*}"))

(define-mode default-expr (10 :switch)
  (:special "{\\s*default\\s*}"))

(define-mode switch-expr (50 :all)
  (:allowed case-expr default-expr)
  (:entry "{switch[^}]*}(?=.*{/switch})")
  (:exit "{/switch}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foreach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode foreach (60 :all)
  (:entry "{foreach[^}]*}(?=.*{/foreach})")
  (:exit "{/foreach}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode for-expr (70 :all)
  (:entry "{for[^}]*}(?=.*{/for})")
  (:exit "{/for}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode call (80 :all)
  (:allowed :all)
  (:entry "{call\\s*\\w*\\s*}(?=.*{/call})")
  (:exit "{/call}")
  (:entry-attribute-parser parse-name-and-arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mode css-expr (20 :all)
  (:special "{css[^}]*}"))


(wiki-parser:remake-lexer 'toplevel)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod wiki-parser:parse ((markup (eql :closure-template.parser)) (obj string))
  (call-next-method))
  ;; (call-next-method markup
  ;;                   (ppcre:regex-replace-all "\\s+"
  ;;                                            obj
  ;;                                            " ")))