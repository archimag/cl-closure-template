;;;; closure-template-html-mode.el
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(require 'sgml-mode)

(defgroup closure-template-html nil
  "Customizations for `closure-template-html-mode'."
  :prefix "closure-template-html-"
  :group 'closure-template)


(defconst closure-template-html-font-lock-keywords-1
  (append sgml-font-lock-keywords-1)

  "First level keyword highlighting.")

(defconst closure-template-html-font-lock-keywords-2
  (append closure-template-html-font-lock-keywords-1
          sgml-font-lock-keywords-2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; closure-template keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface closure-template-tag
  '((t (:inherit font-lock-type-face :bold t)))
  "`closure-template-html-mode' face used to highlight the tags."
  :group 'closure-template-html)

(defvar closure-template-tag-face 'closure-template-tag)


(defvar *closure-template-comment-keywords*
  `(("//.*" . font-lock-comment-face)
    (,(rx "/*"
          (*? anything)
          "*/")
     . font-lock-comment-face)))

(defvar *closure-template-namespace-keywords*
  `((,(rx "{"
          (group "namespace")
          (1+ space)
          (group (1+ (not space)))
          (0+ space)
          "}")
     (1 closure-template-tag-face)
     (2 font-lock-function-name-face))))

(defvar *closure-template-template-keywords*
  `((,(rx "{"
          (group (or "template" "call" "param"))
          (1+ space)
          (group (1+ (not space)))
          (0+ (: (1+ space) (1+ (not (any space "}")))))
          (0+ space)          
          "}")
     (1 closure-template-tag-face)
     (2 font-lock-function-name-face))
    (,(rx "{"
          (group (or "/template" "/call"))
          "}")
     (1 closure-template-tag-face))))

(defvar *closure-template-variable-keywords*
  `((,(rx "$"
          (group (1+ (or word "."))))
     (1 font-lock-variable-name-face))))
          

(defvar *closure-template-substition-keywords*
  `((,(rx (or "{sp}"
              "{nil}"
              "{\\r}"
              "{\\n}"
              "{\\t}"
              "{lb}"
              "{rb}"))
     . font-lock-constant-face)))

(defvar *closure-template-foreach-keywords*
  `((,(rx "{"
          (group "foreach")
          (1+ space)
          (1+ (not space))
          (1+ space)
          (group "in")
          (1+ space)
          (1+ (not space))
          (0+ space)
          "}")
     (1 closure-template-tag-face)
     (2 closure-template-tag-face))
    (,(rx "{"
         (group (or "/foreach" "ifempty"))
         "}")
     (1 closure-template-tag-face))))

(defvar *closure-template-if-switch-keywords*
  `((,(rx "{"
          (group (or "if" "elseif" "switch" "case"))
          (1+ space)
          (1+ (not space))
          (0+ space)
          "}")
     (1 closure-template-tag-face))
    (,(rx "{"
          (group (or "/if" "else" "/switch" "default"))
          "}")
     (1 closure-template-tag-face))))
     
(defun closure-template-html-font-lock-keywords-3 ()
  (append *closure-template-comment-keywords*
          *closure-template-namespace-keywords*
          *closure-template-template-keywords*
          *closure-template-variable-keywords*
          *closure-template-substition-keywords*
          *closure-template-foreach-keywords*
          *closure-template-if-switch-keywords*

          closure-template-html-font-lock-keywords-1
          closure-template-html-font-lock-keywords-2))

(defvar closure-template-html-font-lock-keywords
  closure-template-html-font-lock-keywords-1)

(defvar closure-template-html-mode-syntax-table
  (let ((closure-template-html-mode-syntax-table (make-syntax-table)))
    closure-template-html-mode-syntax-table)
  "Syntax table for closure-template-html-mode.")


;;;###autoload
(define-derived-mode closure-template-html-mode html-mode  "closure-template-html"
  "Major mode for editing Closure Templates.
\\{closure-template-html-mode-map}"
  :group 'closure-template-html

  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       `((closure-template-html-font-lock-keywords
          closure-template-html-font-lock-keywords-1
          closure-template-html-font-lock-keywords-2
          ,(closure-template-html-font-lock-keywords-3))
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords))))


(provide 'closure-template-html-mode)

