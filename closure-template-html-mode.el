;;;; closure-template-html-mode.el
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(require 'apropos)
(require 'font-lock)

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
  `(("//.*" . font-lock-comment-face)))

(defvar *closure-template-namespace-keywords*
  `((,(rx "{"
          (group "namespace")
          (1+ space)
          (group (1+ (not space)))
          (0+ space)
          "}")
     (1 closure-template-tag-face)
     (2 font-lock-function-name-face))))

(defvar *closure-template-literal-keywords*
  `((,(rx (or "{literal}" "{/literal}"))
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
          (group (or "/template" "/call" "/param"))
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
     . closure-template-tag-face)))

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
          *closure-template-literal-keywords*
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

;;; Indentation
(setq closure-open "^[ \t]*{[a-z]+")
(setq closure-close "^[ \t]*{/[a-z]+")
(setq closure-tag "^[ \t]*{")
(setq closure-short-com "^[ \t]*//")
(setq closure-star-at-begining "^[ \t]*\\*")
(setq closure-long-com "^[ \t]*/\\*")

(setq default-closure-width 2)

(defun indent-to-left (ind)
  (if (> default-closure-width ind)
      (indent-line-to 0)
    (indent-line-to (- ind default-closure-width))))

(defun indent-to-right (ind)
  (indent-line-to (+ default-closure-width ind)))

(defun closure-short-com ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at closure-short-com) t)))

(defun closure-any-com ()
  (save-excursion
    (beginning-of-line)
    (if (or (closure-short-com)
	    (looking-at closure-star-at-begining)
	    (looking-at closure-long-com)
	    )
	t)))

(defun closure-tag ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at closure-tag) t)))

(defun closure-open ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at closure-open) t)))

(defun closure-close ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at closure-close) t)))

(defun get-to-previouse-non-empty ()
  (while (and (zerop (forward-line -1))
	      (or (looking-at "[ \t]*$")
		  (closure-any-com))))
  (skip-chars-forward " \t"))

(defun indent-closure-open ()
  (let ((ind (save-excursion
	       (get-to-previouse-non-empty)
	       (let ((i (current-column)))
		 (if (= (point-min) (point))
		     nil
		   (if (closure-close)
		       (list 'same i)
		     (list 'rigth i)))))))

    (if ind
	(if (eq (car ind) 'same)
	    (indent-line-to (cadr ind))
	 (indent-to-right (cadr ind)))
      (indent-line-to 0))))

(defun indent-closure-close ()
  (let ((i (save-excursion
	     (get-to-previouse-non-empty)
	     (if (closure-open)
		 (list 'same (current-column))
	       (list 'left (current-column))))))
    (case (car i)
      ('same (indent-line-to (cadr i)))
      ('left (indent-to-left (cadr i))))))

(defun indent-sgml-in-closure ()
  (let ((p (point)))
    (get-to-previouse-non-empty)
    (let ((c (current-column)))
      (cond
       ((closure-open)
	(goto-char p)
       	(indent-to-right c))
       ((closure-close)
	(goto-char p)
       	(indent-to-left c))
       (t (goto-char p)
	  (sgml-indent-line))))))

(defun closure-indent-line ()
  (interactive)
  (cond
   ((closure-open) (indent-closure-open))
   ((closure-close) (indent-closure-close))
   ((closure-any-com) nil)
   (t (indent-sgml-in-closure))))

;;;###autoload
(define-derived-mode closure-template-html-mode html-mode  "closure-template-html"
  "Major mode for editing Closure Templates.
\\{closure-template-html-mode-map}"
  :group 'closure-template-html

  ;; it mainly from sgml-mode font lock setting
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `((closure-template-html-font-lock-keywords
           closure-template-html-font-lock-keywords-1
           closure-template-html-font-lock-keywords-2
           ,(closure-template-html-font-lock-keywords-3))
          nil t nil nil
          (font-lock-syntactic-keywords
           . sgml-font-lock-syntactic-keywords)))

  ;; Setting up syntax recognition
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (setq comment-start "/* "
	comment-end " */"
	comment-start-skip "/\\*[ \n\t]+")
  ;; Setting up syntax table
  (modify-syntax-entry ?* ". 23")
  (modify-syntax-entry ?/ ". 14")
(set (make-local-variable 'indent-line-function) 'closure-indent-line))




(provide 'closure-template-html-mode)

