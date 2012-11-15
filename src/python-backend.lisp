;;;; python-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Andrey Kutejko <andy128k@gmail.com>

(in-package #:closure-template)

(defvar *gen-name-index* 0)

(defun gen-name ()
  (format nil "gn~A" (incf *gen-name-index*)))

(defun indent (lines)
  (mapcar (lambda (line)
	    (concatenate 'string "    " line))
	  (flatten lines)))

(defun string-to-class-name (str)
  (coerce (iter (with force-upper = t)
		(for ch in-string str)
		(cond
		  ((alphanumericp ch)
		   (collect (if force-upper
				(char-upcase ch)
				ch))
		   (setf force-upper nil))
		  (t
		   (setf force-upper t))))
	  'string))

(defun string-to-method-name (str)
  (coerce (iter (with omit-underline = t)
		(for ch in-string (string str))
		(when (and (not omit-underline)
			   (or (upper-case-p ch)
			       (not (alphanumericp ch))))
		  (collect #\_)
		  (setf omit-underline t))
		(when (alphanumericp ch)
		  (collect (char-downcase ch))
		  (setf omit-underline nil)))
	  'string))

;;; TODO: replace ''' to \'\'\'
(defun python-escape (str)
  (concatenate 'string "'''" str "'''"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun python-function (symbol args)
  (case symbol
    (getf
     (format nil "get_attr_or_item(~A, ~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    (elt
     (format nil "~A[~A]"
	     (python-expression (first args))
	     (python-expression (second args))))
    (+
     (format nil "(~A) + (~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    (/
     (format nil "(~A) / (~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    (or
     (format nil "(~A) or (~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    (and
     (format nil "(~A) and (~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    (not
     (format nil "not (~A)"
	     (python-expression (first args))))
    (:round
     (format nil "round(~{~A,~})"
	     (mapcar #'python-expression args)))
    (equal
     (format nil "(~A) == (~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    ((:not-equal not-equal)
     (format nil "(~A) != (~A)"
	     (python-expression (first args))
	     (python-expression (second args))))
    (:random-int
     (format nil "random_int(~{~A,~})"
	     (python-expression (first args))))
    (:length
     (format nil "len(~{~A,~})"
	     (mapcar #'python-expression args)))
    (otherwise
     (format nil "~A(~{~A,~})" symbol
	     (mapcar #'python-expression args)))))

(defun python-expression (expr)
  (cond
    ((and (consp expr) (symbolp (car expr)))
     (case (car expr)
       (:variable
	(format nil "env['~A']" (string-downcase (second expr))))
       (:index
	(format nil "index(~A)" (second (second expr))))
       (:is-first
	(format nil "is_first(~A)" (second (second expr))))
       (:is-last
	(format nil "is_last(~A)" (second (second expr))))
       (if
	(format nil "(~A if ~A else ~A)" 
		(python-expression (second (cdr expr)))
		(python-expression (first (cdr expr)))
		(python-expression (third (cdr expr)))))
       (otherwise
        (python-function (car expr)
			 (cdr expr)))))
    ((stringp expr)
     (python-escape expr))
    ((numberp expr)
     expr)
    ((symbolp expr)
     (python-escape (string-downcase (string expr))))
    (t
     (error "Bad expression ~A" expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; template command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun python-code-block (expr)
  (iter (for cmd in expr)
	(collect
	 (cond
	   ((stringp cmd)
	    (format nil "out.write(~A)" (python-escape cmd)))
	   ((consp cmd)
	    (case (car cmd)
	      (closure-template.parser:print-tag
	       (python-print-tag (second cmd) (cddr cmd)))
	      (closure-template.parser:literal
	       (format nil "out.write(~A)" (python-escape (second cmd))))
	      (closure-template.parser:if-tag
	       (python-if cmd))
	      (closure-template.parser:switch-tag
	       (python-switch cmd))
	      (closure-template.parser:foreach
	       (python-foreach cmd))
	      (closure-template.parser:for-tag
	       (python-for cmd))
	      (closure-template.parser:with
	       (python-with cmd))
	      (closure-template.parser:call
	       (python-call cmd))
	      (closure-template.parser:comment
	       nil)))))))

;;;; print-tag

(defun python-print-tag (code mode)
  (let ((expr (python-expression code))
	(escape-mode (cond
		       ((getf mode :no-autoescape) :no-autoescape)
		       ((getf mode :id) :id)
		       ((getf mode :escape-html) :escape-html)
		       ((getf mode :escape-uri) :escape-uri)
		       ((getf mode :escape-js) :escape-js)
		       (*autoescape* :escape-html)
		       (t :no-autoescape))))
    (case escape-mode
      (:no-autoescape
       (concatenate 'string "out.write(" expr ")"))
      (:id
       (concatenate 'string "out.write(encode_uri_component(" expr "))"))
      (:escape-uri
       (concatenate 'string "out.write(encode_uri(" expr "))"))
      (:escape-html
       (concatenate 'string "out.write(escape_html(" expr "))")))))

;;;; if

(defun python-if (cmd)
  (iter (for (condition code) in (cdr cmd))
	(for statement
	     first "if ~A:"
	     then (if (eq t condition)
		      "else:"
		      "elif ~A:"))
	(collect (list
		  (funcall #'format nil statement (python-expression condition))
		  (indent (python-code-block code))))))

;;;; switch

(defun python-switch (cmd)
  (let ((val (gen-name)))
    (list
     (format nil "~A = ~A" val (python-expression (second cmd)))
     (iter (for clause in (cdddr cmd))
	   (for statement first "if" then "elif")
	   (collect (list
		     (format nil "~A ~A in (~{~A, ~}):"
			     statement
			     val
			     (mapcar #'python-expression (first clause)))
		     (indent (python-code-block (second clause))))))
     (when (third cmd)
       "else:"
       (indent (python-code-block (third cmd)))))))

;;;; foreach

(defun python-foreach (cmd)
  (let ((varname (string-downcase (second (first (second cmd)))))
	(expr (python-expression (second (second cmd))))
	(body (python-code-block (third cmd)))
	(empty (python-code-block (fourth cmd)))
	(seq (gen-name))
	(item (gen-name)))
    (list
     (format nil "~A = ~A" seq expr)
     (format nil "if ~A:" seq)
     (format nil "    env['%foreach%'] = env.get('%foreach%', []) + [[0, len(~A)]]" seq)
     (format nil "    for ~A in ~A:" item seq)
     (format nil "        env['~A'] = ~A" varname item)
     (indent (indent body))
     "        env['%foreach%'][-1][0] += 1"
     (format nil "    del env['~A']" varname)
     "    env['%foreach%'].pop()"
     (when empty
       (list "else:"
	     (indent empty))))))

;;;; for

(defun python-for (cmd)
  (let ((varname (string-downcase (second (first (second cmd)))))
        (range (mapcar #'python-expression (cdr (second (second cmd)))))
        (body (python-code-block (cddr cmd)))
	(v (gen-name)))
    (list
     (format nil "for ~A in xrange(~A, ~A, ~A):"
	     v
	     (if (second range)
		 (first range)
		 0)
	     (or (second range)
		 (first range))
	     (or (third range)
		 1))
     (format nil "    env['~A'] = ~A" varname v)
     (indent body)
     (format nil "del env['~A']" varname))))

;;;; with

(defun python-with (cmd)
  (let ((ne (gen-name)))
    (list
     (format nil "~A = env" ne)
     "env = env.copy()"
     (iter (for item in (second cmd))
	   (collect
	    (format nil "env['~A'] = ~A"
		    (string-downcase (second (first item)))
		    (python-expression (second item)))))
     (python-code-block (third cmd))
     (format nil "env = ~A" ne))))

;;;; call

(defun python-call (cmd)
  (destructuring-bind (name data &rest params) (cdr cmd)
    (let ((env (gen-name)))
      (list
       (case data
	 (:all
	  (format nil "~A = env.copy()" env))
	 ((nil)
	  (format nil "~A = {}" env))
	 (otherwise
	  (format nil "~A = ~A" env (python-expression data))))
       
       (iter (for param in params)
	     (for param-name = (string-downcase (second (second param))))
	     (collect
	      (if (third param)
		  (format nil "~A['~A'] = ~A"
			  env
			  param-name
			  (python-expression (third param)))
		  (let ((fn (gen-name)))
		    (list
		     (format nil "def ~A(env):" fn)
		     (indent (list
			      "out = StringIO.StringIO()"
			      (python-code-block (cdddr param))
			      "return out.getvalue()"))
		     (format nil "~A['~A'] = ~A(env)"
			     env
			     param-name
			     fn))))))
       
       (if (stringp name)
	   (format nil "cls.~A(~A, out)" (string-to-method-name name) env)
	   (format nil "getattr(cls, ~A)(~A, out)" (python-expression name) env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun python-compile-template (tmpl class-name)
  (destructuring-bind (name &key (autoescape t)) (second tmpl)
    (let ((method-name (string-to-method-name name)))
      (list*
       (format nil "def ~A(cls, env, out):" method-name)
       (indent (let ((*autoescape* autoescape))
		 (python-code-block (cddr tmpl))))
       (format nil "~A.~A = classmethod(~A)" class-name method-name method-name)))))

(defun compile-python-module (data)
  (let* ((class-name (if (second data)
			 (string-to-class-name (second data))
			 "ClosureTemplate.Share"))
	 (lines (list
		 "import StringIO"
		 "from ClosureTemplate import *"
		 "try:"	
		 (format nil "    ~A" class-name)
		 "except NameError:"
		 (format nil "    class ~A(object):" class-name)
		 "        pass"
		 (iter (for tmpl in (cddr data))
		       (collect (python-compile-template tmpl class-name))))))
    (with-output-to-string (out)
      (iter (for line in (flatten lines))
	    (format out "~A~%" line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass python-backend () ())

(defmethod compile-template ((backend (eql :python-backend)) template)
  (compile-template (make-instance 'python-backend)
                    template))

(defmethod compile-template ((backend python-backend) template)
  (compile-python-module (parse-template template)))

(defmethod compile-template ((backend python-backend) (templates list))
  (with-output-to-string (out)
    (iter (for tmpl in templates)
	  (format out "~A~%" (compile-template backend tmpl)))))

