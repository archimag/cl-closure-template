;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defvar *template-variables* nil)

(defvar *local-variables* nil)

(defun write-template-string (str)
  (write-string str *template-output*))


(defun make-template-package (&optional (name "CLOSURE-TEMPLATE.SHARE") &aux (upname (string-upcase name)))
  (or (find-package upname)
      (eval `(defpackage ,(if name
                              upname
                              "CLOSURE-TEMPLATE.SHARE")
               (:use #:cl)
               (:import-from #:closure-template #:write-template-string #:*template-output*)))))


(defparameter *default-translate-package*
  (make-template-package))

(defclass common-lisp-backend () ())


(defmethod translate-expression ((backend common-lisp-backend) expr)
  (flet ((symbol-from-key (key)
           (or (find-symbol (symbol-name key)
                            '#:closure-template)
               (error "Bad keyword ~A" key))))
  (if (and (consp expr)
           (symbolp (car expr)))
      (let ((key (car expr)))
        (case key
          (:variable (let ((varkey (intern (string-upcase (second expr)) :keyword)))
                       (when (not (or (find varkey *local-variables*)
                                      (find varkey *template-variables*)))
                         (push varkey *template-variables*))
                       (intern (string-upcase (second expr)))))
          (otherwise (cons (symbol-from-key (car expr))
                           (iter (for item in (cdr expr))
                                 (collect (translate-expression backend item)))))))
      expr)))


(defmethod backend-print ((backend common-lisp-backend) expr)
  (list 'write-template-string
        expr))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:namespace)) args)
  (let ((*package* (make-template-package (car args))))
    (iter (for tmpl in (cdr args))
          (export (intern (string-upcase (car (second tmpl))))))
    (translate-item backend
                  (cdr args))))
                            

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:template)) args)
  (let* ((*template-variables* nil)
         (body `(with-output-to-string (*template-output*)
                  ,(translate-item backend
                                   (cdr args))))
         (binds (iter (for var in *template-variables*)
                      (collect (list (find-symbol (symbol-name var) *package*)
                                     `(getf ^data^ ,var))))))
  `(defun ,(intern (string-upcase (caar args))) (^data^)
     (let (,@binds)
       ,body))))


(defmethod translate-template ((backend (eql :common-lisp-backend)) template)
  (translate-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend (eql :common-lisp-backend)) template)
  (compile-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend common-lisp-backend) template)
  (eval (translate-template backend template)))
