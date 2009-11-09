;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defun write-template-string (str)
  (write-string str *template-output*))


(defun make-template-package (&optional (name "CLOSURE-TEMPLATE.SHARE") &aux (upname (string-upcase name)))
  (or (find-package upname)
      (eval `(defpackage ,(if name
                              upname
                              "CLOSURE-TEMPLATE.SHARE")
               (:use #:cl)
               (:import-from #:closure-template #:write-template-string)))))


(defvar *template-data*)

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
          (:variable `(getf *template-data* ,(intern (string-upcase (second expr)) :keyword)))
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
  `(defun ,(intern (string-upcase (caar args))) (data)
     (let ((*template-data* data))
       (with-output-to-string (*template-output*)
         ,(translate-item backend
                        (cdr args))))))


(defmethod translate-template ((backend (eql :common-lisp-backend)) template)
  (translate-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend (eql :common-lisp-backend)) template)
  (compile-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend common-lisp-backend) template)
  (eval (translate-template backend template)))
