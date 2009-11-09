;;;;; cl-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defparameter *cl-compile-map* (make-hash-table))

(defvar *template-output-stream*)

(defvar *template-data*)


(defun compile-expression (expr)
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
                                 (collect (compile-expression item)))))))
      expr)))
      

(defun write-template-string (str)
  (write-string str *template-output-stream*))


(defun cl-compile-item (item)
  (cond
    ((and (consp item)
          (symbolp (car item))) (let ((compile-fun (gethash (car item) *cl-compile-map*)))
                                  (if compile-fun
                                      (funcall compile-fun (cdr item))
                                      (cl-compile-item (cdr item)))))
    ((consp item) (cons 'progn
                        (iter (for i in item)
                              (collect (cl-compile-item i)))))
    ((symbolp item) (let ((compile-fun (gethash item *cl-compile-map*)))
                      (if compile-fun
                          (funcall compile-fun nil))))
    ((stringp item) (list 'write-template-string item))))

(defmacro define-cl-compile (name (items) &body body)
  `(setf (gethash ',name
                  *cl-compile-map*)
         (lambda (,items)
           ,@body)))


(define-cl-compile closure-template.parser:template (items)
  `(defun ,(intern (string-upcase (car (car items)))) (data)
     (let ((*template-data* data))
       (with-output-to-string (*template-output-stream*)
         ,(cl-compile-item (cdr (cdr items)))))))

(define-cl-compile closure-template.parser:print-tag (items)
  (list 'write-template-string
        (compile-expression (car items))))
