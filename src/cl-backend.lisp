;;;;; cl-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defparameter *cl-compile-map* (make-hash-table))

(defvar *template-output-stream*)

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
  (list 'defun
        (intern (string-upcase (car (car items))))
        '()
        (cl-compile-item (cdr (cdr items)))))