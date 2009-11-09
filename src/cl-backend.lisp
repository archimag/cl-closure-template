;;;;; cl-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

;;;(defparameter *cl-compile-map* (make-hash-table))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compile-item (backend item))

(defgeneric compile-named-item (backend item args))

(defgeneric compile-print-function (backend))

(defclass backend () ())

(defmethod compile-item (backend (item cons))
  (if (symbolp (car item))
      (compile-named-item backend
                          (car item)
                          (cdr item))
      (cons 'prong
            (iter (for i in item)
                  (let ((c (compile-item backend
                                         i)))
                    (when c
                      (collect c)))))))

(defmethod compile-item (backend (item symbol))
  nil)

(defmethod compile-item (backend (item string))
  (list 'write-template-string
        item))

(defmethod compile-named-item (backend (item (eql 'closure-template.parser:template)) args)
  `(defun ,(intern (string-upcase item)) (data)
     (let ((*template-data* data))
       (with-output-to-string (*template-output-stream*)
         ,(compile-item backend
                        args)))))

(defmethod compile-named-item (backend (item (eql 'closure-template.parser:print-tag)) args)
  (list 'write-template-string
        (compile-expression args)))
  
