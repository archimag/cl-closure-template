;;;; translate.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defvar *template-output*)

(defgeneric translate-item (backend item))

(defgeneric translate-named-item (backend item args))

(defgeneric backend-print (backend expr))

(defgeneric translate-expression (backend expr))

(defgeneric translate-template (backend template))

(defgeneric compile-template (backend template))



(defmethod translate-template (backend template)
  (translate-item backend
                  (parse-template template)))

(defmethod translate-item (backend (item cons))
  (cond
    ((symbolp (car item)) (translate-named-item backend
                                                (car item)
                                                (cdr item)))
    ((= (length item) 1) (translate-item backend
                                         (car item)))
    (t (cons 'progn
            (iter (for i in item)
                  (let ((c (translate-item backend
                                         i)))
                    (when c
                      (collect c))))))))

(defmethod translate-item (backend (item symbol))
  nil)

(defmethod translate-item (backend (item string))
  (backend-print backend
                 item))

(defmethod translate-named-item (backend (item (eql 'closure-template.parser:print-tag)) args)
  (backend-print backend
                 (translate-expression backend (car args))))

