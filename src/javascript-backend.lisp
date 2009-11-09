;;;; javascript-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(ps:defpsmacro write-template-string (str)
    (setf *template-output*
          (+ *template-output*
             str)))


(defclass javascript-backend () ())

(defmethod translate-expression ((backend javascript-backend) expr)
  (flet ((symbol-from-key (key)
           (or (find-symbol (symbol-name key)
                            '#:closure-template)
               (error "Bad keyword ~A" key))))
  (if (and (consp expr)
           (symbolp (car expr)))
      (let ((key (car expr)))
        (case key
          (:variable (second expr))
          (otherwise (cons (symbol-from-key (car expr))
                           (iter (for item in (cdr expr))
                                 (collect (translate-expression backend item)))))))
      expr)))

(defmethod backend-print ((backend javascript-backend) expr)
  (list 'write-template-string
        expr))

;; (defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:namespace)) args)
;;   (let ((namespace (split-sequence:split-sequence #\. (car args))))
;;     (list (iter (form name in names
;;     (translate-item backend
;;                   (cdr args))))