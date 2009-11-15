;;;; javascript-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

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
            (rem (cons 'ps:% (translate-expression backend
                                                   (cdr expr))))
            (:round (translate-expression backend
                                        (cons 'round-closure-template
                                              (cdr expr))))
            (:variable `(ps:@ $data$ ,(make-symbol (string-upcase (second expr)))))
            (otherwise (cons (or (find-symbol (symbol-name key)
                                              '#:closure-template)
                                 (error "Bad keyword ~A" key))
                             (iter (for item in (cdr expr))
                                   (when item
                                     (collect (translate-expression backend item))))))))
          expr)))

(defmethod backend-print ((backend javascript-backend) expr)
  `(setf $template-output$
         (+ $template-output$
            ,expr)))

(defparameter *default-js-namespace* '(ps:@ *closurte-template *share))

(defparameter *check-js-namespace* t)

(defvar *js-namespace*)

(defun js-string-to-symbol (str)
  (make-symbol (coerce (iter (for ch in-string str)
                             (when (upper-case-p ch)
                               (collect #\-))
                             (collect (char-upcase ch)))
                       'string)))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:namespace)) args)
  (let* ((*js-namespace* (if (car args)
                          (cons 'ps:@
                                (mapcar #'js-string-to-symbol
                                        (split-sequence:split-sequence #\. (car args))))
                          *default-js-namespace*)))
    (concatenate 'list
                 (if *check-js-namespace*
                     (nreverse (iter (for i from (length *js-namespace*) downto 2 )
                                     (collect (let ((part (subseq *js-namespace* 0 i)))
                                                `(unless ,part
                                                   (setf ,part (ps:create))))))))
                 (iter (for fun in (cdr args))
                       (collect (translate-item backend
                                                fun))))))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:template)) args)
  (let* ((*template-variables* nil)
         (body (translate-item backend
                               (cdr args))))
    `(setf (,@*js-namespace* ,(js-string-to-symbol (caar args)))
         (lambda ($data$)
           (macrolet ((has-data () '(if $data$ t))
                      (round-closure-template (number &optional digits-after-point)
                        `(if ,digits-after-point
                             (let ((factor (expt 10.0 ,digits-after-point)))
                               (/ (round (* ,number factor)) factor))
                             (round ,number))))
             (setf $template-output$ "")
             ,body
             $template-output$)))))

;; (defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:foreach)) args)
;;   (let* ((loop-var (make-symbol (string-upcase (second (first (first args))))))
;;          (*local-variables* (cons loop-var
;;                                   *local-variables*))
;;          (seq-expr (translate-expression backend (second (first args)))))
;;     (let ((seqvar (gensym "$G")))
;;       `(let ((,seqvar ,seq-expr))
;;          (if ,seqvar
;;                (loop
;;                   for ,loop-var in ,seqvar                  
;;                   do ,(translate-item backend
;;                                       (second args)))
;;              ,(if (third args)
;;                   (translate-item backend
;;                                   (third args))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translate and compile template methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-template ((backend (eql :javascript-backend)) template)
  (translate-template (make-instance 'javascript-backend)
                    template))

(defmethod compile-template ((backend (eql :javascript-backend)) template)
  (compile-template (make-instance 'javascript-backend)
                    template))

(defmethod compile-template ((backend javascript-backend) template)
  (with-output-to-string (out)
    (iter (for i in (translate-template backend template))
          (format out "~A~%" (ps:ps* i)))))
