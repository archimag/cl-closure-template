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
            (:variable (if (find (second expr) *local-variables* :test #'string=)
                           (make-symbol (symbol-name (second expr)))
                           `(ps:@ $data$ ,(make-symbol (string-upcase (second expr))))))
            (getf `(,@(translate-expression backend
                                            (second expr) )
                      ,(make-symbol (string-upcase (third expr)))))
            (otherwise (cons (or (find-symbol (symbol-name key)
                                              '#:closure-template)
                                 (error "Bad keyword ~A" key))
                             (iter (for item in (cdr expr))
                                   (when item
                                     (collect (translate-expression backend item))))))))
          expr)))

(defparameter *js-print-target* '$template-output$)

(defmethod backend-print ((backend javascript-backend) expr)
  (list 'ps:+=
        *js-print-target*
        expr))

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
         (lambda ($$data$$)
           (defvar $data$ (or $$data$$ (ps:create)))
           (defvar $template-output$ "")
           (macrolet ((has-data () '(if $$data$$ t))
                      (round-closure-template (number &optional digits-after-point)
                        `(if ,digits-after-point
                             (let ((factor (expt 10.0 ,digits-after-point)))
                               (/ (round (* ,number factor)) factor))
                             (round ,number))))             
             ,body)
           $template-output$))))

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

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:if-tag)) args)
  "Full copy from common-lisp-backend"
  (cond
    ((= (length args) 1) `(when ,(translate-expression backend
                                                       (first (first args)))
                            ,(translate-item backend
                                             (cdr (first args)))))
    ((and (= (length args) 2)
          (eql (first (second args)) t)) `(if ,(translate-expression backend
                                                                     (first (first args)))
                                              ,(translate-item backend
                                                               (cdr (first args)))
                                              ,(translate-item backend
                                                               (cdr (second args)))))
    (t (cons 'cond
             (iter (for v in args)
                   (collect (list (translate-expression backend
                                                        (first v))
                                  (translate-item backend
                                                  (cdr v)))))))))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:switch-tag)) args)
  `(case ,(translate-expression backend (first args))
     ,@(iter (for clause in (cddr args))
             (collect (list (if (consp (first clause))
                                (iter (for i in (first clause))
                                      (collect (translate-expression backend i)))
                                (first clause))
                            (translate-item backend (cdr clause)))))
     ,@(if (second args) (list (list t
                                     (translate-item backend
                                                     (second args)))))))

(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:for-tag)) args)
  "Full copy from common-lisp-backend"
  (let* ((loop-var (intern (string-upcase (second (first (first args))))))
         (*local-variables* (cons loop-var
                                  *local-variables*))
         (from-expr (translate-expression backend
                                          (second (second (first args)))))
         (below-expr (translate-expression backend
                                           (third (second (first args)))))
         (by-expr (translate-expression backend
                                        (fourth (second (first args))))))
    `(loop
        for ,loop-var from ,(if below-expr from-expr 0) below ,(or below-expr from-expr) ,@(if by-expr (list 'by by-expr))
        do ,(translate-item backend
                            (cdr args)))))


(defmethod translate-named-item ((backend javascript-backend) (item (eql 'closure-template.parser:call)) args)
  (let ((fun-name `(,@*js-namespace* ,(js-string-to-symbol (first args))))
        (data-param (cond
                      ((eql (second args) :all) '$data$)
                      ((second args) (translate-expression backend (second args)))))
        (params (cddr args)))
    (if (not params)
        (backend-print backend
                       (if data-param
                           (list fun-name data-param)
                           (list fun-name)))
        (let ((call-expr '((defvar _$data$_ (ps:create)))))
          (if data-param
              (let ((lvar (gensym "$_")))
                (push `(ps:for-in (,lvar ,data-param)
                                  (setf (ps:@ _$data$_ ,lvar)
                                        (aref ,data-param ,lvar)))
                      call-expr))
          
              (iter (for param in params)
                    (let ((slotname `(ps:@ _$data$_ ,(make-symbol (symbol-name (second (second param)))))))
                      (push `(setf ,slotname "")
                            call-expr)
                      (if (third param)
                          (push `(setf ,slotname
                                       ,(translate-expression backend
                                                              (third param)))
                                call-expr)
                          (let ((*js-print-target* slotname))
                            (push (translate-item backend
                                                  (cdddr param))
                                  call-expr))))))
          `(progn ,@(reverse call-expr)
                  ,(backend-print backend
                                  (list fun-name '_$data$_)))))))

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
