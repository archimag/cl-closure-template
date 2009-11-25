;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defvar *template-variables* nil)

(defvar *local-variables* nil)

(defvar *loops-vars* nil)

(defun escape-html (str)
  (if (stringp str)
      (with-output-to-string (out)
        (iter (for ch in-string str)
              (case ch
                ((#\<) (write-string "&lt;" out))
                ((#\>) (write-string "&gt;" out))
                ((#\") (write-string "&quot;" out))
                ((#\') (write-string "&#039;" out))
                ((#\&) (write-string "&amp;" out))
                (otherwise (write-char ch out)))))
      str))

(defun escape-string (str not-encode)
  (if (stringp str)
      (with-output-to-string (out)
        (iter (for ch in-string str)
              (if (or (char<= #\0 ch #\9) (char<= #\a ch #\z) (char<= #\A ch #\Z)
                      (find ch not-encode :test #'char=))
                  (write-char ch out)
                  (let ((octets (sb-ext:string-to-octets (string ch) :external-format :utf-8)))
                    (format out "%~2,'0x%~2,'0x" (aref octets 0) (aref octets 1))))))
      str))
  

(defun escape-uri (str)
  (escape-string str "~!@#$&*()=:/,;?+'"))

(defun escape-uri-component (str)
  (escape-string str "~!*()'"))

(defun make-template-package (&optional (name "CLOSURE-TEMPLATE.SHARE") &aux (upname (string-upcase name)))
  (or (find-package upname)
      (eval `(defpackage ,(if name
                              upname
                              "CLOSURE-TEMPLATE.SHARE")
               (:use #:cl)
               (:import-from #:closure-template #:*template-output*)))))


(defparameter *default-translate-package*
  (make-template-package))

(defclass common-lisp-backend () ())

(defun translate-variable (varkey)
  (let ((varsymbol (intern (symbol-name varkey))))
    (when (not (or (find varsymbol *local-variables*)
                   (find varkey *template-variables*)))
      (push varkey *template-variables*))
    varsymbol))

(defun +/closure-template (arg1 arg2)
  (if (or (stringp arg1)
          (stringp arg2))
      (format nil "~A~A" arg1 arg2)
      (+ arg1 arg2)))

(defun round/closure-template (number &optional digits-after-point)
  (if digits-after-point
      (let ((factor (expt 10.0 digits-after-point)))
        (/ (round (* number factor)) factor))
      (round number)))

(defmethod translate-expression ((backend common-lisp-backend) expr)
  (if (and (consp expr)
           (symbolp (car expr)))
      (let ((key (car expr)))
        (case key
          (:variable (translate-variable (second expr)))
          ('+ (translate-expression backend
                                    (cons '+/closure-template
                                          (cdr expr))))
          (:round (translate-expression backend
                                        (cons 'round/closure-template
                                              (cdr expr))))
          (otherwise (cons (or (find-symbol (symbol-name key)
                                            '#:closure-template)
                               (error "Bad keyword ~A" key))
                           (iter (for item in (cdr expr))
                                 (when item
                                   (collect (translate-expression backend item))))))))
      expr))


(defmethod backend-print ((backend common-lisp-backend) expr &optional directives)
  (case (or (getf directives :escape-mode)
            (if *autoescape* :escape-html :no-autoescape))
    (:no-autoescape `(write-template-string ,expr))
    (:escape-id `(write-template-string (escape-uri-component ,expr)))
    (:escape-uri `(write-template-string (escape-uri ,expr)))
    (:escape-html `(write-template-string (escape-html ,expr)))))


(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:namespace)) args)
  (let ((*package* (if (car args)
                       (make-template-package (car args))
                       *default-translate-package*)))
    (iter (for tmpl in (cdr args))
          (export (intern (string-upcase (car (second tmpl))))))
    (translate-item backend
                  (cdr args))))
                            

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:template)) args)
  (let* ((*template-variables* nil)         
         (body (let ((*autoescape* (if (find :autoescape (cdar args))
                                       (getf (cdar args) :autoescape)
                                       *autoescape*)))
                 `(with-output-to-string (*template-output*)
                    ,(translate-item backend
                                     (cdr args)))))
         (binds (iter (for var in *template-variables*)
                      (collect (list (find-symbol (symbol-name var) *package*)
                                     `(getf $data$ ,var))))))
    `(defun ,(intern (string-upcase (caar args))) (,@(unless binds '(&optional)) $data$)
       (let ((*loops-vars* nil) ,@binds)
         (macrolet ((write-template-string (str)
                      `(when ,str
                         (format *template-output* "~A" ,str)))
                    (random-int (arg) `(random ,arg))
                    (has-data () '(not (null $data$)))
                    (index (s) `(second (assoc ',s *loops-vars*)))
                    (is-first (s) `(= 0 (index ,s)))
                    (is-last (s) (let ((var (gensym "$G")))
                                   `(let ((,var (assoc ',s *loops-vars*)))
                                      (= (second ,var)
                                         (third ,var))))))
           ,body)))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:foreach)) args)
  (let* ((loop-var (intern (string-upcase (second (first (first args))))))
         (*local-variables* (cons loop-var
                                  *local-variables*))
         (seq-expr (translate-expression backend (second (first args))))
         (seqvar (gensym "$G")))
    `(let ((,seqvar ,seq-expr))
       (if ,seqvar
           (let ((*loops-vars* (acons ',loop-var (list 0 (1- (length ,seqvar)))
                                      *loops-vars*)))
             (loop
                for ,loop-var in ,seqvar                  
                do ,(translate-item backend
                                    (second args))
                do (incf (index ,loop-var))))
           ,(if (third args)
                (translate-item backend
                                (third args)))))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:literal)) args)
  `(write-template-string ,(car args)))


(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:if-tag)) args)
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

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:switch-tag)) args)
  (let* ((case-var (gensym "$G"))
         (clauses (iter (for clause in (cddr args))
                        (collect `((find ,case-var (list ,@(first clause)) :test #'equal) ,(translate-item backend
                                                                                     (cdr clause)))))))
           
    `(let ((,case-var ,(translate-expression backend
                                             (first args))))
       (cond
         ,@clauses
         ,@(if (second args) (list (list t
                                         (translate-item backend
                                                         (second args)))))))))

(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:for-tag)) args)
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


(defmethod translate-named-item ((backend common-lisp-backend) (item (eql 'closure-template.parser:call)) args)
  (let ((fun-name (or (find-symbol (string-upcase (first args)))
                      (error "Unknow template ~A" (first args)))))
    `(let ((data ,(cond
                   ((eql (second args) :all) '$data$)
                   ((second args) (translate-expression backend
                                                        (second args))))))
       ,@(iter (for param in (cddr args))
               (collect (list 'push
                              (if (third param)
                                  (translate-expression backend
                                                        (third param))
                                  `(with-output-to-string (*template-output*)
                                     ,(translate-item backend
                                                      (cdddr param))))
                              'data))
               (collect (list 'push
                              (intern (string-upcase (second (second param))) :keyword)
                              'data)))
       ,(backend-print backend
                       (list fun-name
                             'data)))))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; translate and compile template methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod translate-template ((backend (eql :common-lisp-backend)) template)
  (translate-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend (eql :common-lisp-backend)) template)
  (compile-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend common-lisp-backend) template)
  (eval (translate-template backend template)))
