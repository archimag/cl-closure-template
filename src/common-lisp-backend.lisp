;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

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

(defun != (arg1 arg2)
  (not (equal arg1 arg2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expression handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-variable-handler (varkey)
  (alexandria:named-lambda variable-handler (env)
    (getf env varkey)))

(defun make-if-function-handler (condition success fail)
  (let ((condition-expr (make-expression-handler condition))
        (success-expr (make-expression-handler success))
        (fail-expr (make-expression-handler fail)))
    (alexandria:named-lambda if-function-handler (env)
        (if (funcall condition-expr env)
            (funcall success-expr env)
            (funcall fail-expr env)))))

(defun make-simple-function-hadler (function args)
  (alexandria:named-lambda function-handler (env)
    (apply function
           (iter (for arg in args)
                 (collect (funcall arg env))))))

(defun make-function-handler (symbol args)
  (case symbol
    ((getf elt)
     (make-simple-function-hadler (symbol-function symbol) args))
    (+
     (make-simple-function-hadler #'+/closure-template args))
    
    (:round
     (make-simple-function-hadler #'round/closure-template args))
    
    (:not-equal
     (make-simple-function-hadler (alexandria:named-lambda not-equal (x y)
                                    (not (equal x y)))
                                  args))

    (:random-int
     (make-simple-function-hadler #'random args))

    (otherwise
     (make-simple-function-hadler (or (and (find (symbol-name symbol)
                                                 closure-template.parser::*possible-functions*
                                                 :key 'lispify-string
                                                 :test #'string=)
                                           (find-symbol (symbol-name symbol)
                                                        '#:closure-template))
                                      (find symbol  closure-template.parser::*infix-ops-priority*)
                                      (error "Bad function ~A" symbol))
                                  args))))
    
(defun make-expression-handler (expr)
  (cond
    ((and (consp expr) (symbolp (car expr)))
     (case (car expr)
       (:variable
        (make-variable-handler (second expr)))
       ((:is-first :is-last :index)        
        (make-foreach-function-handler (car expr) (second (second expr))))
       (if
        (make-if-function-handler (first (cdr expr))
                                  (second (cdr expr))
                                  (third (cdr expr))))
       (otherwise
        (make-function-handler (car expr)
                               (mapcar #'make-expression-handler (cdr expr))))))
    ((atom expr)
     (constantly expr))
    (t (error "Bad expression ~A" expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; template command handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-template-atom (obj out)
  (cond
    ((typep obj 'float)
     (let ((*read-default-float-format* (type-of obj)))
       (format out "~A" obj)))
    (obj (format out "~A" obj))))

(defun make-code-block-handler (expr)
  (let ((commands (remove nil (mapcar #'make-command-handler  expr))))
    (alexandria:named-lambda code-block-handler (env out)
      (iter (for command in commands)
            (funcall command env out)))))

(defun make-command-handler (cmd)
  (cond
    ((stringp cmd)
     (alexandria:named-lambda simple-template-string-handler (env out)
       (declare (ignore env))
       (write-template-atom cmd out)))
    ((consp cmd)
     (case (car cmd)
       (closure-template.parser:print-tag
        (make-print-command-handler cmd))
       (closure-template.parser:literal
        (make-literal-command-handler cmd))
       (closure-template.parser:if-tag
        (make-if-command-handler cmd))
       (closure-template.parser:switch-tag
        (make-switch-command-handler cmd))
       (closure-template.parser:foreach
        (make-foreach-command-handler cmd))
       (closure-template.parser:for-tag
        (make-for-command-handler cmd))
       (closure-template.parser:with
        (make-with-command-handler cmd))
       (closure-template.parser:comment nil)))))

;;;; literal

(defun make-literal-command-handler (cmd)
  (assert (eq 'closure-template.parser:literal (car cmd)))
  (let ((text (second cmd)))
    (alexandria:named-lambda literal-command-handler (env out)
      (declare (ignore env))
      (write-template-atom text out))))

;;;; print

(defun make-print-command-handler (cmd)
  (assert (eq 'closure-template.parser:print-tag (car cmd)))
  (let ((expr (make-expression-handler (second cmd)))
        (escape-mode (cond
                       ((getf (cddr cmd) :no-autoescape) :no-autoescape)
                       ((getf (cddr cmd) :id) :id)
                       ((getf (cddr cmd) :escape-html) :escape-html)
                       ((getf (cddr cmd) :escape-uri) :escape-uri)
                       ((getf (cddr cmd) :escape-js) :escape-js))))
    (case (or escape-mode
              (if *autoescape* :escape-html :no-autoescape))
      (:no-autoescape (alexandria:named-lambda no-autoescape-print (env out)
                        (write-template-atom (funcall expr env) out)))
      (:id (alexandria:named-lambda id-print (env out)
             (write-template-atom (encode-uri-component (funcall expr env))
                                  out)))
      (:escape-uri (alexandria:named-lambda escape-uri-print (env out)
                     (write-template-atom (encode-uri (funcall expr env))
                                          out)))
      (:escape-html (alexandria:named-lambda escape-html-print (env out)
                      (write-template-atom (escape-html (funcall expr env))
                                           out))))))
      
;;;; if

(defun make-if-command-handler (cmd)
  (assert (eq 'closure-template.parser:if-tag (car cmd)))
  (let ((clauses (iter (for clause in (cdr cmd))
                       (collect (cons (make-expression-handler (first clause))
                                      (make-code-block-handler (second clause)))))))
    (alexandria:named-lambda if-command-handler (env out)
      (iter (for clause in clauses)
            (when (funcall (car clause) env)
              (funcall (cdr clause) env out)
              (finish))))))

;;;; switch

(defun make-switch-command-handler (cmd)
  (assert (eq 'closure-template.parser:switch-tag (car cmd)))
  (let ((switch-expr (make-expression-handler (second cmd)))
        (default-handler (make-code-block-handler (third cmd)))
        (clauses (iter (for clause in (cdddr cmd))
                       (collect (cons (let ((conditions (mapcar #'make-expression-handler (first clause))))
                                        (alexandria:named-lambda case-condition-handler (env value)
                                            (iter (for item in conditions)
                                                  (finding T such-that (equal value
                                                                              (funcall item env))))))
                                      (make-code-block-handler (second clause)))))))
    (alexandria:named-lambda switch-command-handler (env out)
      (let ((value (funcall switch-expr env)))
        (funcall (or (iter (for clause in clauses)
                           (finding (cdr clause)
                                    such-that (funcall (car clause) env value)))
                     default-handler)
                 env
                 out)))))

;;;; foreach

(defvar *loops-vars* nil)

(defun index (symbol)
  (second (assoc symbol *loops-vars*)))

(defun is-first (symbol)
  (= 0 (index symbol)))

(defun is-last (symbol)
  (let ((var (assoc symbol *loops-vars*)))
    (= (second var)
       (third var))))

(defun make-foreach-function-handler (symbol varname)
  (case symbol
    (:index #'(lambda (env) (declare (ignore env)) (index varname)))
    (:is-first #'(lambda (env) (declare (ignore env)) (is-first varname)))
    (:is-last #'(lambda (env) (declare (ignore env)) (is-last varname)))))


(defun make-foreach-command-handler (cmd)
  (assert (eq 'closure-template.parser:foreach (car cmd)))
  (let ((varname (second (first (second cmd))))
        (expr (make-expression-handler (second (second cmd))))
        (body (make-code-block-handler (third cmd)))
        (empty-handler (make-code-block-handler (fourth cmd))))
    (alexandria:named-lambda foreach-command-handler (env out)
      (let ((seq (funcall expr env)))
        (cond
          ((or (null seq)
               (= (length seq) 0))
           (funcall empty-handler env out))
          (t (let* ((varinfo (list 0 (1- (length seq))))
                    (*loops-vars* (acons varname varinfo *loops-vars*)))
               (map 'nil
                    (alexandria:named-lambda foreach-body-handler (item)                      
                      (funcall body
                               (list* varname item env)
                               out)
                      (incf (first varinfo)))
                    seq))))))))

;;;; for

(defun make-for-command-handler (cmd)
  (assert (eq 'closure-template.parser:for-tag (car cmd)))
  (let ((varname (second (first (second cmd))))
        (range (mapcar #'make-expression-handler (cdr (second (second cmd)))))
        (body (make-code-block-handler (cddr cmd))))
    (alexandria:named-lambda for-command-handler (env out)
      (let ((start (if (second range)
                       (funcall (first range) env)
                       0))
            (end (funcall (or (second range)
                              (first range))
                          env))
            (step (if (third range)
                      (funcall (third range) env)
                      1)))
        (iter (for i from start below end by step)
              (funcall body
                       (list* varname i env)
                       out))))))
  
;;;; with

(defun make-with-command-handler (cmd)
  (assert (eq 'closure-template.parser:with (car cmd)))
  (let ((vars (iter (for item in (second cmd))
                    (collect
                        (cons (second (first item))
                              (make-expression-handler (second item))))))
        (body (make-code-block-handler (third cmd))))
    (alexandria:named-lambda with-command-handler (env out)
      (funcall body
               (append (iter (for item in vars)
                             (collect (car item))
                             (collect (funcall (cdr item) env)))
                       env)
               out))))

;;;; call

;;(list* 'call name data params)

;; (defun make-call-command-hadler (cmd)
;;   (destructuring-bind (name data &rest params)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-namespace-package (&optional (name "closure-template.share"))
  (let ((lispified-name (if (stringp name)
                            (lispify-string name)
                            name)))
    (or (find-package lispified-name)
        (make-package lispified-name
                      :use '(#:cl)))))

(defparameter *default-closure-template-package*
  (make-namespace-package "closure-template.share"))

(defvar *template-data* nil)

(defun has-data (env)
  (declare (ignore env))
  (if *template-data* t))


(defun make-namespace-function (expr package)
  (destructuring-bind (name &key (autoescape t)) (second expr)
    (let ((symbol (intern (lispify-string name) package)))
      (export symbol package)
      (proclaim (list 'ftype 'function symbol))
      (setf (symbol-function symbol)
            (let ((handler (let ((*autoescape*  autoescape))
                             (make-code-block-handler (cddr expr)))))
              (alexandria:named-lambda template-handler (&optional env)
                (let ((*template-data* env))
                  (with-output-to-string (out)
                    (funcall handler env out)))))))))
  

(defun compile-namespace (data)
  (let ((package (if (second data)
                     (make-namespace-package (second data))
                     *default-closure-template-package*)))
    (iter (for tmpl in (cddr data))
          (make-namespace-function tmpl package))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass common-lisp-backend () ())

(defmethod compile-template ((backend (eql :common-lisp-backend)) template)
  (compile-template (make-instance 'common-lisp-backend)
                    template))

(defmethod compile-template ((backend common-lisp-backend) template)
  (compile-namespace (parse-template template)))

(defmethod compile-template ((backend common-lisp-backend) (templates list))
  (map 'nil 'compile-namespace templates))

