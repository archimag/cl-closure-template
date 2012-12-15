;;;; common-lisp-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defvar *autoescape* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ttable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ttable*)

(defclass ttable ()
  ((hash :initform (make-hash-table :test 'equal) :reader ttable-hash)
   (prototype :initarg :prototype :initform nil :reader ttable-prototype)))

(defun ttable-clear (ttable)
  (clrhash (ttable-hash ttable)))

(defun ttable-find-template (ttable lname)
  (or (gethash lname (ttable-hash ttable))
      (let ((prototype (ttable-prototype ttable)))
        (if prototype
            (ttable-find-template prototype lname)))))

(defun ttable-register-template (ttable lname template &key supersede)
  (when (and (not supersede) (gethash lname (ttable-hash ttable)))
    (error "Template ~A has already been registered" lname))
  (setf (gethash lname (ttable-hash ttable))
        template))

(defun ttable-call-template (ttable name env &optional out)
  (if out
      (let ((template (ttable-find-template ttable name))
            (*ttable* ttable))
        (unless template
          (error "Template ~A is undefined" template))
        (funcall template env out))
      (let ((*print-pretty* nil))
        (with-output-to-string (out)
          (ttable-call-template ttable name env out)))))

(defun ttable-template-name-list (ttable)
  (sort (if (ttable-prototype ttable)
            (append (hash-table-keys (ttable-hash ttable))
                    (ttable-template-name-list (ttable-prototype ttable)))
            (hash-table-keys (ttable-hash ttable)))
        #'string<))

(defun ttable-clean-package (ttable package)
  (do-external-symbols (symbol package)
    (let ((sname (symbol-name symbol)))
    (unless (or (string= sname "*PACKAGE-TTABLE*")
                (gethash sname (ttable-hash ttable)))
      (unintern symbol package)))))

(defun ttable-extend-package (ttable package)
  (dolist (name (ttable-template-name-list ttable))
    (unless (find-symbol name package)
      (let ((symbol (ensure-symbol name package)))
        (export symbol package)
        (proclaim (list 'ftype 'function symbol))
        (setf (symbol-function symbol)
              (named-lambda template-handler (&optional env)
                (let ((*print-pretty* nil))
                  (with-output-to-string (out)
                    (ttable-call-template ttable name env out)))))))))

(defun ttable-sync-package (ttable package)
  (ttable-clean-package ttable package)
  (ttable-extend-package ttable package)
  package)

(defun ensure-ttable-package (name &key prototype)
  (or (find-package name)
      (let* ((package (make-package name))
             (package-ttable (ensure-symbol "*PACKAGE-TTABLE*" package)))
        (setf (symbol-value package-ttable)
              (make-instance 'ttable :prototype prototype))
        (export package-ttable package)
        package)))

(defun package-ttable (package)
  (symbol-value (find-symbol "*PACKAGE-TTABLE*" (find-package package))))

(defparameter *default-closure-template-package*
  (ensure-ttable-package "CLOSURE-TEMPLATE.SHARE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expression handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %+ (arg1 arg2)
  (if (or (stringp arg1)
          (stringp arg2))
      (format nil "~A~A" arg1 arg2)
      (+ arg1 arg2)))

(defun %round (number &optional digits-after-point)
  (if digits-after-point
      (let ((factor (expt 10.0 digits-after-point)))
        (/ (round (* number factor)) factor))
      (round number)))

(defun %!= (arg1 arg2)
  (not (equal arg1 arg2)))

(defgeneric %same-name (s1 s2)
  (:method ((s1 symbol) (s2 symbol))
    (string-equal (symbol-name s1) (symbol-name s2)))
  (:method ((s1 string) (s2 symbol))
    (string-equal s1 (symbol-name s2)))
  (:method ((s1 symbol) (s2 string))
    (string-equal (symbol-name s1) s2))
  (:method ((s1 string) (s2 string))
    (string-equal s1 s2))
  (:method (s1 (s2 number))
    (%same-name s1 (write-to-string s2)))
  (:method ((s1 number) s2)
    (%same-name (write-to-string s1) s2))
  (:method (s1 s2)
    nil))

(defgeneric %nonblank (el)
  (:method ((el string))
    (unless (every (lambda (ch) (char= ch #\Space)) el) el))
  (:method ((el (eql :null)))   ; postmodern uses this for NULL fields
    nil)
  (:method ((el (eql 0)))
    nil)
  (:method ((el (eql 0.0)))
    nil)
  (:method ((el (eql nil)))
    nil)
  (:method ((el array))
    (< 0 (length el)))
  (:method (el)
    el))

(defgeneric fetch-property (map key)
  (:method ((map hash-table) key)
    (multiple-value-bind (val found) (gethash key map)
      (if (not found)
          (gethash (symbol-name key) map)
          val)))
  (:method ((map list) key)
    (if (listp (car map))
        (cdr (assoc key map :test #'%same-name))
        (cadr (member key map :test #'%same-name))))
  (:method ((obj standard-object) key)
    (iter (for slot in (closer-mop:class-slots (class-of obj)))
          (for name = (closer-mop:slot-definition-name slot))
          (when (and (slot-boundp obj name)
                     (%same-name name key))
            (return (slot-value obj name))))))

(defgeneric make-expression-handler (expr)
  (:documentation "Make expression handler")
  (:method (expr)
    (constantly expr)))

;; variable

(defmethod make-expression-handler ((var closure-template.parser:var))
  (named-lambda variable-handler (env)
    (fetch-property env (closure-template.parser:var-name var))))

;; dotref

(defmethod make-expression-handler ((dotref closure-template.parser:dotref))
  (let ((hash (make-expression-handler (closure-template.parser:ref-expr dotref)))
        (key (make-expression-handler (closure-template.parser:dotref-name dotref))))
    (named-lambda dot-handler (env)
      (fetch-property (funcall hash env)
                      (funcall key env)))))

;; arref

(defmethod make-expression-handler ((arref closure-template.parser:arref))
  (let ((arr (make-expression-handler (closure-template.parser:ref-expr arref)))
        (position (make-expression-handler (closure-template.parser:arref-position arref))))
    (named-lambda arref-handler (env)
      (elt (funcall arr env)
           (funcall position env)))))

;; fcall

(defvar *user-functions* nil)

(defun find-user-function (name)
  (cdr (assoc name
              *user-functions*
              :test #'%same-name)))

(defun make-function-handler (function args)
  (let ((hargs (mapcar #'make-expression-handler args)))
    (named-lambda function-handler (env)
      (apply function
             (iter (for arg in hargs)
                   (collect (funcall arg env)))))))

(defmethod make-expression-handler ((fcall closure-template.parser:fcall))
  (let ((name (closure-template.parser:fcall-name fcall))
        (args (closure-template.parser:fcall-args fcall)))
    (case name
      ((:is-first :is-last :index)
       (make-foreach-function-handler name
                                      (var-name (car args))))
      (:round
       (make-function-handler #'%round args))
      (:random-int (make-function-handler #'random args))
      (otherwise
       (make-function-handler (or (and (find (symbol-name name)
                                             closure-template.parser::*possible-functions*
                                             :key 'lispify-string
                                             :test #'string=)
                                       (find-symbol (symbol-name name)
                                                    '#:closure-template))
                                  (find-user-function name)
                                  (error "Bad function ~A" name))
                              args)))))

;; operator

(defun make-if-operator-handler (condition success fail)
  (named-lambda if-operator-handler (env)
    (if (%nonblank (funcall condition env))
        (funcall success env)
        (funcall fail env))))

(defun make-or-handler (args)
  (let ((a (first args))
        (b (second args)))
    (named-lambda or-handler (env)
      (or (%nonblank (funcall a env))
          (%nonblank (funcall b env))))))

(defun make-and-handler (args)
  (let ((a (first args))
        (b (second args)))
    (named-lambda and-handler (env)
      (and (%nonblank (funcall a env))
           (%nonblank (funcall b env))))))

(defun make-not-handler (expr)
  (named-lambda not-handler (env)
    (not (%nonblank (funcall expr env)))))

(defmethod make-expression-handler ((obj closure-template.parser:operator))
  (let ((op (closure-template.parser:op-name obj))
        (args (mapcar #'make-expression-handler (closure-template.parser:op-args obj))))
    (case op
      (or  (make-or-handler args))
      (and (make-and-handler args))
      (not (make-not-handler (first args)))
      (if  (make-if-operator-handler (first args)
                                     (second args)
                                     (third args)))
      (otherwise
       (when (eql op '+)
         (setf op #'%+))
       (when (eql op '!=)
         (setf op #'%!=))
       (named-lambda operator-handler (env)
         (apply op
                (iter (for arg in args)
                      (collect (funcall arg env)))))))))

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
    (named-lambda code-block-handler (env out)
      (iter (for command in commands)
            (funcall command env out)))))

(defgeneric make-command-handler (cmd)
  (:method (cmd)
    (constantly cmd)))

;;;; simple string

(defmethod make-command-handler ((cmd string))
  (named-lambda simple-template-string-handler (env out)
    (declare (ignore env))
    (write-template-atom cmd out)))

;;;; literal

(defmethod make-command-handler ((cmd literal))
  (let ((text (literal-content cmd)))
    (named-lambda literal-command-handler (env out)
      (declare (ignore env))
      (write-template-atom text out))))

;;;; print

(defun make-user-print-directive-handler (d d-args expr)
  (if-let (d-handler (gethash d closure-template.parser::*user-print-directives*))
    #'(lambda (env) (funcall d-handler d-args env (funcall expr env)))
    expr))

(defun make-all-user-print-directives-handler (cmd expr)
  (let* ((d (car cmd))
         (d-args (cadr cmd))
         (d-handler (make-user-print-directive-handler d d-args expr)))
    (if (cddr cmd)
        (make-all-user-print-directives-handler (cddr cmd)
                                                d-handler)
        d-handler)))

(defmethod make-command-handler ((cmd closure-template.parser:print-command))
  (let ((expr (make-all-user-print-directives-handler (cdr (closure-template.parser:print-directives cmd))
						      (make-expression-handler (closure-template.parser:print-expression cmd))))
        (escape-mode (let ((props (closure-template.parser:print-directives cmd)))
                       (cond
                         ((getf props :no-autoescape) :no-autoescape)
                         ((getf props :id) :id)
                         ((getf props :escape-html) :escape-html)
                         ((getf props :escape-uri) :escape-uri)
                         ((getf props :escape-js) :escape-js)))))
    (case (or escape-mode
              (if *autoescape* :escape-html :no-autoescape))
      (:no-autoescape (named-lambda no-autoescape-print (env out)
                        (write-template-atom (funcall expr env) out)))
      (:id (named-lambda id-print (env out)
             (write-template-atom (encode-uri-component (funcall expr env))
                                  out)))
      (:escape-uri (named-lambda escape-uri-print (env out)
                     (write-template-atom (encode-uri (funcall expr env))
                                          out)))
      (:escape-html (named-lambda escape-html-print (env out)
                      (write-template-atom (escape-html (funcall expr env))
                                           out))))))
    
;;;; if

(defun make-boolean-expression-handler (expr)
  (let ((expr (make-expression-handler expr)))
    (named-lambda boolean-handler (env)
      (%nonblank (funcall expr env)))))

(defmethod make-command-handler ((cmd closure-template.parser:if-command))
  (let ((clauses (iter (for clause in (closure-template.parser:if-command-options cmd))
                       (collect (cons (make-boolean-expression-handler (first clause))
                                      (make-code-block-handler (second clause)))))))
    (named-lambda if-command-handler (env out)
      (iter (for clause in clauses)
            (when (funcall (car clause) env)
              (funcall (cdr clause) env out)
              (finish))))))
  
;;;; switch

(defmethod make-command-handler ((cmd switch-command))
  (let ((switch-expr (make-expression-handler (switch-expression cmd)))
        (default-handler (make-code-block-handler (switch-default cmd)))
        (clauses (iter (for clause in (switch-cases cmd))
                       (collect (cons (let ((conditions (mapcar #'make-expression-handler (first clause))))
                                        (named-lambda case-condition-handler (env value)
                                          (iter (for item in conditions)
                                                (finding T such-that (equal value
                                                                            (funcall item env))))))
                                      (make-code-block-handler (second clause)))))))
    (named-lambda switch-command-handler (env out)
      (let ((value (funcall switch-expr env)))
        (funcall (or (iter (for clause in clauses)
                           (finding (cdr clause)
                                    such-that (funcall (car clause) env value)))
                     default-handler)
                 env
                 out)))))

(defun make-switch-command-handler (cmd)
  (assert (eq 'closure-template.parser:switch-command (car cmd)))
  (let ((switch-expr (make-expression-handler (second cmd)))
        (default-handler (make-code-block-handler (third cmd)))
        (clauses (iter (for clause in (cdddr cmd))
                       (collect (cons (let ((conditions (mapcar #'make-expression-handler (first clause))))
                                        (named-lambda case-condition-handler (env value)
                                          (iter (for item in conditions)
                                                (finding T such-that (equal value
                                                                            (funcall item env))))))
                                      (make-code-block-handler (second clause)))))))
    (named-lambda switch-command-handler (env out)
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
    (:index
     (named-lambda index-foreach-handler (env)
       (declare (ignore env))
       (index varname)))
    (:is-first
     (named-lambda is-first-foreach-handler (env)
       (declare (ignore env))
       (is-first varname)))
    (:is-last
     (named-lambda is-last-foreach-handler (env)
       (declare (ignore env))
       (is-last varname)))))

(defmethod make-command-handler ((cmd foreach))
  (let ((varname (var-name (foreach-varname cmd)))
        (expr (make-expression-handler (foreach-expression cmd)))
        (body (make-code-block-handler (foreach-code-block cmd)))
        (empty-handler (make-code-block-handler (foreach-if-empty-code cmd))))
    (named-lambda foreach-command-handler (env out)
      (let ((seq (funcall expr env)))
        (cond
          ((or (null seq)
               (= (length seq) 0))
           (funcall empty-handler env out))
          (t (let* ((varinfo (list 0 (1- (length seq))))
                    (*loops-vars* (acons varname varinfo *loops-vars*)))
               (map 'nil
                    (named-lambda foreach-body-handler (item)
                      (funcall body
                               (list* varname item env)
                               out)
                      (incf (first varinfo)))
                    seq))))))))
  
;;;; for

(defmethod make-command-handler ((cmd for-command))
  (let ((varname (var-name (for-varname cmd)))
        (range (mapcar #'make-expression-handler (cdr (for-range cmd))))
        (body (make-code-block-handler (for-code-block cmd))))
    (named-lambda for-command-handler (env out)
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

(defmethod make-command-handler ((cmd with))
  (let ((vars (iter (for item in (with-vars cmd))
                    (collect
                        (cons (var-name (first item))
                              (make-expression-handler (second item))))))
        (body (make-code-block-handler (with-body cmd))))
    (named-lambda with-command-handler (env out)
      (funcall body
               (append (iter (for item in vars)
                             (collect (car item))
                             (collect (funcall (cdr item) env)))
                       env)
               out))))

;;;; call

(defun make-call-data-handler (data)
  (case data
    (:all #'identity)
    ((nil) (constantly nil))
    (otherwise (make-expression-handler data))))

(defun make-param-handler (param)
  (if (second param)
      (make-expression-handler (second param))
      (let ((body (make-code-block-handler (cddr param))))
        (named-lambda full-param-handler (env)
          (let ((*print-pretty* nil))
            (with-output-to-string (out)
              (funcall body env out)))))))

(defmethod make-command-handler ((cmd call))
  (let ((name-expr (make-expression-handler (call-name cmd)))
        (data-expr (make-call-data-handler (call-data cmd)))
        (args (iter (for param in (call-params cmd))
                    (collect
                        (cons (var-name (first param))
                              (make-param-handler param))))))
      (named-lambda call-command-handler (env out)
        (ttable-call-template *ttable*
                              (lispify-string (funcall name-expr env))
                              (append (iter (for arg in args)
                                            (collect (car arg))
                                            (collect (funcall (cdr arg) env)))
                                      (funcall data-expr env))
                              out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *template-data* nil)

(defun has-data ()
  (and *template-data* t))

(defun compile-namespace (obj)
  (let* ((namespace-name (closure-template.parser:namespace-name obj))
         (package (if namespace-name
                      (ensure-ttable-package (string-upcase namespace-name))
                      *default-closure-template-package*))
         (ttable (package-ttable package)))
    (iter (for tmpl in (closure-template.parser:namespace-templates obj))
          (let* ((name (closure-template.parser:template-name tmpl))
                 (autoescape (getf (closure-template.parser:template-properties tmpl) :autoescape t))
                 (handler (let ((*autoescape*  autoescape))
                            (make-code-block-handler (closure-template.parser:template-code-block tmpl)))))
            (ttable-register-template ttable
                                      (lispify-string name)
                                      (named-lambda template-namespace-handler (env out)
                                        (let ((*template-data* env))
                                          (funcall handler env out)))
                                      :supersede t)))
    (ttable-extend-package ttable package)
    package))
                                      
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
  (iter (for tmpl in templates)
        (compile-template backend tmpl)))

(defun compile-cl-templates (templates)
  (compile-template :common-lisp-backend
                    templates))
