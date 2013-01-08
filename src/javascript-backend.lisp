;;;; javascript-backend.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defparameter *symbol-counter* 1)

(defvar *local-variables* nil)

(defvar *js-namespace* nil
  "Current JavaScript namespace")

(defparameter *indent-level* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-write-parenthesis ((out &optional (parenthesis "()")) &body body)
  `(progn
     (write-char (char ,parenthesis 0) ,out)
     ,@body
     (write-char (char ,parenthesis 1) ,out)))

(defmacro with-local-var ((name) &body body)
  `(let ((*local-variables* (cons ,name *local-variables*)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +json-lisp-escaped-chars+
    '((#\" . #\")
      (#\\ . #\\)
      (#\/ . #\/)
      (#\b . #\Backspace)
      (#\f . #\)
      (#\n . #\Newline)
      (#\r . #\Return)
      (#\t . #\Tab)
      (#\u . (4 . 16)))))

(defun %js (text)
  (with-output-to-string (stream)
    (iter (for ch in-string text)
          (for code = (char-code ch))
          (let ((special (car (rassoc ch +json-lisp-escaped-chars+))))
            (cond
              (special
               (write-char #\\ stream)
               (write-char special stream))
              ((< #x1f code #x7f)
               (write-char ch stream))
              (t
               (let ((esc-width-radix '#.(rassoc-if #'consp +json-lisp-escaped-chars+)))
                 (destructuring-bind (esc . (width . radix)) esc-width-radix
                   (format stream "\\~C~V,V,'0R" esc radix width code)))))))))

(defmacro with-increase-indent (&body body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun write-indent (out)
  (dotimes (i (* 4 *indent-level*))
    (write-char #\Space out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric write-expression (expr out)
  (:documentation "Write expression as JavaScript")
  (:method ((expr string) out)
    (format out "~S" (%js expr)))
  (:method ((expr integer) out)
    (format out "~d" expr))
  (:method ((expr real) out)
    (format out "~f" expr))
  (:method (expr (out (eql nil)))
    (with-output-to-string (stream)
      (write-expression expr stream))))

;;;; dotref

(defmethod write-expression ((expr dotref) out)
  (write-expression (ref-expr expr) out)
  (let ((name (dotref-jsname expr)))
    (cond
      ((typep (ref-expr expr) 'injected-data)
       (format out ".~A" name))
      ((stringp name)
       (with-write-parenthesis (out "[]")
         (write-expression name out)))
      ;;; WTF
      (t
       (format out ".~A" name)))))

;;;; aref

(defmethod write-expression ((expr arref) out)
  (write-expression (ref-expr expr) out)
  (with-write-parenthesis (out "[]")
    (write-expression (arref-position expr) out)))

;;;; injected-data

(defmethod write-expression ((expr injected-data) out)
  (write-string "$ij$" out))

;;;; variable

(defparameter +reserved-js-words+ '("abstract" "assert" "boolean" "break"
                                    "byte" "case" "catch" "char" "class"
                                    "const" "continue" "debugger" "decimal"
                                    "default" "delete" "do" "double" "else"
                                    "ensure" "enum" "event" "export" "extends"
                                    "false" "final" "finally" "float" "for"
                                    "function" "get" "goto" "if" "implements"
                                    "import" "in" "instanceof" "int"
                                    "interface" "internal" "invariant" "let"
                                    "long" "namespace" "native" "new" "null"
                                    "package" "private" "protected" "public"
                                    "require" "return" "sbyte" "set" "short"
                                    "static" "super" "switch" "synchronized"
                                    "this" "throw" "throws" "transient" "true"
                                    "try" "typeof" "uint" "ulong" "use"
                                    "ushort" "var" "void" "volatile" "while"
                                    "with" "yield"))

(defmethod write-expression ((expr var) out)
  (let ((name (var-jsname expr)))
    (if (member name  *local-variables* :test #'string=)
        (write-string name out)
        (if (member name +reserved-js-words+ :test #'string=)
            (format out "$env$['~A']" name)
            (format out "$env$.~A" name)))))

;;;; list

(defmethod write-expression ((expr list-expr) out)
  (format out
          "[~{~A~^, ~}]"
          (iter (for val in (list-expr-values expr))
                (collect (write-expression val nil)))))

;;;; map

(defmethod write-expression ((expr map-expr) out)
  (format out
          "{~{~A~^,~}}"
          (iter (for item in (map-expr-items expr))
                (collect
                    (format nil
                            "~A: ~A"
                            (write-expression (first item) nil)
                            (write-expression (second item) nil))))))

;;;; fcall

(defmethod write-expression ((expr fcall) out)
  (let ((name (fcall-jsname expr))
        (args (fcall-args expr)))
    (flet ((write-simple-funcall (fun-name)
             (write-string fun-name out)
             (with-write-parenthesis (out)
               (write-expression (first args) out)
               (iter (for arg in (cdr args))
                     (write-string ", " out)
                     (write-expression arg out)))))
      (cond                
        ((string= name "isFirst")
         (format out "($counter_~A$ == 0)" (var-jsname (first args))))
        ((string= name "isLast")
         (format out
                 "($counter_~A$ == ($sequence_~A$.length - 1))"
                 (var-jsname (first args))
                 (var-jsname (first args))))
        ((string= name "index")
         (format out "$counter_~A$" (var-jsname (first args))))        
        ((string= name "hasData")
         (format out "($env$ && !~A.$isEmpty$($env$))" *js-namespace*))
        ((string= name "isNonnull")
         (with-write-parenthesis (out)
           (write-expression (first args) out)
           (write-string " !== undefined && " out)
           (write-expression (first args) out)
           (write-string " !== null" out)))
        ((string= name "length")
         (write-expression (first args) out)
         (write-string ".length" out))
        ((string= name "keys")
         (format out
                 "~A.$keys$(~A)"
                 *js-namespace*
                 (write-expression (first args) nil)))
        ((string= name "augmentMap")
         (format out
                 "~A.$augmentMap$(~A, ~A)"
                 *js-namespace*
                 (write-expression (first args) nil)
                 (write-expression (second args) nil)))
        ((string= name "round")
         (write-simple-funcall (format nil "~A.$round$" *js-namespace*)))
        ((string= name "randomInt")
         (write-string "Math.floor" out)
         (with-write-parenthesis (out)
           (write-string "Math.random() * " out)
           (write-expression (first args) out)))
        ((string= name "strContains")
         (with-write-parenthesis (out)
           (write-expression (first args) out)
           (write-string ".indexOf" out)
           (with-write-parenthesis (out)
             (write-expression (second args) out))
           (write-string "!==-1" out)))
        (t
         (write-simple-funcall (format nil "Math.~A" name)))))))

;;;; operator

(defmethod write-expression ((expr operator) out)
  (let ((name (op-name expr))
        (args (op-args expr)))
    (case (length args)
      (1
       (case name
         (-
          (write-string "-" out)
          (write-expression (first args) out))
         (not
          (write-string "!" out)
          (write-expression (first args) out))))
      (2
       (with-write-parenthesis (out)
         (write-expression (first args) out)
         (write-string (case name
                         (and " && ")
                         (or " || ")
                         (rem " % ")
                         (equal " == ")
                         (not-equal " != ")
                         (otherwise (symbol-name name)))
                       out)
         (write-expression (second args) out)))
      (3
       (when (eql name 'if)
         (with-write-parenthesis (out)
           (write-expression (first args) out)
           (write-string " ? " out)
           (write-expression (second args) out)
           (write-string " : " out)
           (write-expression (third args) out)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric write-command (cmd out)
  (:method ((cmd string) out)
    (write-indent out)
    (write-string "$result$.push" out)
    (with-write-parenthesis (out)
      (with-write-parenthesis (out "\"\"")
        (write-string (%js cmd) out)))
    (write-line ";" out))
  (:method ((cmd list) out)
    (iter (for item in cmd)
          (write-command item out))))

;;;; comment

(defmethod write-command ((cmd comment) out)
  ;; no comments
  )

;;;; literal

(defmethod write-command ((cmd literal) out)
  (write-indent out)
  (format out "$result$.push(\"~A\");~&" (%js (literal-content cmd))))

;;;; print

(defmethod write-command ((cmd print-command) out)
  (write-indent out)
  (write-string "$result$.push" out)
  (let ((expr (print-expression cmd))
        (directives (print-directives cmd)))
    (with-write-parenthesis (out)
      (cond
        ((getf directives :id)
         (write-string "encodeURIComponent" out)
         (with-write-parenthesis (out)
           (write-expression expr out)))
        ((getf directives :escape-uri)
         (write-string "encodeURI" out)
         (with-write-parenthesis (out)
           (write-expression expr out)))
        ((or (getf directives :escape-html)
             (and *autoescape*
                  (not (getf directives :no-autoescape))))
         (format out "~A.$escapeHTML$" *js-namespace*)
         (with-write-parenthesis (out)
           (write-expression expr out)))
        (t
         (write-expression expr out))))
    (write-line ";" out)))

;;;; if

(defmethod write-command ((cmd if-command) out)
  (let ((first t))
    (iter (for option in (if-command-options cmd))
          (cond
            (first
             (setf first nil)
             (write-indent out)
             (write-string "if " out)
             (with-write-parenthesis (out)
               (write-expression (first option) out)))
            ((eql (first option) t)
             (write-indent out)
             (write-string "else" out))
            (t
             (write-indent out)
             (write-string "else if " out)
             (with-write-parenthesis (out)
               (write-expression (first option) out))))
          (write-line " {" out)
          (with-increase-indent
            (write-command (second option) out))
          (write-indent out)
          (write-line "}" out))))

;;;; switch

(defmethod write-command ((cmd switch-command) out)
  (write-indent out)
  (write-string "switch " out)
  (with-write-parenthesis (out)
    (write-expression (switch-expression cmd) out))
  (write-line " {" out)

  (iter (for case in (switch-cases cmd))
        (iter (for item in (first case))
              (with-increase-indent
                (write-indent out)
                (write-string "case " out)
                (write-expression item out)
                (write-line ":" out)

                (with-increase-indent
                  (write-command (second case) out)
                  (write-indent out)
                  (write-line "break;" out)))))

  (when (switch-default cmd)
    (with-increase-indent
      (write-indent out)
      (write-line "default:" out)

      (with-increase-indent
        (write-command (switch-default cmd) out)
        (write-indent out)
        (write-line "break;"))))

  (write-indent out)
  (write-line "}" out))

;;;; foreach

(defmethod write-command ((cmd foreach) out)
  (let* ((varname (var-jsname (foreach-varname cmd)))
         (seqname (format nil "$sequence_~A$" varname))
         (counter-name (format nil "$counter_~A$" varname)))

    (write-indent out)
    (format out "var ~A = " seqname)
    (write-expression (foreach-expression cmd) out)
    (write-line ";" out)

    (write-indent out)
    (format out "if (~A) {~&" seqname)

    (with-increase-indent
      (write-indent out)
      (format out
              "for (var ~A = 0; ~A < ~A.length; ++~A) {~&"
              counter-name
              counter-name
              seqname
              counter-name)

      (with-increase-indent
        (write-indent out)
        (format out "var ~A = ~A[~A];~&" varname seqname counter-name)

        (with-local-var (varname)
          (write-command (foreach-code-block cmd) out)))

      (write-indent out)
      (write-line "}" out))

    (write-indent out)
    (write-line "}" out)

    (when (foreach-if-empty-code cmd)
      (write-indent out)
      (write-line "else " out)

      (with-increase-indent
        (write-command (foreach-if-empty-code cmd) out))

      (write-indent out)
      (write-line "}"))))

;;;; for

(defmethod write-command ((cmd for-command) out)
  (let ((varname (var-jsname (for-varname cmd)))
        (range (cdr (for-range cmd))))

    (write-indent out)

    (case (length range)
      (1
       (format out "for (var ~A = 0; ~A < " varname varname)
       (write-expression (first range) out)
       (format out "; ++~A) {~&" varname))
      (2
       (format out "for (var ~A = " varname)
       (write-expression (first range) out)
       (format out "; ~A < " varname)
       (write-expression (second range) out)
       (format out "; ++~A) {~&" varname))
      (3
       (format out "for (var ~A = " varname)
       (write-expression (first range) out)
       (format out "; ~A < " varname)
       (write-expression (second range) out)
       (format out "; ~A += " varname)
       (write-expression (third range) out)
       (write-line ") {" out)))

    (with-increase-indent
      (with-local-var (varname)
        (write-command (for-code-block cmd) out)))

    (write-indent out)
    (write-line "}" out)))

;;;; with

(defmethod write-command ((cmd with) out)
  (write-indent out)
  (write-line "(function () {" out)

  (with-increase-indent
    (let ((*local-variables* *local-variables*))
      (iter (for item in (with-vars cmd))
            (for varname = (var-jsname (first item)))
            (setf *local-variables*
                  (cons varname *local-variables*))
            (write-indent out)
            (format out "var ~A = " varname)
            (write-expression (second item) out)
            (write-line ";" out))
      (write-command (with-body cmd) out)))

  (write-indent out)
  (write-line "})();" out))

;;;; call

(defmethod write-command ((cmd call) out)
  (cond
    ((call-params cmd)
     (write-call-with-params cmd out))
    (t
     (write-call-without-params cmd out))))

(defun write-call-with-params (cmd out)
  (incf *symbol-counter*)
  (let ((new-env-name (format nil "$env_~A$" *symbol-counter*)))

    (let ((data (call-data cmd)))
      (write-indent out)
      (format out "var ~A = " new-env-name)

      (cond
        ((eql data :all)
         (format out "~A.$objectFromPrototype$($env$)" *js-namespace*))
        ((eql data nil)
         (write-string "{}" out))
        (t
         (format out "~A.$objectFromPrototype$" *js-namespace*)
         (with-write-parenthesis (out)
           (write-expression data out))))

      (write-line ";" out))

    (iter (for param in (call-params cmd))
          (for param-name = (var-jsname (first param)))

          ;; short param
          (when (second param)
            (write-indent out)
            (format out "~A.~A = " new-env-name param-name)
            (write-expression (second param) out)
            (write-line ";" out))

          ;; full param
          (when (third param)
            (incf *symbol-counter*)

            (write-indent out)
            (format out "~A.~A = function () {~&" new-env-name param-name)

            (with-increase-indent
              (write-indent out)
              (write-line "var $result$ = [];" out)

              (write-command (third param) out)

              (write-indent out)
              (write-line "return $result$.join('');" out))

            (write-indent out)
            (write-line "}();" out)))

    (write-indent out)
    (write-call-name cmd out)
    (format out "(~A, $result$, $ij$);~&" new-env-name)))

(defun write-call-without-params (cmd out)
  (write-indent out)
  (write-call-name cmd out)
  (with-write-parenthesis (out)
    (let ((data (call-data cmd)))
      (cond
        ((eql data :all)
         (write-string "$env$" out))
        ((eql data nil)
         (write-string "{}" out))
        (t
         (write-expression (call-data cmd) out))))
    (write-string ", $result$, $ij$" out))
  (write-line ";" out))

(defun write-call-name (cmd out)
  (cond
    ((stringp (call-name cmd))
     (format out "~A.~A" (or (call-namespace cmd) *js-namespace*) (call-name cmd)))
    (t
     (write-string *js-namespace* out)
     (with-write-parenthesis (out "[]")
       (write-expression (call-name cmd) out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; namespace/template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-template (tmpl out)
  (format out
          "~&~A.~A = function($env$, $target$, $ij$) {~&"
          *js-namespace*
          (template-name tmpl))

  (let ((*indent-level* 1)
        (*autoescape* (getf (template-properties tmpl) :autoescape t)))
    (write-indent out)
    (write-line "if (!$env$) { $env$ = {}; }" out)
    (write-indent out)
    (write-line "var $result$ = $target$ || [];" out)
    (write-line "" out)

    (write-command (template-code-block tmpl) out)

    (write-line "" out)
    (write-indent out)
    (write-line "if (!$target$) return $result$.join('');" out)
    (write-indent out)
    (write-line "else return null;" out)

    (write-line "};" out)))

(defun write-namespace (namespace out &aux (name (namespace-name namespace)))
  (write-namespace-declaration name out)
  (write-namespace-helpers name out)
  (write-namespace-body name (namespace-templates namespace) out))

(defun write-namespace-helpers (name out)
  (macrolet ((write-function (funname (&optional (args "obj"))  &body body)
               `(progn
                  (write-line "" out)
                  (format out "~A.~A = function (~A) {~&" name ,funname ,args)
                  ,@body
                  (write-line "};" out))))
    ;; hasData
    (write-function "$isEmpty$" ()
      (write-line "    for (var prop in obj) if (obj.hasOwnProperty(prop)) return false;" out)
      (write-line "    return true;" out))

    ;; keys
    (write-function "$keys$" ("map")
      (write-line "    var keys = [];" out)
      (write-line "    for (var prop in map) keys.push(prop);" out)
      (write-line "    return keys;" out))

    ;; augmentMap
    (write-function "$augmentMap$" ("baseMap, additionalMap")
      (format out "    var dict = ~A.$objectFromPrototype$(baseMap);~&" name)
      (write-line "    for (var prop in additionalMap) dict[prop] = additionalMap[prop];" out)
      (write-line "    return dict;" out))

    ;; escapeHTML
    (write-function "$escapeHTML$" ()
      (write-string "    if (typeof obj == \'string\') return String(obj)" out)
      (write-string ".split('&').join('&amp;')" out)
      (write-string ".split( '<').join('&lt;')" out)
      (write-string ".split('>').join('&gt;')" out)
      (write-string ".split('\\\"').join('&quot;')" out)
      (write-string ".split('\\'').join('&#039;')" out)
      (write-line ";" out)
      (write-line "    else return obj;" out))

    ;; round
    (write-function "$round$" ("number, ndigits")
      (write-line "    if (ndigits) {" out)
      (write-line "        var factor = Math.pow(10.0, ndigits);" out)
      (write-line "        return Math.round(number * factor) / factor;" out)
      (write-line "    }" out)
      (write-line "    else return Math.round(number)" out))

    ;; objectFromPrototype
    (write-function "$objectFromPrototype$" ()
      (write-line "    function C () {}" out)
      (write-line "    C.prototype = obj;" out)
      (write-line "    return new C;" out))))

(defun write-namespace-declaration (name out)
  (iter (for pos first (position #\. name) then (position #\. name :start (1+ pos)))
        (for short-name = (subseq name 0 pos))
        (format out "if (typeof ~A === 'undefined') { ~A = {}; }~&" short-name short-name)
        (while pos))
  (values))

(defun write-namespace-body (name templates out)
  (let ((*js-namespace* name))
    (iter (for tmpl in templates)
          (write-line "" out)
          (write-template tmpl out))))

(defgeneric compile-to-js (obj)
  (:method ((obj namespace))
    (with-output-to-string (out)
      (write-namespace obj out)))
  (:method (obj)
    (compile-to-js (parse-template obj))))

(defun compile-to-requirejs (obj)
  ;(:method ((obj namespace))
  (with-output-to-string (out)
    ;(write-namespace obj out)))
    (write-string "define(function () {
var module = { };" out)
    (write-namespace-helpers "module" out)
    (write-namespace-body "module" (namespace-templates (parse-template obj)) out)
    (write-string "return module; });" out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; namespace/template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass javascript-backend () ())

(defclass requirejs-backend (javascript-backend) ())

(defmethod compile-template ((backend (eql :javascript-backend)) template)
  (compile-template (make-instance 'javascript-backend)
                    template))

(defmethod compile-template ((backend (eql :requirejs-backend)) template)
  (compile-template (make-instance 'requirejs-backend)
                    template))

(defmethod compile-template ((backend javascript-backend) template)
  (compile-to-js template))

(defmethod compile-template ((backend requirejs-backend) template)
  (compile-to-requirejs template))

(defmethod compile-template ((backend javascript-backend) (templates list))
  (let ((namespace-map (make-hash-table :test 'equal)))
    (iter (for template in templates)
          (for namespace = (parse-template template))
          (for name = (namespace-name namespace))
          (setf (gethash name namespace-map)
                (cons (namespace-templates namespace)
                      (gethash name namespace-map nil))))
    (with-output-to-string (out)
      (iter (for (name code-blocks) in-hashtable namespace-map)
            (write-namespace-declaration name out)
            (write-namespace-helpers name out)
            (iter (for templates in code-blocks)
                  (write-namespace-body name templates out))))))

(defmethod compile-js-templates (templates)
  (compile-template :javascript-backend
                    templates))
