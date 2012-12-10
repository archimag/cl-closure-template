(in-package #:closure-template)

(defclass closure-template (asdf:source-file)
  ((type :initform "tmpl")))

(defmethod asdf:perform ((operation asdf:compile-op) (c closure-template))
  nil)

(defmethod asdf:perform ((operation asdf:load-op) (c closure-template))
  (let ((source-file (asdf:component-pathname c)))
    (format t "; compiling templates file \"~A\"~%" source-file)
    (closure-template:compile-template :common-lisp-backend source-file)))

(defmethod asdf:output-files (operation (c closure-template))
  nil)

(in-package :asdf)

(import 'closure-template:closure-template :asdf)
