;;;; float-controls.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

;; http://code.google.com/p/cl-closure-template/
(asdf:operate 'asdf:load-op '#:closure-template)

;; http://common-lisp.net/project/parenscript/
(asdf:operate 'asdf:load-op '#:parenscript)

;; http://www.cliki.net/RESTAS
(asdf:operate 'asdf:load-op '#:restas)

;; http://common-lisp.net/project/cl-json/
(asdf:operate 'asdf:load-op '#:cl-json)

(restas:define-module #:example.float-controls
  (:use :cl))

(restas:start '#:example.float-controls :port 8080)

(in-package #:example.float-controls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dir* 
  (merge-pathnames "example/float-controls/"
                   (asdf:component-pathname (asdf:find-system '#:closure-template)))
  "float-controls directory")

(defparameter *jquery-dir*
  (merge-pathnames "example/jquery/"
                   (asdf:component-pathname (asdf:find-system '#:closure-template)))
  "JQuery directory")

(defparameter *template-path* (merge-pathnames "float-controls.tmpl" *dir*)
  "Path to file with tempaltes")

;;;; template compilation

(closure-template:compile-template :common-lisp-backend *template-path*)

(defparameter *js-templates*
  (closure-template:compile-template :javascript-backend *template-path*)
  "Compile templates to JavaScript")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *name* "Ivan Petrov")

(defparameter *email* "Ivan.Petrov@example.com")

(defun with-json (&rest args)
  (list* :json (json:encode-json-plist-to-string args)
         args))

(defun name-to-json ()
  (with-json :value *name*
             :save-link (genurl 'save-name)))

(defun email-to-json ()
  (with-json :value *email*
             :save-link (genurl 'save-email)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route file ("resources/:file")
  (merge-pathnames (format nil "resources/~A" file)
                   *dir*))

(define-route image ("resources/images/:file")
  (merge-pathnames (format nil "resources/images/~A" file)
                   *dir*))

(define-route jquery ("jquery/:file")
  (merge-pathnames (format nil "~A" file)
                   *jquery-dir*))

(define-route templates.js ("resources/templates.js"
                            :content-type "text/javascript")
  *js-templates*)

(define-route main (""
                    :render-method 'example.float-controls.view:page)
  (list :name (name-to-json)
        :email (email-to-json)))

(define-route save-name ("api/name" 
                         :method :post 
                         :content-type "application/json"
                         :render-method #'json:encode-json-plist-to-string)
  (setf *name*
        (hunchentoot:post-parameter "value"))
  (name-to-json))

(define-route save-email ("api/email"
                          :method :post
                          :content-type "application/json"
                          :render-method #'json:encode-json-plist-to-string)
  (setf *email*
        (hunchentoot:post-parameter "value"))
  (email-to-json))
