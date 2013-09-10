;;;; messages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

;; http://code.google.com/p/cl-closure-template/
(asdf:operate 'asdf:load-op '#:closure-template)

;; http://www.cliki.net/RESTAS
(asdf:operate 'asdf:load-op '#:restas)

;; http://common-lisp.net/project/cl-json/
(asdf:operate 'asdf:load-op '#:cl-json)

(restas:define-module #:example.messages
    (:use #:cl))

(restas:start '#:example.messages :port 8080)

(in-package #:example.messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *basedir*
  (make-pathname :directory
                 (butlast
                  (pathname-directory
                   (asdf:component-pathname (asdf:find-system '#:closure-template))))))

(defparameter *dir* 
  (merge-pathnames "example/messages/" *basedir*)
  "Messages directory")

(defparameter *jquery-dir*
  (merge-pathnames "example/jquery/"
                   *basedir*)
  "JQuery directory")

(defparameter *template-path* (merge-pathnames "messages.tmpl" *dir*)
  "Path to file with tempaltes")

;;;; template compilation
(closure-template:compile-template :common-lisp-backend *template-path*)

(defparameter *js-templates*
  (closure-template:compile-template :javascript-backend *template-path*)
  "Compile templates to JavaScript")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *last-message-id* 0)

(defvar *messages* nil)

(defun message-id (msg)
  (getf msg :id))

(defun message-with-href (msg)
  (list* :href (restas:genurl 'message-detail :id (message-id msg))
         msg))

(defun make-message (author title message)
  (car (push (list :id (incf *last-message-id*)
                   :author author
                   :title title
                   :message message)
             *messages*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route messages.js ("messages.js")
  (merge-pathnames "messages.js" *dir*))

(restas:define-route templates.js ("templates.js")
  (:content-type "text/javascript")
  *js-templates*)

(restas:define-route jquery ("jquery/jquery:(str).js")
  (merge-pathnames (format nil "jquery~A.js" str)
                   *jquery-dir*))


(restas:define-route all-messages ("/")
  (:render-method 'messages:show-all-messages)
  (list :messages
        (loop for msg in *messages*
           collect (message-with-href msg))))

(restas:define-route create-message ("/" :method :post)
  (:content-type "text/javascript")
  (:render-method #'json:encode-json-plist-to-string)
  (message-with-href (make-message (hunchentoot:post-parameter "author")
                                   (hunchentoot:post-parameter "title")
                                   (hunchentoot:post-parameter "message"))))


(restas:define-route message-detail ("messages/:id")
  (:content-type "text/javascript")
  (:render-method #'json:encode-json-plist-to-string)
  (or (find (parse-integer id)
            *messages*
            :key #'message-id)
      hunchentoot:+http-not-found+))
