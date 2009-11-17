;;;; messages.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(require 'asdf)

;; load restas (http://github.com/archimag/restas) web framework 
(asdf:operate 'asdf:load-op '#:restas) 

(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:parenscript)

;; define site
(restas:defsite #:example.messages
  (:use :cl))

;; yes, site it is package...
(in-package #:example.messages)

;; little magic
(restas:define-site-plugin default (#:example.messages))

;;; start site
(restas:start-site '#:example.messages :port 8080)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dir* 
  (merge-pathnames "example/messages/"
                   (asdf:component-pathname (asdf:find-system '#:closure-template)))
  "Messages directory")

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

(defun make-message (author title message)
  (car (push (list :id (incf *last-message-id*)
                   :author author
                   :title title
                   :message message)
             *messages*)))

(defun message-id (msg)
  (getf msg :id))

(defun message-url (msg)
  (restas:genurl 'message-detail :id (message-id msg)))

(defun escape-string (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
         while char
         do (case char
              ((#\<) (write-string "&lt;" out))
              ((#\>) (write-string "&gt;" out))
              ((#\") (write-string "&quot;" out))
              ((#\') (write-string "&#039;" out))
              ((#\&) (write-string "&amp;" out))
              ((#\Return) )
              ((#\Newline) (write-string "<br />" out))
              (otherwise (write-char char out)))))))

(defun message-json (msg)
  (format nil
          "{href: ~S,title: ~S,author: ~S,message: ~S}"
          (message-url msg)
          (escape-string (getf msg :title))
          (escape-string (getf msg :author))
          (escape-string (getf msg :message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route file ("resources/:file")
  (merge-pathnames (format nil "resources/~A" file)
                   *dir*))

(restas:define-route templates.js ("resources/templates.js"
                                   :content-type "text/javascript")
  *js-templates*)

(restas:define-route all-messages ("/")
  (messages:show-all-messages (list :messages
                                    (loop for msg in *messages*
                                       collect (list* :href (message-url msg) msg)))))

(restas:define-route create-message ("/"
                                     :method :post
                                     :content-type "text/javascript")
  (message-json (car (push (list :id (incf *last-message-id*)
                                 :author (hunchentoot:post-parameter "author")
                                 :title (hunchentoot:post-parameter "title")
                                 :message (hunchentoot:post-parameter "message"))
                           *messages*))))


(restas:define-route message-detail ("messages/:id"
                                     :content-type "text/javascript")
  (let ((msg (find (parse-integer id)
                   *messages*
                   :key #'message-id)))
    (if msg
        (message-json msg)
        hunchentoot:+http-not-found+)))
