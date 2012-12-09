;;;; javascript.lisp

(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:restas)

(restas:define-module #:closure-template.js-test
  (:use #:cl))

(in-package #:closure-template.js-test)

(defparameter *static-pathname*
  (merge-pathnames "js/"
                   (make-pathname :directory (pathname-directory (asdf:component-pathname (asdf:find-system '#:closure-template-test))))))

(restas:define-route index ("/")
  (merge-pathnames "index.html" *static-pathname*))

(restas:define-route template.js ("closure-templates.js" :content-type "application/javascript")
  (closure-template::compile-js-templates (merge-pathnames "template.soy" *static-pathname*)))

(restas:define-route static (":path")
  (merge-pathnames path *static-pathname*))

(restas:start '#:closure-template.js-test :port 8080)
