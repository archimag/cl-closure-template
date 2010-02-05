;;;; githubway.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(require 'asdf)

(loop for system in '(#:closure-template ;; http://code.google.com/p/cl-closure-template/
                      #:parenscript      ;; http://common-lisp.net/project/parenscript/
                      #:restas           ;; http://www.cliki.net/RESTAS
                      #:cl-json          ;; http://common-lisp.net/project/cl-json/
                      )
   do (asdf:operate 'asdf:load-op system))

(restas:defsite #:example.githubway
  (:use :cl))

(restas:start-site '#:example.githubway :port 8080)


(in-package #:example.githubway)


