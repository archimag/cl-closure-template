;;;; closure-template.asd
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:closure-template-system
  (:use #:cl #:asdf))

(in-package #:closure-template-system)

(when (find-system 'asdf-system-connections)
  (operate 'load-op 'asdf-system-connections))

(defsystem closure-template
  :depends-on (#:wiki-parser #:split-sequence #:babel)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "expression" :depends-on ("packages"))
                                     (:file "template" :depends-on ("expression"))
                                     (:file "translate" :depends-on ("template"))
                                     (:file "common-lisp-backend" :depends-on ("translate"))
                                     (:file "standard-templates" :depends-on ("common-lisp-backend"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'closure-template))))
  (operate 'load-op 'closure-template-test)
  (operate 'test-op 'closure-template-test :force t))


(defsystem closure-template-test
  :depends-on (#:closure-template #:lift)
  :components ((:module "t"
                        :components ((:file "core-test")
                                     (:file "cl-backend-test" :depends-on ("core-test"))))))


(defmethod perform ((o test-op) (c (eql (find-system 'closure-template-test))))
  (operate 'load-op 'closure-template-test )
  (let* ((test-results (funcall (intern (symbol-name 'run-closure-template-tests) '#:closure-template.test)))
         (errors (funcall (intern (symbol-name 'errors) :lift) test-results))
         (failures (funcall (intern (symbol-name 'failures) :lift) test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))

#+asdf-system-connections
(defsystem-connection closure-template-javascript-backend
  :requires (closure-template parenscript)
  :components ((:module "src"
                        :components ((:file "javascript-backend")))))

