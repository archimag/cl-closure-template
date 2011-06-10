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
  :depends-on (#:babel #:parse-number #:esrap #:iterate)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "parser" :depends-on ("packages"))
                                     (:file "translate" :depends-on ("parser"))
                                     (:file "escape" :depends-on ("packages"))
                                     (:file "common-lisp-backend" :depends-on ("translate" "escape"))
                                     ;;(:file "standard-templates" :depends-on ("common-lisp-backend"))
                                     ))))

(defmethod perform ((o test-op) (c (eql (find-system 'closure-template))))
  (operate 'load-op 'closure-template)
  (operate 'test-op 'closure-template-test))


(defsystem closure-template-test
  :depends-on (#:closure-template #:lift)
  :components ((:module "t"
                        :components ((:file "suite")
                                     (:file "parser" :depends-on ("suite"))
                                     (:file "common-lisp-backend" :depends-on ("suite"))))))

#+asdf-system-connections
(defsystem-connection closure-template-javascript-backend
  :requires (closure-template parenscript)
  :components ((:module "src"
                        :components ((:file "javascript-backend")))))

