;;;; closure-template.asd
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem closure-template
  :depends-on (#:babel #:parse-number #:esrap #:iterate #:closer-mop #:split-sequence #:alexandria)
  :pathname "src/"
  :serial t
  :components ((:module "parser"
                        :pathname "parser/"
                        :serial t
                        :components ((:file "defpackage")
                                     (:file "command")
                                     (:file "expression")))
               (:file "defpackage")
               (:file "interface")
               (:file "asdf" :depends-on ("defpackage"))
               (:module "common-lisp-backend"
                        :pathname ""
                        :serial t
                        :components ((:file "escape")
                                     (:file "common-lisp-backend")))
               (:file "standard-templates")
               (:module "javascript-backend"
                        :pathname ""
                        :components ((:file "javascript-backend")))
               ;; (:module "python-backend"
               ;;          :pathname ""
               ;;          :components ((:file "python-backend")))
               ))

(defmethod perform ((o test-op) (c (eql (find-system 'closure-template))))
  (operate 'load-op 'closure-template)
  (operate 'test-op 'closure-template-test))

(defsystem closure-template-test
  :depends-on (#:closure-template #:lift)
  :pathname "t/"
  :serial t
  :components ((:file "suite")
               (:file "parser")
               (:file "common-lisp-backend")))
