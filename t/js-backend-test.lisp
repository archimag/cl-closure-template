;;;; javascript.lisp

(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:closure-template-test)
(asdf:operate 'asdf:load-op '#:parenscript)
(asdf:operate 'asdf:load-op '#:hunchentoot)

(in-package #:closure-template.test)

(defparameter *dir* (asdf:component-pathname (asdf:find-system '#:closure-template)))

(defun plist->ps (plist)
  (cond
    ((and (consp plist)
          (keywordp (car plist))) (cons 'ps:create
                                        (iter (for item in plist)
                                              (unless (eql item 'quote)
                                              (collect (plist->ps item))))))
    ((consp plist) (cons 'list
                         (iter (for item in plist)
                               (unless (eql item 'quote)
                               (collect (plist->ps item))))))
    ((keywordp plist) (make-symbol (symbol-name plist)))
    (t plist)))

(defvar *current-test* nil)

(defun test-code->ps (origin)
  (cond
    ((and (consp origin)
          (eql (car origin) 'template-call)) (list `(,@*current-test* ,(make-symbol (second origin)))
                                                   (plist->ps (if (consp (third origin)) 
                                                                  (second (third origin))
                                                                  (third origin)))))
     
    ((consp origin) (iter (for item in origin)
                          (collect (test-code->ps item))))
    (t origin)))

(defparameter *js-test-suite* '(ps:@ *closure-template *test))

(defun generate-js-test ()
  (let* ((templates nil)
         (tests (iter (for (key var) in-hashtable (get 'common-lisp-backend-test :test-name->code-table))
                      (let ((name key)
                            (result (let ((res (second (car var))))
                                       (if (and (consp res)
                                                (eql (car res) 'quote))
                                           res
                                           (eval res))))
                            (template (third (second (third (car var)))))
                            (code (third (third (car var)))))
                        (unless (eql name 'calculate-5)
                          (let* ((test-symbol (make-symbol (format nil "TEST-~:@(~a~)" name)))
                                 (*default-js-namespace* `(,@*js-test-suite* ,test-symbol))
                                 (*current-test* `(,@*js-test-suite* ,test-symbol))
                                 (closure-template::*check-js-namespace* nil))
                            (push (compile-template :javascript-backend
                                                    template)
                                  templates)
                            (collect test-symbol)
                            (collect `(lambda ()
                                        (ps:with this
                                                 (,(if (typep result 'cons)
                                                       'assert-hash-equal
                                                       'assert-equal)
                                                   ,result
                                                   ,(test-code->ps code)))))))))))
    (with-output-to-string (out)
      (format out
              "~A~%"
              (ps:ps (defvar *closure-template (ps:create))
                     (setf (ps:@ *closure-template *test) (ps:create))))
      (format out
              "~A~%"
              (ps:ps* `(setf ,*js-test-suite*
                            (ps:create ,@tests))))
      (iter (for i in (nreverse templates))
            (format out
                    "~A~%"
                    i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start hunchentoot web server for run tests in browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass test-site-acceptor (hunchentoot:acceptor) () )

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor test-site-acceptor) req)
  (let ((uri (hunchentoot:request-uri req)))
    (cond
      ((string= uri "/") (hunchentoot:handle-static-file (merge-pathnames "t/js/index.html"
                                                                          *dir*)))
      ((string= uri "/closure_template_test.js") (progn
                                                   (setf (hunchentoot:content-type*)
                                                         "text/javascript")
                                                   (generate-js-test)))
      (t (hunchentoot:handle-static-file (merge-pathnames (format nil "t/js~A" uri)
                                                          *dir*))))))


(defun start-test-site (&optional (port 8080))
  (hunchentoot:start (make-instance 'test-site-acceptor
                                    :port port)))

(export 'start-test-site)