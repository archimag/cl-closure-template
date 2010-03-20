;;;; escape.lisp
;;;;
;;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:closure-template)

(defun escape-html (str)
  (if (stringp str)
      (with-output-to-string (out)
        (iter (for ch in-string str)
              (case ch
                ((#\<) (write-string "&lt;" out))
                ((#\>) (write-string "&gt;" out))
                ((#\") (write-string "&quot;" out))
                ((#\') (write-string "&#039;" out))
                ((#\&) (write-string "&amp;" out))
                (otherwise (write-char ch out)))))
      str))

(defun encode-string (str not-encode)
  (if (stringp str)
      (with-output-to-string (out)
        (iter (for ch in-string str)
              (if (or (char<= #\0 ch #\9) (char<= #\a ch #\z) (char<= #\A ch #\Z)
                      (find ch not-encode :test #'char=))
                  (write-char ch out)
                  (iter (for octet in-vector (babel:string-to-octets (string ch) :encoding :utf-8))
                          (format out "%~2,'0x" octet)))))
      str))
  

(defun encode-uri (str)
  (encode-string str "~!@#$&*()=:/,;?+'"))

(defun encode-uri-component (str)
  (encode-string str "~!*()'"))


(defun decode-uri (string)
  "Replaces each escape sequence in the encoded URI with the character 
that it represents."
  (let ((curpos 0)
        (maxpos (length string))
        (strs nil))
    (flet ((next-ansii-substring ()
             (let ((pos (or (position #\% string :start curpos)
                            maxpos)))
               (when (< curpos pos)
                 (push (subseq string
                               curpos
                               pos)
                       strs)
                 (setf curpos pos))))
           (next-encoded-substring ()
             (let ((pos (or (iter (for i from curpos below maxpos by 3)
                                  (finding i
                                           such-that (not (char= (aref string i)
                                                                 #\%))))
                            maxpos)))
               (when (< curpos pos)
                 (let ((octets (make-array (/ (- pos curpos)
                                              3)
                                           :element-type '(unsigned-byte 8)
                                           :fill-pointer 0)))
                   (iter (for i from curpos below pos by 3)
                         (vector-push (parse-integer string
                                                     :start (1+ i)
                                                     :end (+ i 3)
                                                     :radix 16)
                                      octets))
                   (setf curpos pos)
                   (push (babel:octets-to-string octets
                                                 :encoding :utf-8)
                         strs))))))
      (iter (while (< curpos maxpos))
            (next-ansii-substring)
            (while (< curpos maxpos))
            (next-encoded-substring)
            ))
      (apply #'concatenate
             'string
             (nreverse strs))))
