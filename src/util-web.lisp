;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defmacro with-html ((&key (prologue nil) (indent t)) &body body)
  `(with-html-output-to-string
    (*standard-output* nil :prologue ,prologue :indent ,indent)
    ,@body))

(defmacro with-authentication (&body body)
  `(if (null (hunchentoot:session-value :auth))
       (redirect "/login")
       (progn ,@body)))

(defun blog-post-gen-link (host uid)
  (concatenate 'string "http://" host "/post?id=" uid))

(defun parse-tags (input)
  (mapcar #'(lambda (x)
	      (string-trim " " x))
	  (filter-list (split-str input #\,) '())))

(defun tags-list-to-str (tags)
  (list-to-str tags ""))
