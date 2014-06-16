;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(asdf:defsystem #:eterhost-site
  :version "0"
  :serial t
  :description "Simple blog engine for EterHost site"
  :author "Stanislav M. Ivankin <lessgrep@gmail.com>"
  :license "GPLv3"
  :depends-on (:hunchentoot
               :cl-who
	       :cl-markdown
	       :cl-mongo
	       :local-time
	       :parenscript
	       :sb-daemon
	       :swank
	       :md5
;;	       :closure-html
;;	       :cxml-stp
;;	       :drakma
	       )
  :components ((:module "src"
			:serial t
			:components ((:file "package")
				     (:file "blog")
				     (:file "blog-db")
				     (:file "blog-web")
				     (:file "util")))))
