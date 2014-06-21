;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:common-lisp-user)

(require :cl-xmpp-sasl)

(setq *connection* (xmpp:connect :hostname "eterhost.org"))
(xmpp:auth *connection* "tester" "vq4s6ZUloJoVCCC6X0Rt"
	   "eterhost.org" :mechanism :sasl-digest-md5)
(xmpp:message *connection* "grepz@eterhost.org" "what's going on?")

(xmpp:connectedp *connection*)

(xmpp:get-roster *connection*)
