;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defvar *swank-port* 95000)

(defvar *docroot* #p"/home/grepz/Projects/eterhost-site/")

(defvar *access-log*      #p"/tmp/eterhost-site-access.log")
(defvar *ssl-access-log*  #p"/tmp/eterhost-site-ssl-access.log")
(defvar *message-log*     #p"/tmp/eterhost-site-message.log")
(defvar *ssl-message-log* #p"/tmp/eterhost-site-ssl-message.log")

(defvar *ssl-cert* #p"/path/to/cert.pem")
(defvar *ssl-key*  #p"/path/to/key.pem")

(defvar *ssl-key-passwd* "somepass")

(defparameter *loglevel* :error)

(defparameter *http-port*  4242)
(defparameter *https-port* 4343)
