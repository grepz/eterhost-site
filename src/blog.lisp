;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defparameter *eterhost-running*         nil)
(defparameter *hunchentoot-ssl-listener* nil)
(defparameter *hunchentoot-listener*     nil)

(defvar *blog-hostname*  "eterhost.org")
(defvar *host-atom-uuid* "59FE1880-F62E-11E3-8BDA-C8F7337E6FD0")

(defvar *feed-update-timestamp* "2014-06-18T00:00:00Z")
(defvar *feed-symbol-cut* 256)

(defvar *blog-last-change-timestamp* "2014-06-14")
(defvar *blog-author*                "Stanislav M. Ivankin")
(defvar *blog-author-email*          "lessgrep@gmail.com")
(defvar *blog-name*                  "EterHost.org")

(defun make-dir (root sub)
  (merge-pathnames
   (make-pathname :directory (append (pathname-directory root) (list sub)))))

;;(setf hunchentoot:*catch-errors-p* t)
;;(load "/home/grepz/Projects/eterhost-site/src/config.lisp")

(defun blog-start (&key (debug nil) (http-port 80) (https-port 443)
		     (loglevel :warning) docroot ssl-cert ssl-key ssl-key-passwd
		     access-log ssl-access-log message-log ssl-message-log)
  (assert (and (null *hunchentoot-listener*)
	       (null *hunchentoot-ssl-listener*)))
  (format t "Starting hunchentoot http:~a, https:~a.~%" http-port https-port)
  (setf hunchentoot:*lisp-errors-log-level*   loglevel
	hunchentoot:*lisp-warnings-log-level* loglevel
	hunchentoot:*catch-errors-p* (not debug))
  (when (not debug)
    (push (lambda ()
	    (setf swank::*log-output* nil))
	  sb-ext:*save-hooks*))
  (setf *hunchentoot-listener* ;; HTTP
	(make-instance
	 'hunchentoot:easy-acceptor
	 :port http-port
	 :document-root docroot
	 :error-template-directory (make-dir *docroot* "www/errors")
	 :access-log-destination access-log
	 :message-log-destination message-log)
	*hunchentoot-ssl-listener* ;; HTTPS
	(make-instance
	 'hunchentoot:easy-ssl-acceptor
	 :port https-port
	 :ssl-privatekey-password ssl-key-passwd
	 :ssl-certificate-file ssl-cert
	 :ssl-privatekey-file ssl-key
	 :document-root docroot
	 :error-template-directory (make-dir *docroot* "www/errors")
	 :access-log-destination ssl-access-log
	 :message-log-destination ssl-message-log))
  (blog-db-start)
  (hunchentoot:start *hunchentoot-listener*)
  (hunchentoot:start *hunchentoot-ssl-listener*))

(defun blog-stop ()
  (blog-db-stop)
  (hunchentoot:stop *hunchentoot-listener*)
  (hunchentoot:stop *hunchentoot-ssl-listener*)
  (setf *hunchentoot-listener* nil
	*hunchentoot-ssl-listener* nil))

(defun main (argv)
  (when (< (length sb-ext:*posix-argv*) 2)
    (error "Missing command line arguments"))
  ;; Load configuration file
  (destructuring-bind (argv0 config) sb-ext:*posix-argv*
    (load (pathname config)))
  (format t "---> ~a~%" sb-ext:*posix-argv*)
  ;; Fork
  (sb-daemon:daemonize :output "/tmp/eterhostd.out"
		       :error "/tmp/eterhostd.err"
		       :exit-parent t
		       :sigterm (lambda (sig)
				  (declare (ignore sig))
				  (setf *eterhost-running* nil)))
  ;; Create swank server instance
  (setf *swank-server*
        (swank:create-server :port *swank-port*
                             :dont-close t))
  ;; And finally fire up site functionality
  (setf *eterhost-running* t)
  ;; Pass config options
  (blog-start :http-port       *http-port*
	      :https-port      *https-port*
	      :docroot         *docroot*
	      :access-log      *access-log*
	      :ssl-access-log  *ssl-access-log*
	      :message-log     *message-log*
	      :ssl-message-log *ssl-message-log*
	      :ssl-cert        *ssl-cert*
	      :ssl-key         *ssl-key*
	      :ssl-key-passwd  *ssl-key-passwd*
	      :loglevel        *loglevel*)
  ;; Idle
  (loop while *eterhost-running* do (sleep 1)))
