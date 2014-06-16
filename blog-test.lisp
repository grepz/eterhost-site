;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defvar *post-num* 10)
(defvar *post-link* "http://127.0.0.1:4242/post")
(defvar *post-content* "post_data=")

(defvar *pool* nil)

(defvar *reqs-per-conn* 1)
(defvar *conn-num* 3)

(defun get-post-content (id)
    (let* ((query (list (cons "id" (stringify id))))
	   (str (drakma:http-request *post-link* :parameters query))
	   (document (chtml:parse str (cxml-stp:make-builder)))
	   res)
      (stp:do-recursively (node document res)
	(when (and (typep node 'stp:element)
		   (equal (stp:local-name node) "div")
		   (equal (stp:attribute-value node "class") "post-content"))
	  (setq res (format nil "~A" (stp:string-value node)))))))

(defun http-request-session ()
  (let ((post-ids (loop for x = (1+ (random *post-num*))
		     repeat *reqs-per-conn* collect x)))
    (loop for id in post-ids collect
	 (cons id (get-post-content id)))))

(defun blog-check-request ()
  (let* ((id (1+ (random *post-num*)))
	 (dc (concatenate 'string *post-content* (stringify id)))
	 (dg (string-trim '(#\Space #\Newline) (get-post-content id))))
    (string= dc dg)))

(defun start-test ()
;;  (setq *pool* (thread-pool:make-thread-pool 5))
;;  (thread-pool:start-pool *pool*)
  )

(let ((out *standard-output*))
	   (defun do-req (x)
	     (if (not (blog-check-request))
		(format out "------------> Error!~%")
		(format out "------------> Ok.~%"))
	     (format out "Thread ~a complete.~%" x)
	     (sleep (random 5))))

(defun test ()
  (blog-db-get-posts 100))

(loop for x from 0 to 1000 do
     (thread-pool:add-to-pool *pool* #'test))

(let ((out *standard-output*))
      (defun test-persist ()
	  (let ((test (car (elephant:get-instances-by-value
			    'blog-data-post 'title "green sleeves"))))
	  (loop repeat 50 do
	       (sleep 0.5)
	       (when test
		 (format out "Title: ~a~%" (title test)))))))

(let ((out *standard-output*))
      (defun test-comments (nick post-id)
	(with-transaction ()
	  (let ((comment "comment")
		(author (comment-author/get "grep-z@ya.ru")))
	  (loop repeat 100 do
	       (comment-data/add comment nick post-id (author-id author)
				 (approved author))
	       (format out "Posting for nick ~a~%" nick))))))

(time (test-comments "user1" 3))
(time (test-comments "user2" 3))


#(46 0 0 0 3 113 117 101 114 121 0 5 0 0 0 0 3 111 114 100 101 114 98 121
       0 20 0 0 0 16 69 68 73 84 45 84 73 77 69 0 255 255 255 255 0 0)

(cl-mongo::bson-decode)
