;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defparameter *blog-posts-per-page* 0)

(defvar *date-time-format* '((:year 4) #\- (:month 2) #\- (:day 2)
			     #\Space (:hour 2) #\: (:min 2) #\Space
			     :timezone))

(defvar *blog-nav-list* '(("/"         "Blog")
			  ("/projects" "Projects")
			  ("/archive"  "Archive")
			  ("/feed"     "Feed")
			  ("/about"    "About")))

(defun blog-navigation (menu-list separator)
  (with-html ()
    (:ul
     (dolist (entry menu-list)
       (htm
	(:li (:a :href (car entry)
		 (fmt "~a" (cadr entry)))
	     (fmt "~a" separator))))
     (when (hunchentoot:session-value :auth)
       (htm
	(:li (:a :href "/admin" "Admin")))))))

(defun blog-footer ()
  "Footer for the site page"
  (with-html ()
    (:div :class "footer"
	  (:div :class "footer-inner"
		(:div :class "footer-left"
		      (:ul
		       (:li "Copyleft / ")
		       (:li (fmt "~a / " *blog-name*))
		       (:li (fmt "~a" *blog-last-change-timestamp*)))
		      (:ul
		       (:li "Author: ")
		       (:li :class "small"
			    (:a :href "about" (fmt "~a" *blog-author*)))
		       (:br)
		       (:li "Powered by: ")
		       (:li :class "small"
			    (:a :href "http://en.wikipedia.org/wiki/Common_Lisp"
				"CL"))
		       (:li " / ")
		       (:li :class "small"
			    (:a :href "http://weitz.de/hunchentoot/"
				"Hunchentoot web server"))))
		(:div :class "footer-right"
		      (fmt "~a" (blog-navigation *blog-nav-list* "|")))))))

(defun blog-header (name)
  "Hedear for the site page"
  (with-html ()
    (:div :class "header"
	  (:div :class "logo"
		(:h1
		 (fmt "~a" name)
		 (:div :class "navigation"
		       (fmt "~a" (blog-navigation *blog-nav-list* " "))))))))

(defmacro blog-page ((&key title name) &body body)
  "Generic blog web page macro."
  `(with-html (:prologue t)
	(:html :xmlns "http://www.w3.org/1999/xhtml"
	       :xml\:lang "ru"
	       :lang "ru"
	       (:head
		(:meta :http-equiv "Content-Type"
		       :content "text/html;charset=utf-8")
		(:title (fmt "~a" ,title))
		(:script :type "text/javascript" :src "/eterhost.js")
		(:link :rel "shortcut icon"
		       :href "/img/favicon.ico"
		       :type "image/x-icon")
		(:link :type "text/css"
		       :rel "stylesheet"
		       :href "/css/blog.css"))
	       (:body
		(fmt "~a" (blog-header ,name))
		(:div :class "main"
		      ,@body)
		(:div :class "separater")
		(fmt "~a" (blog-footer))))))

(defun blog-post-comment (comment &key (admin nil))
  "Print comment html data."
  (with-html ()
    (:div :id "comment-box"
	  (:p (:h4
	       (fmt "~a"
		    (concatenate
		     'string "By \"" (comment-nick comment) "\" on "
		     (local-time:format-timestring
		      nil (local-time:universal-to-timestamp
			   (get-edit-time comment))
		      :timezone local-time:+utc-zone+
		      :format *date-time-format*)
		     (when admin
		       (concatenate 'string " (Visible:"
				    (if (hidden? comment) "no" "yes") ")"))
		     ":"))))
	  (fmt "~a" (blog-db-comment/format comment)))
    (:br)))

(defun blog-post-tags (tags)
  (with-html ()
    (:div :class "tag"
	  (:b "Tags:")
	  (dolist (tag tags)
	    (htm
	     (:a :href (format nil "/tag?tag=~a" tag) (fmt "~a" tag)))))))

(defun htmlize-blog-post (post &key (admin nil) (title-link nil)
				 (comments nil))
  "Produce blog post html."
  (let ((id (blog-db/id-to-str post))
	(tags (get-tags post)))
    (with-html ()
      (:h1
       ;; If title-link is set, then generate a link leading to a post
       (if title-link
	   (htm (:a :href (format nil "post?id=~a" id)
		    (fmt "~a" (get-title post))))
	   (fmt "~a" (get-title post))))
      ;; Show post html content
      (:p (:div :class "post-content" (fmt "~a" (get-text-html post))))
      (when tags
	(fmt "~a" (blog-post-tags tags)))
      (:br)
      ;; Post menu
      (:div :class "post-menu"
	    ;; edit time goes first
	    (:b (fmt "Updated on ~a"
		     (local-time:format-timestring
		      nil (local-time:universal-to-timestamp
			   (get-edit-time post))
		      :timezone local-time:+utc-zone+
		      :format *date-time-format*)))
	    ;; If comments parameter is provided and comments are allowed for
	    ;; the post, generate a link leading to comments section
	    (when (and comments (comments-allowed? post))
	      (htm (:div :class "link-comment"
			 (:a :href (format nil "post?id=~a#comments" id)
			     (fmt "~a"
				  (blog-db-get-comments-num
				   (blog-db/get-oid post)))))))
	    ;; If user is logged as admin, show additional controlling options
	    ;; such as edit and delete link
	    (when admin
	      (htm
	       (:div :class "link-edit"
		(:a :href
		 (format nil "/admin/edit-data?type=post&id=~a&act=edit" id)
		 (fmt "Edit")))
	       (:div :class "link-delete"
		(:a :href
		 (format nil "/admin/edit-data?type=post&id=~a&act=del" id)
		 (fmt "Delete")))))))))

(define-easy-handler (tutorial2-javascript :uri "/eterhost.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (defun greeting-callback ()
      (alert "Hello World!!!")
      (return-from greeting-callback false))
    (defun comment-submit-check ()
      (let ((comment (getprop
		      (aref
		       (aref document.forms "fcomment")
		       "comment") 'value))
	    (email (getprop
		    (aref
		     (aref document.forms "fcomment")
		     "email")'value)))
	(when (or (null comment)
		  (null email)
		  (string= comment "")
		  (string= email ""))
	  (alert "Important fields are empty.")
	  (return-from comment-submit-check false))
	(return-from comment-submit-check true)))
    ;; (defun disqus-fun ()
    ;;   (let ((dsq (document.create-element "script")))
    ;; 	(setf dsq.type "text/javascript"
    ;; 	      dsq.async true)
    ;; 	(setf dsq.src (+ "//" disqus_shortname ".disqus.com/embed.js"))
    ;; 	(let ((func (or (aref (document.get-elements-by-tag-name "head") 0)
    ;; 			(aref (docment.get-elements-by-tag-name "body") 0))))
    ;; 	  (func.append-child dsq))))
    ))

(define-easy-handler (blog-posts :uri "/") ()
  (blog-page
      (:title "EterHost.org" :name "EterHost.org - Blog")
      (:div :class "data-column"
       (:div :id "content"
	(dolist (post (blog-db-get-posts *blog-posts-per-page*))
	 (fmt "~a"
	      (htmlize-blog-post
	       post :admin (hunchentoot:session-value :auth)
	       :title-link t :comments t)))))))

(define-easy-handler (blog-posts-tag :uri "/tag") ()
  (let ((tag (hunchentoot:get-parameter "tag")))
    (blog-page
	(:title "EterHost.org - Tag" :name "EterHost.org - Blog")
      (:div :class "data-column"
       (:div :id "content"
	(dolist (post (blog-db-get-posts-by-tag *blog-posts-per-page* tag))
	 (fmt "~a"
	      (htmlize-blog-post
	       post :admin (hunchentoot:session-value :auth)
	       :title-link t :comments t))))))))

(define-easy-handler (post :uri "/post") ()
  (let ((id-param (hunchentoot:get-parameter "id")) doc data id)
    ;; Check if ID parameter was supplied
    (when (null id-param)
      (redirect "/"))
    (setf id (id-str-to-oid id-param)
	  doc (doc-find-by-oid *db-post-collection* id))
    ;; If no doc found, return to the root
    (when (null doc)
      (redirect "/"))
    (setf data (make-instance 'blog-db-post :mongo-doc doc)
	  (hunchentoot:session-value :post-id) id)
    (blog-page
	(:title (concatenate 'string "EterHost.org - " (get-title data))
		:name "EterHost.org - Blog")
      (:div :class "data-column"
	    (:div :id "content"
		  (fmt "~a" (htmlize-blog-post data
				       :admin (hunchentoot:session-value :auth)
				       :comments nil))
		  (when (comments-allowed? data)
		    (htm
		     (:h2 "Leave comment:")
		     (:form :class "comment-form" :action "/comment"
			    :name "fcomment"
			    :onsubmit (ps (comment-submit-check))
			    :method :post
			    (:label (:span "Email:")
				    (:input :type :text :name "email"))
			    (:label (:span "Name:")
				    (:input :type :text :name "nick"))
			    (:br)
			    (:label (:span "Comment")
				    (:textarea :id "content" :name "comment"))
			    (:input :type :submit :value "Submit"))
		     (:h2 "Comments:")
		     (:div :id "comments" (:a :name "comments")
		      (dolist (comment (blog-db-get-comments :post-id id))
		      	(when (or (not (hidden? comment))
		      		  (hunchentoot:session-value :auth))
		      	  (fmt "~a"
		      	   (blog-post-comment comment
			    :admin (hunchentoot:session-value :auth)))))))))))))

(define-easy-handler (comment :uri "/comment") ()
  (let* ((email   (post-parameter "email"))
	 (nick    (post-parameter "nick"))
	 (comment (post-parameter "comment"))
	 (post-id (hunchentoot:session-value :post-id))
	 (addr    (hunchentoot:remote-addr*))
	 author)
    (hunchentoot:log-message*
     :info "Comment. e-mail='~a';nick='~a';comment='~a';Post ID='~a';Addr='~a'"
     email nick comment post-id addr)
    (if (or (null post-id) (comment-data-invalid? comment email nick))
	(hunchentoot:log-message* :info "Comment is not valid to be stored.")
	(progn
	  (hunchentoot:log-message* :info "Saving comment in DB.")
	  (setf author (or (car (blog-db-get-comment-author email))
			   (blog-db-add-comment-author email)))
	  (hunchentoot:log-message* :info "Author ID=<~a>, Post ID=<~a>"
				    (blog-db/get-oid author) post-id)
	  (blog-db-add-comment
	   (hunchentoot:escape-for-html comment)
	   (if (zerop (length nick))
	       "anonymous"
	       (hunchentoot:escape-for-html nick))
	   post-id (blog-db/get-oid author) addr (approved? author))))
    (if (null post-id)
	(redirect "/")
	(redirect (format nil "/post?id=~a&" (id-oid-to-str post-id))))))

(define-easy-handler (login :uri "/login" :default-request-type :post)
    ((username :parameter-type 'string)
     (password :parameter-type 'string))
  ;; Returns current session or creates new one if needed, sends cookie
  (hunchentoot:start-session)
  ;; If user is already authenticated, skip to /admin
  (when (hunchentoot:session-value :auth)
    (redirect "/admin"))
  ;; If SSL is not used redirect to a ssl enabled link
  (when (null (ssl-p))
    (redirect "/login" :protocol :https :port *https-port*))
  (cond
    ((null (hunchentoot:session-value :failed-login))
     (setf (hunchentoot:session-value :failed-login) 0))
    ;; TODO: Check for max tries and log attempt
    )
  (blog-page (:title "EterHost.org" :name "EterHost.org - Login")
   (:div :id "static-content"
    (:form :class "login-form" :action "/login-check" :method :post
     (:p (:input :type :text :name "username"
		 :placeholder "Username" :value username))
     (:p (:input :type :password :name "password"
		 :placeholder "Password" :value password))
     (:p (:input :type :submit :value "Login"))))))

(define-easy-handler (logout :uri "/logout") ()
  (setf (hunchentoot:session-value :auth) nil)
  (redirect "/"))

(define-easy-handler (login-check :uri "/login-check") ()
  (let* ((username (post-parameter "username"))
	 (password (post-parameter "password"))
	 (dbuser (blog-db-get-user username)))
    (hunchentoot:log-message*
     :info "Username='~a', password='~a';" username password)
    (if (blog-db-user/pwcheck dbuser (md5:md5sum-string password))
	(progn ;; Login successfull
	  (setf (hunchentoot:session-value :failed-login) 0
		(hunchentoot:session-value :auth)         t)
	  (redirect "/admin"))
	(progn ;; Login failed
	  (incf (hunchentoot:session-value :failed-login))
	  (redirect "/login")))))

(define-easy-handler (admin :uri "/admin") ()
  (with-authentication
    (blog-page (:title "EterHost.org - Admin" :name "EterHost.org - Admin")
      (:div :id "static-content"
       (:h1 "Edit content")
       (:ul
	(:li (:a :href "/admin/edit-data?type=post&act=edit" "New post"))
	(:li (:a :href "/admin/edit-static" "Edit static"))
	(:li (:a :href "/admin/edit-comments" "Comments")))))))

(define-easy-handler (admin-edit-comments :uri "/admin/edit-comments") ()
  (with-authentication
    (blog-page (:title "EterHost.org - Admin" :name "EterHost.org - Admin")
      (:div :id "static-content"
	    (:form :class "comment-edit-form"
		   :action "/admin/edit-comments-submit"
		   :method :get
		   (dolist (author (blog-db-get-comment-author nil))
		     (htm
		      (:input :type :checkbox
			      :name "comment_author"
			      ;; TODO: Bug on firefox, not show as checked for
			      ;; some reason
;;			      :checked (approved author)
			      :value (author-email author)
			      (fmt "Approved: ~a [~a]"
				   (if (approved? author) "yes" "no")
				   (author-email author)))
		      (:br)))
		   (:br)
		   (:input :type :submit :name "comment_submit"
			   :value "Change")
		   (:br)
		   (:input :type :submit :name "comment_submit"
			   :value "Delete"))))))

(define-easy-handler (comment-check :uri "/admin/edit-comments-submit") ()
  (with-authentication
    (let ((authors (hunchentoot:get-parameters*))
	  (action (hunchentoot:get-parameter "comment_submit"))
	  obj)
      (if (string= action "Change")
	  (loop for author in authors do
	       (when (string= (car author) "comment_author")
		 (setf obj (car (blog-db-get-comment-author (cdr author))))
		 (blog-db-change-approved-status obj)
		 (loop for comment in (blog-db-get-comments
				       :author-id (blog-db/get-oid obj)) do
		      (blog-db-comment/hide comment (not (approved? obj))))))
	  (loop for author in authors do
	       (when (string= (car author) "comment_author")
		 (setf obj (car (blog-db-get-comment-author (cdr author))))
		 ;; Delete comments that belong to author
		 (dolist (comment (blog-db-get-comments
				   :author-id (blog-db/get-oid obj)))
		   (blog-db/delete comment))
		 ;; Delete author
		 (blog-db/delete obj))))
    (redirect "/admin/edit-comments"))))

(define-easy-handler (admin-edit-static :uri "/admin/edit-static") ()
  (with-authentication
    (blog-page (:title "EterHost.org - Static"
		       :name "EterHost.org - Admin - Static")
      (:div :id "static-content"
       (:ul
	(dolist (data (blog-db-get-static nil))
	  (htm
	   (:li
	    (:a :href (format nil "/admin/edit-data?id=~a&act=edit&type=static"
			      (blog-db/id-to-str data))
		(fmt "~a" (get-title data)))))))
       (:br)
       (:a :href "/admin" "Return")))))

(define-easy-handler (edit-data :uri "/admin/edit-data") ()
  (with-authentication
    (let* ((id-param  (hunchentoot:get-parameter "id"))   ;; OID
	   (act-type  (hunchentoot:get-parameter "act"))  ;; edit/delete
	   (data-type (hunchentoot:get-parameter "type")) ;; static/post
	   data id)
      ;; Check parameters
      (when (or (null data-type) (null act-type))
	(redirect "/"))
      (when id-param
	(setf id (id-str-to-oid id-param)))
      ;; de-serialize DB data
      (setf data
	    (cond ((string= data-type "post")
		   (make-instance 'blog-db-post :mongo-doc
				  (doc-find-by-oid *db-post-collection* id)))
		  ((string= data-type "static")
		   (make-instance 'blog-db-data :mongo-doc
				  (doc-find-by-oid *db-static-collection* id)))
		  (t nil)))
      (when (null data)
	(redirect "/"))
      (cond ((string= act-type "edit")
	     (setf (hunchentoot:session-value :dataref) data)
	     ;; Show data edit form
	     (blog-page (:title "EterHost.org - Edit data"
				:name "EterHost.org - Edit")
	       (:div :id "static-content"
		     (:form :class "data-edit-form"
			    :action "/admin/edit-data-submit"
			    :method :post
			    (:h3 "Edit blog data:")
			    (:label (:span "Title")
				    (:input :id "title"
					    :type "text"
					    :name "title"
					    :value (get-title data)))
			    (:label (:span "Content")
				    (:textarea :id "content"
					       :name "content"
					       (fmt "~a" (get-text-src
					       data))))
			    (when (string= data-type "post")
			      (htm
			       (:label (:span "Tags")
				       (:input
					:id "tags"
					:type "text"
					:name "tags"
					:value (blog-db-post/tags-str data)))))
			    (:input :type :submit :name "data_submit"
				    :value "Submit")
			    (:input :type :submit :name "data_submit"
				    :value "Check")))))
	    ((string= act-type "del")
	     ;; Delete comments that belong to post
	     (dolist (comment (blog-db-get-comments :post-id id))
	        (blog-db/delete comment))
	     ;; Delete post
	     (blog-db/delete data)
	     (redirect "/"))))))

(define-easy-handler (edit-check :uri "/admin/edit-data-submit") ()
  (with-authentication
    (let ((title (post-parameter "title"))
	  (text-src (remove #\Linefeed (post-parameter "content")))
	  (tags (post-parameter "tags"))
	  (action (hunchentoot:get-parameter "data_submit"))
	  (data (hunchentoot:session-value :dataref)))
      (assert data)
      (setf (get-title data)     title
	    (get-text-src data)  text-src
	    (get-edit-time data) (get-universal-time))
      (when (/= (length tags) 0)
	(setf (get-tags data) (parse-tags tags)))
      ;; Render to html format
      (blog-db-data/render data)
      (blog-db/generate-doc data)
      (blog-db/save data)
      (redirect (format nil "/post?id=~a&"
			(id-oid-to-str (blog-db/get-oid data)))))))

(define-easy-handler (projects :uri "/projects") ()
  (blog-page
   (:title "EterHost.org - Projects" :name "EterHost.org - Projects")
   (:div :id "static-content"
	 (:h1 "TODO"))))

(define-easy-handler (archive :uri "/archive") ()
  (blog-page
   (:title "EterHost.org - Archive" :name "EterHost.org - Archive")
   (:div :id "static-content"
	 (:h1 "TODO"))))

(define-easy-handler (about :uri "/about") ()
  (blog-page
   (:title "EterHost.org - About" :name "EterHost.org - About")
   (:div :id "static-content"
	 (fmt "~a" (get-text-html (car (blog-db-get-static "about")))))))

(define-easy-handler (feed :uri "/feed") ()
   (with-atom-xml ("http://eterhost.org/feed" *feed-update-timestamp*
		   :title "Grepz Blog" :link-alt "http://eterhost.org"
		   :subtitle "Grepz's internet hut." :id *host-atom-uuid*)
     (dolist (post (blog-db-get-posts 10))
       (fmt "~a" (atom-xml-entry
		     (hunchentoot:escape-for-html (get-text-html post))
		   :title (get-title post)
		   :id (get-feed-uuid post)
		   :entry-link (blog-post-gen-link
				*blog-hostname* (blog-db/id-to-str post))
		   :updated (get-edit-time post))))))

;; (defun 404-dispatcher (request)
;;   '404-page)

;; (defun 404-page ()
;;   "404 is here!")

;;(setf *dispatch-table* (nconc *dispatch-table* '(404-dispatcher)))


(defmethod handle-request :before ((acc acceptor) (req request))
  ;; (hunchentoot:log-message*
  ;;  :info "Addr='~a:~a', Request='~a', Docroot='~a';"
  ;;  (remote-addr req) (remote-port req) (request-uri req)
  ;;  (acceptor-document-root acc))
  )

(defun comment-data-invalid? (comment email nick)
  (or (zerop (length comment))
      (zerop (length email))
      (and (not (zerop (length nick)))
	   (every #'(lambda (x) (char= x #\Space)) nick))
      (null (cl-ppcre:all-matches ".+@.+\..+" email))
      (> (length comment) 8196)
      (> (length email) 256)
      (> (length nick) 24)))
