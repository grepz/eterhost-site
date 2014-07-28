;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defparameter *blog-posts-per-page* 15
  "Show that much posts per blog page, if zero - unlimited")
(defparameter *web-log-report-per-page* 50)

;; Atom feed `updated' tag format
(defvar *feed-time-format* '((:year 4) #\- (:month 2) #\- (:day 2)
			     #\T (:hour 2) #\: (:min 2) #\: (:sec 2) #\Z))

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
			    (:a :href "about" (fmt "~a" *blog-author*))))
		      (:ul
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
		(:h1 (fmt "~a" name))
		(:div :class "navigation"
		      (fmt "~a" (blog-navigation *blog-nav-list* " ")))))))

(defmacro blog-page ((&key title name page-nav) &body body)
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
		      ,@body
		      (when ,page-nav
			(fmt "~a" (page-navigation
				   (first ,page-nav) (second ,page-nav)
				   (third ,page-nav)))))
		(:div :class "separater")
		(fmt "~a" (blog-footer))))))

(defun blog-post-comment (comment &key (admin nil))
  "Print comment html data."
  (with-html ()
    (:div :id "comment-box"
	  (:div :class "comment-head"
		(:h4
		 (fmt "~a"
		      (concatenate
		       'string "By \"" (comment-nick comment) "\" on "
		       (format-time (get-edit-time comment)))))
		(when admin
		  (htm
		   (:h4
		    (concatenate 'string " (Visible:"
				 (if (hidden? comment) "no" "yes") ")"))
		   (:a :href
		       (cl-who:escape-string
			(format
			 nil "/admin/edit-data?type=comment&id=~a&act=del"
			 (blog-db/id-to-str comment)))
		       "Delete"))))
	  (fmt "~a" (blog-db-comment/format comment))
    (:br))))

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
	   (htm (:a :href (format nil "post/~a/~a"
				  id (get-title-url post))
		    (fmt "~a" (hunchentoot:escape-for-html
			       (get-title-src post)))))
	   (fmt "~a" (hunchentoot:escape-for-html (get-title-src post)))))
      ;; Show post html content
      (:div :class "post-content"
	    (fmt "~a" (get-text-html post)))
      (when tags
	(fmt "~a" (blog-post-tags tags)))
      (:br)
      ;; Post menu
      (:div :class "post-menu"
	    ;; edit time goes first
	    (:b (fmt "Updated on ~a"
		     (format-time (get-edit-time post))))
	    ;; If comments parameter is provided and comments are allowed for
	    ;; the post, generate a link leading to comments section
	    (when (and comments (comments-allowed? post))
	      (htm (:div :class "link-comment"
			 (:a :href (format nil "post/~a/~a#comments"
					   id (get-title-url post))
			     (fmt "~a"
				  (blog-db-get-comments-num
				   (blog-db/get-oid post)))))))
	    ;; If user is logged as admin, show additional controlling options
	    ;; such as edit and delete link
	    (when admin
	      (htm
	       (:div :class "link-edit"
		(:a :href
		 (cl-who:escape-string
		  (format nil "/admin/edit-data?type=post&id=~a&act=edit" id))
		 (fmt "Edit")))
	       (:div :class "link-delete"
		(:a :href
		 (cl-who:escape-string
		  (format nil "/admin/edit-data?type=post&id=~a&act=del" id))
		 (fmt "Delete")))))))))

(defun page-navigation (cur total base-link)
  ;; When cur is nil, assume it is first page
  (when (or (null cur) (zerop cur))
    (setq cur 1))
  (with-html ()
    (:div :id "page-nav"
	  ;; Show link to the first page if we are further then 2 pages away
	  (when (> cur 2)
	    (htm (:a :href (concatenate 'string base-link "?pg=1")
		     "First")))
	  ;; If we have previous pages, show 'previous' link
	  (when (> cur 1)
	    (htm (:a :href (concatenate 'string base-link
					(format nil "?pg=~a" (1- cur)))
		     "Previous")))
	  ;; Show link to the next page if there are more pages ahead
	  (when (< cur total)
	    (htm (:a :href (concatenate 'string base-link
					(format nil "?pg=~a" (1+ cur)))
		     "Next")))
	  ;; Show link to the last page if there are more then 1 pages and we
	  ;; can't access last page by using 'next' link
	  (when (and (>= total 2) (<= cur (- total 2)))
	    (htm (:a :href (concatenate 'string base-link
					(format nil "?pg=~a" total))
		     "Last"))))))

(define-easy-handler (javascript :uri "/eterhost.js") ()
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
    ;; (defun post-preview (form)
    ;;   (form.set-attribute "target" "_blank"))
    ;; (defun post-submit (form)
    ;;   (form.remove-attribute "target"))
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
  (let* ((page-param (hunchentoot:get-parameter "pg"))
	 (page (if page-param (parse-integer page-param :junk-allowed t) 0))
	 (last-page (ceiling (/ (blog-db-count
				 *db-post-collection*)
				*blog-posts-per-page*))))
  (blog-page
      (:title "EterHost.org" :name "EterHost.org - Blog"
       :page-nav (list page last-page "/"))
      (:div :class "data-column"
       (:div :id "content"
	(dolist (post (blog-db-get-posts
		       (if (< page 0) 0 (if (> page 0) (1- page) page))
		       *blog-posts-per-page*))
	 (fmt "~a"
	      (htmlize-blog-post
	       post :admin (hunchentoot:session-value :auth)
	       :title-link t :comments t))))))))

(define-easy-handler (blog-posts-tag :uri "/tag") ()
  (let ((tag (hunchentoot:get-parameter "tag")))
    (blog-page
	(:title "EterHost.org - Tag" :name "EterHost.org - Blog")
      (:div :class "data-column"
       (:div :id "content"
	(dolist (post (blog-db-get-posts-by-tag 0 0 tag))
	  (fmt "~a"
	       (htmlize-blog-post
		post :admin (hunchentoot:session-value :auth)
		:title-link t :comments t))))))))

(defun page-handler-post ()
  (let (id-str id doc data)
    (multiple-value-bind (str vec)
	(cl-ppcre:scan-to-strings
	 "^post/([A-Z0-9]{24})/(.+)$"
	 (namestring (hunchentoot:request-pathname)))
      (when (null str)
	(redirect "/"))
      (setf id-str (aref vec 0)
	    id (id-str-to-oid id-str)
	    doc (doc-find-by-oid *db-post-collection* id))
      ;; XXX: Redirect to 404?
      (when (null doc)
	(redirect "/"))
      (setf data (make-instance 'blog-db-post :mongo-doc doc))
      (when (string/= (get-title-src data) (aref vec 1))
	(redirect "/")))
    (blog-page
	(:title (concatenate 'string "EterHost.org - "
			     (hunchentoot:escape-for-html (get-title-src data)))
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
			    :id "fcomment"
			    :onsubmit (ps (comment-submit-check))
			    :method :post
			    (:label (:span "Email:")
				    (:input :type :text :name "email"))
			    (:label (:span "Name:")
				    (:input :type :text :name "nick"))
			    (:br)
			    (:label (:span "Comment")
				    (:textarea :id "content" :name "comment"))
			    (:input :type :submit :value "Submit")
			    (:input :type :hidden :id "content"
				    :value id-str :name "comment_post_id")
			    (:input :type :hidden :id "content"
				    :value (get-title-url data)
				    :name "comment_post_title"))
		     (:h2 "Comments:")
		     (:div :id "comments" (:a :name "comments")
		      (dolist (comment (blog-db-get-comments :post-id id))
		      	(when (or (not (hidden? comment))
		      		  (hunchentoot:session-value :auth))
		      	  (fmt "~a"
		      	   (blog-post-comment comment
			    :admin (hunchentoot:session-value :auth)))))))))))))

(define-easy-handler (comment :uri "/comment") ()
  (let* ((email      (post-parameter "email"))
	 (nick       (post-parameter "nick"))
	 (comment    (post-parameter "comment"))
	 (post-id    (post-parameter "comment_post_id"))
	 (post-title (post-parameter "comment_post_title"))
	 (addr       (hunchentoot:remote-addr*))
	 author)
    (assert (or (string/=  post-id "")
		(not (comment-data-invalid? comment email nick))))
    (setf author (or (car (blog-db-get-comment-author email))
		     (blog-db-add-comment-author email)))
    (blog-db-add-comment
     (hunchentoot:escape-for-html comment)
     (if (zerop (length nick)) "anonymous" (hunchentoot:escape-for-html nick))
     (id-str-to-oid post-id) (blog-db/get-oid author) addr (approved? author))
    ;; Update blog info information without updating feed `updated' tag
    (blog-db-info-update (blog-db-info-get-recent) :comments t :update nil)
    (redirect (format nil "/post/~a/~a" post-id post-title))))

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
	(:li (:a :href (cl-who:escape-string
			"/admin/edit-data?type=post&act=edit") "New post"))
	(:li (:a :href "/admin/edit-static"   "Edit static"))
	(:li (:a :href "/admin/edit-comments" "Comments"))
	(:li (:a :href "/admin/statistic"     "Site statistic")))))))

(define-easy-handler (admin-statistic :uri "/admin/statistic") ()
  (with-authentication
    (let* ((page-param (hunchentoot:get-parameter "pg"))
	   (page (if page-param (parse-integer page-param :junk-allowed t) 0))
	   (last-page (ceiling (/ (blog-db-count
				   *db-log-report-collection*)
				  *web-log-report-per-page*)))
	   (reports (blog-db-log-report-get
		     (if (< page 0) 0 (if (> page 0) (1- page) page))
		     :limit *web-log-report-per-page*)))
      (blog-page (:title "EterHost.org - Admin" :name "EterHost.org - Admin"
		  :page-nav (list page last-page "/admin/statistic"))
	(:div :id "static-content"
	      (:a :href "/admin" "Back")
	      (:h1 "Reports:")
	      (:table
	       (:tr
		(:th "Report")
		(:th "Type") (:th "Start") (:th "End")
		(:th "Hits") (:th "Down") (:th "Up")
		(:th "Del"))
	       (dolist (report reports)
		 (htm
		  (:tr
		   (:td (:a :href (format nil "/admin/statistic/report?id=~a"
					  (blog-db/id-to-str report))
			    (fmt "~a" (blog-db-log-report/name report))))
		   (:td (fmt "~a" (get-access-type report)))
		   (:td (fmt "~a" (format-time (get-start-time report))))
		   (:td (fmt "~a" (format-time (get-end-time report))))
		   (:td (fmt "~a" (get-total-hits report)))
		   (:td (fmt "~,2fMB"
			     (/ (get-download-size report) 1048576)))
		   (:td (fmt "~,2fMB"
			     (/ (get-upload-size report) 1048576)))
		   (:td (:a :href
			    (format nil "/admin/statistic/report?id=~a&del=1"
				    (blog-db/id-to-str report)) "x")))))))))))

(define-easy-handler (admin-statstic-report :uri "/admin/statistic/report") ()
  (with-authentication
    (let ((id-param  (hunchentoot:get-parameter "id"))
	  (del       (hunchentoot:get-parameter "del")))
      (assert (not (zerop (length id-param))))
      (when del
	(blog-db-log-report-delete (id-str-to-oid id-param))
	(redirect "/admin/statistic"))
      (blog-page (:title "EterHost.org - Admin" :name "EterHost.org - Admin")
	(:div :id "static-content"
	      (:a :href "/admin/statistic" "Back")
	      (:h1 "Links(hits)")
	      (:table
	       (:tr
		(:th "link") (:th "Total hits")
		(dolist (entry (blog-db-log-top-hits id-param))
		  (htm
		   (:tr
		    (:td (fmt "~a" (car entry)))
		    (:td (fmt "~a" (cadr entry)))))))))))))

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
	    (:a :href (cl-who:escape-string
		       (format nil "/admin/edit-data?id=~a&act=edit&type=static"
			       (blog-db/id-to-str data)))
		(fmt "~a" (hunchentoot:escape-for-html
			   (get-title-src data))))))))
       (:br)
       (:a :href "/admin" "Return")))))

(define-easy-handler (edit-data :uri "/admin/edit-data") ()
  (with-authentication
    (let* ((id-param  (hunchentoot:get-parameter "id"))   ;; OID
	   (act-type  (hunchentoot:get-parameter "act"))  ;; edit/delete
	   (data-type (hunchentoot:get-parameter "type")) ;; static/post/comment
	   data id)
      ;; Check parameters
      (when (or (null data-type) (null act-type))
	(redirect "/"))
      (when id-param
	(setf id (id-str-to-oid id-param)))
      ;; de-serialize DB data
      (setf data
	    (cond ((string= data-type "post")
		   ;; TODO: Do stuff using blog-db-get-by-id?
		   (make-instance 'blog-db-post :mongo-doc
				  (doc-find-by-oid *db-post-collection* id)))
		  ((string= data-type "static")
		   (make-instance 'blog-db-data :mongo-doc
				  (doc-find-by-oid *db-static-collection* id)))
		  ((string= data-type "comment")
		   (make-instance 'blog-db-comment :mongo-doc
				  (doc-find-by-oid *db-comment-collection* id)))
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
					    :value (get-title-src data)))
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
;;				    :onclick (ps (post-submit this.form))
				    :value "Submit")
			    (:input :type :submit :name "data_submit"
;;				    :onclick (ps (post-preview this.form))
				    :value "Check")))))
	    ((string= act-type "del")
	     ;; Delete comments that belong to post
	     ;; Delete post
	     (when (string= data-type "post")
	       (dolist (comment (blog-db-get-comments :post-id id))
		 (blog-db/delete comment)))
	     (blog-db/delete data)
	     (redirect "/"))))))

(define-easy-handler (edit-check :uri "/admin/edit-data-submit") ()
  (with-authentication
    (let ((title-src (post-parameter "title"))
	  (text-src  (remove #\Linefeed (post-parameter "content")))
	  (tags      (post-parameter "tags"))
	  (action    (hunchentoot:get-parameter "data_submit"))
	  (data      (hunchentoot:session-value :dataref)))
      (assert data)
      ;; TODO: check title for invalid characters
      (setf (get-title-src data) title-src
	    (get-text-src data)  text-src
	    (get-edit-time data) (get-universal-time))
      (when (/= (length tags) 0)
	(setf (get-tags data) (parse-tags tags)))
      ;; Render to html format
      (blog-db-data/render data)
      (blog-db/generate-doc data :save t)
      ;; Update blog info + update feed `updated' tag
      (blog-db-info-update (blog-db-info-get-recent) :posts t)
      (redirect (format nil "/post/~a/~a"
			(id-oid-to-str (blog-db/get-oid data))
			(get-title-url data))))))

;; (cl-mongo::make-elements 40)
;; (doc-id (make-document))
;; (type-of (make-document))
;; (cl-mongo::_id (cl-mongo::_id (make-document)))
;; (doc-id-short (db-doc (car (blog-db-get-posts 0))))
;; (cl-mongo::make-bson-oid)

;; (doc-id-short (db-doc (car (blog-db-get-posts 0))))
;; (doc-id (db-doc (car (blog-db-get-posts 0))))
;; (doc-id (make-document))
;; (doc-id (make-document))
;; (doc-id (db-doc (car (blog-db-get-posts 0))))

;; (gethash "_id" (cl-mongo::elements (make-document)))
;; (gethash "_id" (cl-mongo::elements (db-doc (car (blog-db-get-posts 0)))))

;; (loop for key being the hash-keys of (cl-mongo::elements (db-doc (car (blog-db-get-posts 0))))
;;    using (hash-value value do
;; 		  (format t "~a:~a~%" key value)))

;; ;; 1.
;; (setq test (make-document))
;; ;; 2.
;; (doc-id test)
;; ;; 3.
;; (db.save "test" test)
;; ;; 4.
;; (doc-id (car (docs (db.find "test" :all))))

;; ;;
;; (cl-mongo::bson-encode-container (kv "_id" (cl-mongo::_id test)))
;; (cl-mongo::bson-encode-container test)

;; (cl-mongo::add-octets (cl-mongo::_id (cl-mongo::_id test)) (cl-mongo::make-octet-vector 100))

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
  (let ((info (blog-db-info-get-recent)))
    (when info
      (with-atom-xml ("http://eterhost.org/feed"
		      (format-time (get-feed-update-time
				    (blog-db-info-get-recent))
				   :format *feed-time-format*)
		     :title "Grepz Blog" :link-alt "http://eterhost.org"
		     :subtitle "Grepz's internet hut."
		     :id (get-feed-root-uuid info))
	(dolist (post (blog-db-get-posts 0 10))
	  (fmt "~a" (atom-xml-entry
		     (hunchentoot:escape-for-html (get-text-html post))
		     ;; XXX: Shall I use html escaped title here or url encoded?
		     :title (hunchentoot:escape-for-html (get-title-src post))
		     :id (get-feed-uuid post)
		     :entry-link (blog-post-gen-link
				  *blog-hostname* (blog-db/id-to-str post)
				  (get-title-url post))
		     :updated (get-edit-time post))))))))

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
