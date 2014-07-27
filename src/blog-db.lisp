;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

;; Help:
;; 1. Delete all documents in collection:
;; db.collection.remove({})
;; 2. Find element by field in collection:
;; db.collection.find({tag : value})
;; 3. Show all databases:
;; show dbs
;; 4. Drop collection:
;; db.collection.drop()

(defvar *blog-mongo-db-host* "localhost")
(defvar *blog-mongo-db-port* 27017)
(defvar *blog-mongo-db*      "blog")

(defvar *db-user-collection*           "users")
(defvar *db-post-collection*           "posts")
(defvar *db-static-collection*         "static")
(defvar *db-comment-collection*        "comments")
(defvar *db-comment-author-collection* "authors")
(defvar *db-post-tag-collection*       "tags")
(defvar *db-blog-info-collection*      "bloginfo")

(defun doc-id-short (doc)
  "Fix for the cl-mongo way of handling OID's, when object isn't saved to DB
  its OID is actually UUID and is 16 bytes long, not 12 as it should.
  TODO: Patch cl-mongo properly to address this issue."
  (cl-mongo::_id (cl-mongo::_id doc)))

;; Makes it possible to use mongo connections in multithreaded environment
(defmacro with-blog-db (&body body)
  `(let ((cl-mongo::*mongo-registry* nil))
     (with-mongo-connection (:host *blog-mongo-db-host*
				   :port *blog-mongo-db-port*
				   :db *blog-mongo-db*)
	,@body)))

(defun blog-db-start ()
  "Start mongo DB connection"
  (mongo :name :blog-db :host *blog-mongo-db-host* :port *blog-mongo-db-port*
	 :db *blog-mongo-db*)
  ;; If theres no info entry in DB, create new one
  (unless (blog-db-info-get-recent)
    (blog-db-info-create-new :old-uuid nil)))

(defun blog-db-stop ()
  "Stop mongo DB connection"
  (mongo-close :blog-db))

(defun blog-db-count (collection)
  (with-blog-db
    (round (get-element "n" (car (docs (db.count collection :all)))))))

(defun blog-db-get-by-id (id class)
  (let ((doc (doc-find-by-oid *db-post-collection* (id-str-to-oid id))))
    (if doc (make-instance class :mongo-doc doc) nil)))

(defun blog-db/get-obj (oid collection class)
  "Get CLOS object using OID"
  (let ((doc (doc-find-by-oid collection oid)))
    (when doc
      (make-instance class :mongo-doc doc))))

(defclass blog-db-base ()
  ((db-doc :reader db-doc)
   (collection :reader db-collection
	       :initarg :collection)))

(defmethod initialize-instance :after ((obj blog-db-base) &key mongo-doc)
  (let ((slots (cddr (mapcar #'(lambda (x) (sb-pcl:slot-definition-name x))
			     (sb-pcl:class-slots (class-of obj))))))
    (when mongo-doc
      (mapcar #'(lambda (slot)
		  (setf (slot-value obj slot)
			(get-element (symbol-name slot) mongo-doc))) slots)
      (setf (slot-value obj 'db-doc) mongo-doc))))

(defgeneric blog-db/generate-doc (blog-db-base &key save))

(defmethod blog-db/generate-doc ((obj blog-db-base) &key save)
  "Generate mongo doc for blog post"
  (let ((slots (cddr (mapcar #'(lambda (x) (sb-pcl:slot-definition-name x))
			     (sb-pcl:class-slots (class-of obj)))))
	(doc (if (and (slot-boundp obj 'db-doc) (slot-value obj 'db-doc))
		 (db-doc obj) (make-document))))
    (mapcar #'(lambda (slot)
		(add-element (symbol-name slot) (slot-value obj slot) doc))
	    slots)
    (setf (slot-value obj 'db-doc) doc)
    (when save
      (with-blog-db (db.save (db-collection obj) (slot-value obj 'db-doc))))
    doc))

(defmethod print-object ((obj blog-db-base) stream)
  (with-slots (collection) obj
    (format stream
     "~a(~a), OID=~a~%" (type-of obj) collection
     (if (blog-db/have-doc obj) (blog-db/id-to-str obj) "None"))))

(defgeneric blog-db/delete (blog-db-base))

(defmethod blog-db/delete ((obj blog-db-base))
  "Delete blog object from database"
  (with-blog-db
    (when (blog-db/have-doc obj)
      (db.delete (db-collection obj) (db-doc obj)))))

(defgeneric blog-db/have-doc (blog-db-base))

(defmethod blog-db/have-doc ((obj blog-db-base))
  "Test if blog object has mongo doc generated"
  (when (and (slot-boundp obj 'db-doc) (slot-value obj 'db-doc)) t))

(defmethod blog-db/get-oid ((obj blog-db-base))
  "Get mongo document OID"
  (when (blog-db/have-doc obj)
    (doc-id-short (slot-value obj 'db-doc))))

(defmethod blog-db/get-bson-oid ((obj blog-db-base))
  (cl-mongo::make-bson-oid :oid (blog-db/get-oid obj)))

(defun doc-find-by-oid (collection oid)
  "Find mongo doc in `collection' using its OID"
  (with-blog-db
    (when oid
      (car (docs (db.find collection
			  (kv "_id" (cl-mongo::make-bson-oid :oid oid))))))))

(defmethod blog-db/id-to-str ((obj blog-db-base))
  "Convert mongo OID to a string representation"
  (id-oid-to-str (blog-db/get-oid obj)))

(defgeneric blog-db/insert-doc (blog-db-base))


(defclass blog-db-user (blog-db-base)
  ((collection :initform *db-user-collection*)
   (user :accessor username
	 :initarg :user)
   (hash :accessor pwhash
	 :initarg :hash)
   (salt :accessor pwsalt
	 :initarg :salt)
   (role :accessor user-role
	 :initarg :role
	 :initform '())))

;; 1. Concatenate user hash and object salt
;; 2. Get new hash
;; 3. Compare object hash and new hash

(defmethod blog-db-user/pwcheck ((obj null) hash))

(defmethod blog-db-user/pwcheck ((obj blog-db-user) hash)
  "Check that user entered valid password"
  (let ((upwhash (md5:md5sum-sequence
		  (concatenate
		   '(vector (unsigned-byte 8))
		   hash (list-to-array
			 (pwsalt obj) :type '(unsigned-byte 8))))))
    (compare-lists (pwhash obj) (array-to-list upwhash))))

(defun blog-db-user-add (user passwd)
  "Add new user to database"
  (let* ((salt (random-array 16 256 :type '(unsigned-byte 8)))
	 (tmphash (md5:md5sum-string passwd))
	 (user (make-instance
		'blog-db-user :user user
		:salt (array-to-list salt)
		:hash (array-to-list (md5:md5sum-sequence
				      (concatenate '(vector (unsigned-byte 8))
						   tmphash salt))))))
    (blog-db/generate-doc user :save t)
    user))

(defun blog-db-get-user (user)
  "Query user from DB using its username."
  (with-blog-db
      (let ((user (car (docs (db.find *db-user-collection* ($ "USER" user)
				      :limit 1)))))
	(when user
	  (make-instance 'blog-db-user :mongo-doc user)))))

(defclass blog-db-data (blog-db-base)
  ((collection :initform *db-static-collection*)
   (title-src :accessor get-title-src
	      :initarg :title-src
	      :initform "")
   (title-url :accessor get-title-url
	      :initform "")
   (text-src :accessor get-text-src
	     :initarg :text-src
	     :initform "")
   (text-html :accessor get-text-html
	      :initform "")
   (create-time :reader get-create-time
		:initform (get-universal-time))
   (edit-time :accessor get-edit-time
	      :initform (get-universal-time))))

(defmethod get-text-html ((obj null)))

(defgeneric blog-db-data/render (blog-db-base))

(defgeneric blog-db-data/url-convert (blog-db-data))

(defmethod blog-db-data/url-convert ((obj blog-db-data))
  ())

(defmethod blog-db-data/render ((obj blog-db-data))
  (setf (get-text-html obj) (render-html (get-text-src obj))
	(get-title-url obj) (hunchentoot:url-encode (get-title-src obj))))

(defclass blog-db-post (blog-db-data)
  ((collection :initform *db-post-collection*)
   (tags :initarg :tags
	 :accessor get-tags
	 :initform '())
   (comments :initarg :comments
	     :accessor comments-allowed?
	     :initform t)
   (feed-uuid :initarg :feed-uuid
	      :reader get-feed-uuid
	      :initform (format nil "~a" (uuid:make-v1-uuid)))))

(defmethod blog-db-post/tags-str ((obj blog-db-post))
  (list-to-str (get-tags obj) ""))

(defclass blog-db-comment (blog-db-base)
  ((collection :initform *db-comment-collection*)
   (author-id :initarg :author
	      :accessor comment-author)
   (post-id :initarg :post
	    :accessor comment-post)
   (nick :initarg :nick
	 :accessor comment-nick
	 :initform "anonymous")
   (data :initarg :data
	 :accessor get-comment)
   (hidden :initarg :hidden
	   :accessor hidden?
	   :initform t)
   (ip :initarg :ip
       :reader get-ip
       :initform "")
   (edit-time :reader get-edit-time
	      :initform (get-universal-time))))

(defmethod blog-db-comment/hide ((obj blog-db-comment) hide)
  (setf (hidden? obj) hide)
  (blog-db/generate-doc obj :save t))

(defmethod blog-db-comment/format ((obj blog-db-comment))
  "Prepare comment data to be shown to user."
  (string-replace (get-comment obj) (string #\Return) "<br>" :test #'string=))

(defclass blog-db-comment-author (blog-db-base)
  ((collection :initform *db-comment-author-collection*)
   (email :initarg :email
	  :reader author-email)
   (approved :initarg :approved
	     :initform t
	     :accessor approved?)))

(defmethod blog-db-comment-author/author-approved-switch
    ((obj blog-db-comment-author))
  "Switch approved state for comment author"
  (setf (approved? obj) (not (approved? obj))))


(defclass blog-db-info (blog-db-base)
  ((collection :initform *db-blog-info-collection*)
   (feed-root-uuid :initarg :feed-root-uuid
		   :reader get-feed-root-uuid
		   :initform (format nil "~a" (uuid:make-v1-uuid)))
   (feed-update-time :initarg :feed-update-time
		     :reader get-feed-update-time
		     :initform (get-universal-time))
   (timestamp :initarg :timestamp
	      :reader get-timestamp
	      :initform (get-universal-time))
   (posts-num :initarg :posts-num
	      :reader get-posts-num
	      :initform 0)
   (comments-num :initarg :comments-num
		 :reader get-comments-num
		 :initform 0)))

(defgeneric blog-db-info-update (obj &rest junk))

(defmethod blog-db-info-update ((obj blog-db-info)
				&key (posts nil) (comments nil) (update t))
  (with-slots (posts-num comments-num feed-update-time) obj
    (when posts
      (setf posts-num (blog-db-count *db-post-collection*)))
    (when comments
      (setf comments-num (blog-db-count *db-comment-collection*)))
    (when (and update (or posts comments))
      (setf feed-update-time (get-universal-time))
      (blog-db/generate-doc obj :save t))))

(defun blog-db-info-create-new (&key (old-uuid t) (old-update-time nil))
  (let* ((recent-info (blog-db-info-get-recent))
	 (comments (blog-db-count *db-comment-collection*))
	 (posts (blog-db-count *db-post-collection*))
	 (info (make-instance 'blog-db-info :posts-num posts
			      :comments-num comments)))
    (when old-uuid
      (setf (slot-value info 'feed-root-uuid)
	    (get-feed-root-uuid recent-info)))
    (when old-update-time
      (setf (slot-value info 'feed-update-time)
	    (get-feed-update-time recent-info)))
    (blog-db/generate-doc info :save t)))

(defun blog-db-info-get-recent ()
  (with-blog-db
    (let ((document
	   (docs (db.sort *db-blog-info-collection*
			  :all :asc nil :field "TIMESTAMP" :limit 1))))
      (when document
	(make-instance 'blog-db-info :mongo-doc (car document))))))

(defclass blog-db-post-tag (blog-db-base)
  ((collection :initform *db-post-tag-collection*)
   (tag :initarg :tag
	:accessor post-tag)
   (weight :initarg :weight
	   :accessor post-tag-weight
	   :initform 0)))

(defun blog-db-get-posts (off limit)
  "Get `limit' most recent posts from DB"
  (declare (fixnum limit)
	   (fixnum off)
	   (optimize (speed 3) (safety 0)))
  (with-blog-db
      (let ((documents
	     (docs (db.sort *db-post-collection*
			    :all :asc nil :field "CREATE-TIME"
			    :limit limit
			    :skip (* off limit)))))
	(the list (mapcar #'(lambda (x)
			      (make-instance 'blog-db-post :mongo-doc x))
			  documents)))))

(defun blog-db-get-posts-by-tag (off limit tag)
  "Get `limit' most recent posts from DB"
  (declare (fixnum limit)
	   (fixnum off)
	   (simple-string tag)
	   (optimize (speed 3) (safety 0)))
  (with-blog-db
    (let ((documents
	   (docs (db.sort *db-post-collection*
			  (if (= (length tag) 0)
			      :all ($all "TAGS" (list tag)))
			  :asc nil :field "CREATE-TIME"
			  :limit limit
			  :skip (* off limit)))))
      (the list (mapcar #'(lambda (x)
			    (make-instance 'blog-db-post :mongo-doc x))
			documents)))))

(defun blog-db-add-static (link)
  "Add static data, `link' should be unique"
  (let ((static (make-instance 'blog-db-data :title link)))
    (blog-db/generate-doc static :save t)))

;;(blog-db-add-static "about")

(defun blog-db-get-static (title)
  "Get static data by its title"
  (with-blog-db
    (let ((static-docs (docs (db.find *db-static-collection*
				      (if title ($ "TITLE" title) :all)))))
      (mapcar #'(lambda (x)
		  (make-instance 'blog-db-data :mongo-doc x)) static-docs))))

(defun blog-db-get-comment-author (email)
  (with-blog-db
    (let ((author (docs (db.find *db-comment-author-collection*
				 (if email
				     ($ "EMAIL" email) :all)
				 :limit 0))))
      (mapcar #'(lambda (x)
		  (make-instance 'blog-db-comment-author :mongo-doc x))
	      author))))

(defun blog-db-add-comment-author (email)
  (let ((author (make-instance 'blog-db-comment-author :email email)))
    (blog-db/generate-doc author :save t)
    author))

(defmacro blog-db-get-comments (&key post-id author-id)
  `(with-blog-db
     (let ((comments (docs (db.find *db-comment-collection*
       ($
	,@(when post-id
	    (list `($ "POST-ID" (cl-mongo::make-bson-oid :oid ,post-id))))
	,@(when author-id
	    (list `($ "AUTHOR-ID" (cl-mongo::make-bson-oid :oid ,author-id)))))
       :limit 0))))
       (mapcar #'(lambda (x)
		   (make-instance 'blog-db-comment :mongo-doc x))
	       comments))))

(defun blog-db-get-comments-num (post-id)
  (declare ((vector (unsigned-byte 8)))
	   (optimize (speed 3) (safety 0)))
  (with-blog-db
    (let* ((comments
	    (docs (db.find *db-comment-collection*
			   ($ "POST-ID" (cl-mongo::make-bson-oid :oid post-id))
			   :limit 0)))
	   (result
	    (reduce #'(lambda (x y)
			(let ((comment (make-instance
					'blog-db-comment :mongo-doc y)))
			  (if (hidden? comment)
			      x (cons y x)))) comments :initial-value '())))
      (declare (list comments result))
      (the fixnum (length result)))))

(defun blog-db-add-comment (comment nick post-id author-id ip approved)
  (let ((comment (make-instance 'blog-db-comment
				:author (cl-mongo::make-bson-oid :oid author-id)
				:post (cl-mongo::make-bson-oid :oid post-id)
				:nick nick :data comment :ip ip
				:hidden (not approved))))
    (blog-db/generate-doc comment :save t)
    comment))

(defun blog-db-change-approved-status (author)
  (blog-db-comment-author/author-approved-switch author)
  (blog-db/generate-doc author :save t))

(defun update-fill-create-time ()
  (dolist (post (blog-db-get-posts 0))
    (setf (slot-value post 'create-time) (get-edit-time post))
    (blog-db/generate-doc post :save t)))


(defun update-fill-comment-ip (ip)
  (with-blog-db
    (let ((documents (docs (db.find *db-comment-collection* :all :limit 0))))
      (mapcar #'(lambda (x)
		  (let ((comment (make-instance 'blog-db-comment
						:mongo-doc x)))
		    (setf (slot-value comment 'ip) ip)
		    (blog-db/generate-doc comment :save t)))
	      documents))))

(defun upgrade-entries-for-title ()
  (with-blog-db
    (let ((all-docs (docs (db.find *db-post-collection* :all :limit 0))))
      (mapcar #'(lambda (x)
		  (let ((post (make-instance 'blog-db-post
					     :mongo-doc x)))
		    (when (or (null (get-title-src post))
			      (string= (get-title-url post) ""))
		      (setf
		       (get-title-src post) (get-element "TITLE" x)
		       (get-title-url post) (hunchentoot:url-encode
					     (get-title-src post))))
		    (print (get-element "TITLE" x))
		    (print (get-title-url post))
		    (blog-db/generate-doc post :save t)))
	      all-docs))))


;;(upgrade-entries-for-title)


;; (time
;;  (with-blog-db
;;    (docs (db.find *db-comment-collection*
;; 		  ($ "POST-ID"
;; 		     (cl-mongo::make-bson-oid
;; 		      :oid (id-str-to-oid "8db3ad73ecb041198467112d")))
;; 		  :limit 0))))
;; (time (blog-db-get-comments-num (id-str-to-oid "8db3ad73ecb041198467112d")))

;; (let (var)
;;   (loop for x from 0 to 1000 do
;;        (sleep 0.1)
;;        (setf var (make-instance
;; 		  'blog-db-post
;; 		  :title (concatenate 'string "Test title " (stringify x))
;; 		  :text-src "Some text"))
;;        (blog-db-data/render var)
;;        (blog-db/generate-doc var)
;;        (blog-db/save var)))


;;(describe (blog-db-user-add "user" "password"))
;;(setq test (blog-db-get-user "user"))
;;(time (blog-db-user/pwcheck test (md5:md5sum-string "password")))
