;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defvar *resource-uri* '("^/img/" "^/css/" "^/eterhost.js" ))
(defvar *client-bots* '("YandexBlogs" "YandexBot" "bingbot" "Googlebot" "ZmEu"
			"NerdyBot" "Yahoo!" "Mail.Ru bot" "Baiduspider"
			"Feedly" "ApacheBench" "Morfeus Fucking Scanner"))

(defvar *db-log-report-collection* "logreport")
(defvar *db-log-entry-collection*  "logentry")

(defvar *log-regex* "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}) - \\[([^\\]]+)\\] \"([^\\\"]+)\" (\\d+|-) (\\d+|-) \"([^\\\"]*)\" \"([^\\\"]*)\"")

(defclass blog-db-log-report (blog-db-base)
  ((collection :initform *db-log-report-collection*)
   (start-time :initarg :start-time
	       :reader get-start-time)
   (end-time :initarg :end-time
	     :reader get-end-time)
   (gen-time :initarg :gen-time
	     :reader get-gen-time
	     :initform (get-universal-time))
   (hits :initarg :total-hits
	 :accessor get-total-hits
	 :initform 0)
   (dsize :initarg :download-size
	  :accessor get-download-size
	  :initform 0)
   (usize :initarg :upload-size
	  :accessor get-upload-size
	  :initform 0)
   (access-type :initarg :access-type
		:reader get-access-type
		:initform "HTTP")))

(defclass blog-db-log-entry (blog-db-base)
  ((collection :initform *db-log-entry-collection*)
   (report-id :initarg :report-id
	      :reader get-report-id)
   (addr :initarg :addr
	 :reader get-addr)
   (user-agent :initarg :user-agent
	       :reader get-user-agent)
   (url :initarg :url
	:reader get-url)
   (http-code :initarg :http-code
	      :reader get-http-code)
   (http-method :initarg :http-method
		:reader get-http-method)
   (size :initarg :size
	 :reader get-size
	 :initform 0)
   (timestamp :initarg :timestamp
	      :reader get-timestamp)))

(defmethod blog-db-log-report/name ((obj blog-db-log-report))
  (format-time  (get-gen-time obj)
		:format '((:year 4) #\- (:month 2) #\- (:day 2) #\/
			  (:hour 2) #\: (:min 2))))

(defmethod http-code-ok ((obj blog-db-log-entry))
  (string= (get-http-code obj) "200"))

(defmethod http-code-not-found ((obj blog-db-log-entry))
  (string= (get-http-code obj) "404"))

(defmethod stat-significant-uri ((obj blog-db-log-entry))
  (loop for x in *resource-uri*
        for res = (cl-ppcre:all-matches x (get-url obj))
        while (null res)
     finally (return (null res))))

(defmethod entry-not-bot ((obj blog-db-log-entry))
  (loop for str in *client-bots*
        for res = (search str (get-user-agent obj))
        while (null res)
       finally (return (null res))))

(defmacro with-blog-db-log-entries ((var report-id) &body body)
  `(with-blog-db
     (loop for doc in
	  (docs (iter (db.find *db-log-entry-collection*
			       ($ "REPORT-ID" (cl-mongo::make-bson-oid
					       :oid ,report-id)) :limit 0)))
	for ,var = (make-instance 'blog-db-log-entry :mongo-doc doc) do
	  ,@body)))

(defun blog-db-log-report-delete (oid)
  (with-blog-db
    (let ((report (blog-db/get-obj
		   oid *db-log-report-collection* 'blog-db-log-report)))
      (when report
	(with-blog-db-log-entries (var oid)
	  (blog-db/delete var))
	(blog-db/delete report)))))

(defun blog-db-log-report-get (&key (limit 60))
  (with-blog-db
    (let ((documents
	   (docs (db.sort *db-log-report-collection*
			  :all :asc nil :field "GEN-TIME" :limit limit))))
	(mapcar #'(lambda (x)
		    (make-instance 'blog-db-log-report :mongo-doc x))
		documents))))

(defun blog-db-log-report-generate (log-path access-type
				    &key start-time end-time)
  (let ((report (make-instance 'blog-db-log-report
			       :start-time (get-universal-time)
			       :end-time (get-universal-time)
			       :access-type access-type))
	oid)
    (blog-db/generate-doc report :save t)
    (setq oid (blog-db/get-oid report))
    (multiple-value-bind (tbl inv inv-str)
	(hunchentoot-log-parse log-path nil nil)
      (hunchentoot-log-table-proc tbl 'blog-db-log-entry-new oid))
;;    (describe report)
    (with-blog-db-log-entries (var oid)
      (cond ((string= (get-http-method var) "POST")
	     (setf (get-upload-size report)
		   (+ (get-upload-size report) (get-size var))))
	    ((string= (get-http-method var) "GET")
	     (setf (get-download-size report)
		   (+ (get-download-size report) (get-size var)))))
      (incf (get-total-hits report)))
    (blog-db/generate-doc report :save t)))

(defun hunchentoot-log-parse (fpath start end)
  (let ((scanner (cl-ppcre::create-scanner *log-regex*))
	(tbl (make-hash-table :test 'equal))
	(inv 0) inv-str)
    (with-open-file (input fpath)
      (do ((line (read-line input nil)
               (read-line input nil)))
        ((null line))
	(multiple-value-bind (str vec)
	    (cl-ppcre:scan-to-strings scanner line)
	  (declare (ignore str))
	  (if vec
	    (push (subseq vec 1) (gethash (elt vec 0) tbl))
	    (progn
	      (incf inv)
	      (push line inv-str))))))
    (values tbl inv inv-str)))

(defun hunchentoot-log-hash-entry-who (entry)
  (filter-list (loop for x in entry collect (elt x 5)) '()))

(defun hunchentoot-log-hash-entry-what (entry)
  (filter-list (loop for x in entry collect (elt x 1)) '()))

(defun hunchentoot-log-table-proc (hash func &rest params)
  (loop for key being the hash-keys of hash using (hash-value value)
     for x = 0 then (1+ x) do
       (funcall func key value params)
     finally (return x)))

(defun stat-get-post-name (query)
  (multiple-value-bind (str vec)
      (cl-ppcre:scan-to-strings "id=([A-Z0-9]{24})" query)
    (if vec
	(concatenate
	 'string "Post ID: " (elt vec 0) "; ("
	 (let ((post (blog-db-get-by-id (elt vec 0) 'blog-db-post)))
	   (if post
	       (hunchentoot:escape-for-html (get-title-src post))
	       "Not found")) ")")
	(concatenate 'string "Unknown post query: " query))))

(defun stat-get-tag (query)
  (multiple-value-bind (str vec)
      (cl-ppcre:scan-to-strings "tag=([^,&]+)" query)
    (if vec
	(concatenate 'string "Tag: " (hunchentoot:url-decode (elt vec 0)))
	(concatenate 'string "Unknown post tag query: " query))))

(defun blog-db-log-top-hits (report-id &key (limit 0))
  (let ((tbl (make-hash-table :test 'equal))
	(oid (id-str-to-oid report-id)))
    (with-blog-db-log-entries (var oid)
      (when (and (http-code-ok var)
		 (stat-significant-uri var)
		 (entry-not-bot var))
	(multiple-value-bind (uri query) (strip-url-params (get-url var))
	  (cond ((string= uri "/post")
		 (incf (gethash (stat-get-post-name query) tbl 0)))
		((string= uri "/tag")
		 (incf (gethash (stat-get-tag query) tbl 0)))
		(t
		 (incf (gethash uri tbl 0)))))))
    (when (zerop limit)
      (setq limit (hash-table-count tbl)))
    (sort
     (loop for key being the hash-keys of tbl using (hash-value value)
	   for x = 0 then (1+ x)
	   while   (< x limit)
	   collect (list key value))
     #'(lambda (x y) (> (cadr x) (cadr y))))))

(defun blog-db-log-entry-new (addr val oid)
  (loop for x in val
     for res = (split-str (elt x 1) #\Space)
     for obj = (make-instance
		'blog-db-log-entry :addr addr
		:report-id (cl-mongo::make-bson-oid :oid (car oid))
		:addr addr :user-agent (elt x 5)
		:url (cadr res) :http-code (elt x 2)
		:http-method (car res)
		;; In case size is "-"
		:size (if (string/=  (elt x 3) "-") (parse-integer (elt x 3)) 0)
		:timestamp (hunchentoot-log-time-to-unix (elt x 0))) do
       (blog-db/generate-doc obj :save t)))

;; (loop for x from 0 to 10 do
;;      (time (blog-db-log-report-generate
;; 	    "/tmp/eterhost-site-access.log" "HTTP"))
;;      (format t "--> ~a~%" x))
