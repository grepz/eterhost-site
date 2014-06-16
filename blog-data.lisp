;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defvar *elephant-store* nil
  "DB store instance.")

(defvar *blog-posts* nil
  "Instance representing blog in database.")

(defvar *blog-store*
  '(:clsql (:sqlite3 #p"/tmp/blog.db"))
  ;;  '(:bdb "/tmp/blogdb/")
  )

(defun start-db ()
  (open-store *blog-store*)
  (setq *blog-posts*
	(or (get-from-root "blog-posts")
	    (let ((blog-posts (make-pset)))
	      (add-to-root "blog-posts" blog-posts)
	      blog-posts))))

(defun stop-db ()
  (close-store))

(defun get-new-id (class accessor)
  "Get next incremental ID for the `class' instance."
  (let ((ids (mapcar accessor
		     (copy-list (get-instances-by-class class)))))
    (if ids
	(1+ (reduce #'max ids))
	0)))

(defpclass comment-author ()
  ((email :initarg :email
	  :accessor author-email
	  :index t)
   (id :initarg :id
       :accessor author-id
       :index t)
   (approved :initarg :approved
	     :accessor approved
	     :initform nil)))

(defmethod comment-author-approved/switch ((obj comment-author))
  "Switch approved state for comment author"
  (if (approved obj)
      (setf (approved obj) nil)
      (setf (approved obj) t)))

(defmethod comment-author-approved/change ((obj comment-author) approve)
  "Change approved state, `approve' can be nil or t"
  (setf (approved obj) approve))

(defmethod print-object ((obj comment-author) stream)
  (format stream
	  "[~a]: Email='~a'; Author-id=~a; Approved=~a;"
	  (type-of obj) (author-email obj) (author-id obj) (approved obj)))

(defun comment-author/add (email)
  "Add new comment author using unique email"
  (let ((author (make-instance 'comment-author
			       :email email
			       :id (get-new-id 'comment-author 'author-id))))
    (insert-item author *blog-posts*)))

(defun comment-author/get (email)
  "Get comment author object by email reference"
  (car (elephant:get-instances-by-value 'comment-author 'email email)))

(defun comment-author/list ()
  "Return list of all comment authors in DB."
  (loop for author in (get-instances-by-class 'comment-author) do
       (format t "~a~%" author)))

(defpclass comment-data ()
  ((id :initarg :id
       :accessor comment-id)
   (data :initarg :data
	 :accessor get-comment)
   (author :initarg :comment-author
	   :accessor comment-author
	   :index t)
   (post :initarg :comment-post
	 :accessor comment-post
	 :index t)
   (nick :initarg :nick
	 :accessor nick)
   (shown :initarg :shown
	  :accessor shown
	  :initform nil)
   (timestamp :initform (get-universal-time)
	      :accessor timestamp)))

;; (map-class #'print-friend 'friend)
;;       name: Carlos birthdate: (1 1 1972)
;;       name: Adriana birthdate: (24 4 1980)
;;       name: Zaid birthdate: (14 8 1976)

(defun blog-comment-get-newest (&optional (num 5))
  )

;; TODO: Same as get newest comments
(defun blog-post-get-newest (&optional (num 5))
  (let* ((posts (copy-list (get-instances-by-class 'blog-data-post)))
	 (len (length posts)))
    (if (zerop len)
	(list (make-instance 'blog-data-post :title "__STUB__"
			     :content-src "" :hidden t :id 0))
	(subseq
	 (sort posts #'(lambda (x y) (and (not (hidden x))
			       (> (timestamp x) (timestamp y)))))
	 0 (if (< len num) len num)))))

(defmethod print-object ((obj comment-data) stream)
  (format stream
	  "[~a] Post=~a;Author=~a;Nick=~a;Time=~a;Comment:'~a'~%"
	  (comment-id obj) (comment-post obj) (comment-author obj) (nick obj)
	  (format-timestring nil (universal-to-timestamp (timestamp obj))
			     :format +rfc-1123-format+)
	  (get-comment obj)))

(defmethod comment-data/format ((obj comment-data))
  "Prepare comment data to be shown to user."
  (string-replace (get-comment obj) (string #\Return) "<br>"
		  :test #'string=))

(defun comment-data/add (data nick post-id author-id shown)
  "Add new comment."
  (insert-item
   (make-instance 'comment-data
		  :data (hunchentoot:escape-for-html data)
		  :id (get-new-id 'comment-data 'comment-id)
		  :comment-author author-id
		  :nick (if (null nick) "anonymous"
			    (hunchentoot:escape-for-html nick))
		  :shown shown
		  :comment-post post-id)
   *blog-posts*)
  (when shown
    ;; If comment author is approved increment comment count
    (incf (comments-num (blog-data/get-by-id post-id 'blog-data-post)))))

(defun comment-data/get (post-id)
  )

(defpclass blog-data ()
  ((id :initarg :id
       :accessor data-id)
   (title :initarg :title
	  :type string
	  :accessor title
	  :index t)
   (content-src :initarg :content-src
		:type string
		:accessor content-src)
   (content-htm :initarg :content-htm
		:type string
		:accessor content-htm)
   (timestamp :initform (get-universal-time)
	      :accessor timestamp)))

(defpclass blog-data-content (blog-data)
  ((hidden :initarg :hidden
	   :accessor hidden
	   :initform nil)
   (tags :type list
	 :accessor tags)))

(defpclass blog-data-post (blog-data-content)
  (
   ;; (comments :initarg :comments
   ;; 	     :initform '()
   ;; 	     :accessor comments)
   (comments-num :initarg :comments-num
		 :initform 0
		 :accessor comments-num)))

(defpclass blog-data-article (blog-data-content)
  ())

(defpclass blog-data-static (blog-data)
  ((permalink :initarg :permalink
	      :type string
	      :accessor permalink)))

(defmethod blog-data/render ((data blog-data))
  "Render markup data to a html format, save rendered data in html slot"
  (let ((doc (cl-markdown:markdown (content-src data))))
    (setf (content-htm data) (cl-markdown:render-to-stream doc :html nil))))

(defun blog-data/get-by-id (id class)
  "Get `class' instance having particular `id'"
  (if (and id class)
      (find id (copy-list (get-instances-by-class class))
	    :test #'(lambda (x y) (= x (data-id y))))
      nil))

(defun blog-article-add (title content)
  (unless (or (string= title "")
	      (string= content ""))
    (with-transaction ()
      (if (null
	   (find title (copy-list (get-instances-by-class 'blog-data-article))
		 :test #'(lambda (x y) (string= x (title y)))))
	  (let ((article (make-instance
			  'blog-data-article :title title
			  :content-src content
			  :id (get-new-id 'blog-data-article 'data-id))))
	    (blog-data/render article)
	    (insert-item article *blog-posts*))
	  (progn
	    ;; TODO: Tell user that article title is not unique
	    )))))

(defun blog-post-add (title content &key (hidden nil) (tags '()))
  (unless (or (string= title "")
	      (string= content ""))
    (let ((post (make-instance 'blog-data-post
			       :title title
			       :content-src content
			       :hidden hidden
			       :id (get-new-id 'blog-data-post 'data-id)
			       :tags tags)))
      (blog-data/render post)
      (insert-item post *blog-posts*))))

;; TODO: Get posts by pages
(defun blog-posts/get ()
  (let ((posts
	 (remove-if 'hidden
		    (copy-list (get-instances-by-class 'blog-data-post)))))
    (sort posts #'> :key #'timestamp)))

(defun blog-get-static (link)
  (let ((data (find link
		    (copy-list (get-instances-by-class 'blog-data-static))
		    :test #'(lambda (x y)
			      (string= x (permalink y))))))
    (unless (null data)
      (content-htm data))))

(defun comments-num (post-id)
  (let ((num 0)
	(comments (get-instances-by-value 'comment-data 'post post-id)))
    (loop for comment in comments do
	 (when (shown comment)
	   (incf num)))
    num))

(defun comments-by-author (author-id)
  (get-instances-by-value 'comment-data 'author author-id))

;; (author-id (cadr (get-instances-by-class 'comment-author)))

;; (let ((comment1 (make-instance 'blog-comment-post :comment "some comment"
;; 			       :author-id 0
;; 			       :visible-name "Stas"))
;;       (comment2 (make-instance 'blog-comment-post :comment "another comment"
;; 			       :author-id 0
;; 			       :visible-name "Stas"))
;;       (lst '()))
;;   (push comment1 lst)
;;   (push comment2 lst)
;;   (setf (comments-num (cadr (get-instances-by-class 'blog-data-post))) 2
;; 	(comments (cadr (get-instances-by-class 'blog-data-post))) lst))


;; (setf  (comments (cadr (get-instances-by-class 'blog-data-post))) lst)

;; (setf  (comments-num (cadr (get-instances-by-class 'blog-data-post))) 3)

;; (setq lst '())

;; (push 1 lst)

;; lst

;;(print (content-src (car (cddddr (get-instances-by-class 'blog-data-post)))))

;; (dolist (elem (get-instances-by-class 'blog-data-post))
;; 		 (elephant:drop-instance elem))

;; (get-instances-by-class 'blog-data-static)

;; (let ((about-static (make-instance 'blog-data-static :title "About"
;; 				   :content-src "### About

;; Just some holder text

;; * First element
;; * Second element
;; * Third element

;; Nothing else at the moment."
;; 				   :permalink "/about"
;; 				   :id (get-new-id 'blog-data-static 'data-id))))

;;   (blog-data-render about-static)
;;   (insert-item about-static *blog-posts*))

;; (blog-article-add "About1" "
;; Hello, my name is Stanislav M. Ivankin or grepz. This site is just an attempt to create a place to hold any useful(or not very (:) information I can share.

;; My current interests in computer software/hardware are:
;; * FPGA: It is indeed quite fun to be able to implement parts of algorithms on FPGA stone, although I am on very early stages of FPGA diving.
;; * MCU's: I concider myself having some experience with MCU's, especially STM32 microcontrollers and some stuff you can combine with those, starting from flash/SD, LCD, LAN modules to FPGA chips
;; * Various operating systems: RT OS's for small embedded devices, microkernel/excokernel OS's, especially if later is combined with power of virtual machines.
;; * Hacking useful code together: Anything to have some fun while/after coding

;; Also being big fan of bicycling, mountain hiking and muay thai combat sport. Trying to practice everyting from this list and live my life having some fun. :)

;; = About this site

;; This site created with help from Common Lisp The Language and Emacs The Text Editor. It is in very early stage of development.")


;; (blog-post-add "First post" "
;; === Header1

;; * Opt1

;; * Opt2

;; * Opt3

;; str1

;;     while (1) {
;;         do_func();
;;     }

;; str2")

;; (blog-post-add "Second post" "
;; This is example of post with alot of code fragments in it and some long lines, it will help to test blog.
;; Some C code:

;;     #include <stdlib.h>
;;     #include <stdio.h>
;;     #include <stdint.h>

;;     int main(int argc, char *argv[]) {
;;        printf(\"Hello world!\\n\");
;;        *((uint32_t *)0xAAAA) = 0xBBBB;
;;        return 0;
;;     }

;; End of C code fragment.

;; Now some lisp code:

;;     (setq lst (list 1 2 3 4 5))
;;     (mapcar #'(lambda (x) (+ 1 x)) list)

;; End of Lisp code fragment.
;; And thats enough for now.
;; ")

(defun string-replace (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
