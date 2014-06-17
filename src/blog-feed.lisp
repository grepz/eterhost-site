;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defvar *feed-time-format* '((:year 4) #\- (:month 2) #\- (:day 2)
			    #\T (:hour 2) #\: (:min 2) #\: (:sec 2) #\Z))

(defun post-to-feed-content (html)
  (let ((len (length html)))
    (if (> len *feed-symbol-cut*)
	(concatenate 'string
	  (hunchentoot:escape-for-html (subseq html 0 *feed-symbol-cut*))
	  "[...]")
	(hunchentoot:escape-for-html html))))

(defmacro with-atom-xml ((link updated &key (title "") (subtitle "") (id "")
			  (author-name *blog-author*)
			  (author-email *blog-author-email*))
			 &body body)
  `(with-html (:prologue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	       :indent nil)
     (:feed :xmlns "http://www.w3.org/2005/Atom"
	    (:title ,title) (:subtitle ,subtitle)
	    (:link :href ,link :rel "self" :type "application/atom+xml")
	    (:author (:name ,author-name) (:email ,author-email))
	    (:id (fmt "~a" (concatenate 'string "urn:uuid:" ,id)))
	    (:updated (fmt "~a" ,updated))
	    ,@body)))

(defmacro atom-xml-entry (summary &key (title "") (entry-link "") (id "")
			  (updated ""))
  `(with-html (:prologue nil :indent nil)
     (:entry (:title (fmt "~a" ,title)) (:link :href ,entry-link)
	     (:id (fmt "~a" (concatenate 'string "urn:uuid:" ,id)))
	     (:updated (fmt "~a" (local-time:format-timestring
				 nil (local-time:universal-to-timestamp
				      ,updated) :format *feed-time-format*)))
	     (:summary :type "html"
		       (fmt "~a" ,summary)))))
