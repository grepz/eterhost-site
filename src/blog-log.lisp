;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

(defvar *log-regex* "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}) - \\[([^\\]]+)\\] \"([^\\\"]+)\" (\\d+|-) (\\d+|-) \"([^\\\"]*)\" \"([^\\\"]*)\"")

(defun hunchentoot-log-get-hash (fpath)
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

(defun hunchentoot-log-simple-parse (hash)
  (loop for key being the hash-keys of hash using (hash-value value)
     for x = 0 then (1+ x) do
       (progn
	 (format t "IP: ~a; Hits: ~a: Client: ~a; What: ~a~%"
		 key (length value)
		 (hunchentoot-log-hash-entry-who value)
		 (hunchentoot-log-hash-entry-what value)))
     finally (return x)))

(multiple-value-bind (tbl inv inv-str)
    (hunchentoot-log-get-hash "~/tmp/ssl-access.log")
  (format t "Invalid entries=~a; Total number of unique visitors=~a~%"
	  inv (hunchentoot-log-simple-parse tbl))
  (format t "Invalid strings: ~a~%" inv-str))
