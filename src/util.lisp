;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:eterhost-site)

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

(defun split-str (string delim &key (remove-empty t))
  "Split string by `delim' which is char, remove empty subseqs if
  `remove-empty' is t"
  (let ((len (length string)))
    (loop
       for i = 0 then (1+ j)
       for j = (or (position delim string :start i) len)
       unless (and (= j i) remove-empty)
       collect  (subseq string i j)
       while (< j len))))

(defun id-oid-to-str (oid)
  "Formats OID to string."
  (declare ((vector (unsigned-byte 8)) oid)
	   (optimize (speed 3) (safety 0)))
  (the simple-string
       (reduce #'(lambda (x y) (concatenate 'string x (hex-print y)))
	       oid :initial-value "")))

(defun id-str-to-oid (str)
  "Converts OID hex string representation into a OID vector suitable to use with
  cl-mongo"
  (assert (evenp (length str)))
  (let* ((len (length str))
	 (elems (floor len 2))
	 (vect (make-array elems :element-type '(unsigned-byte 8))))
    (loop
       for i from 0 below elems
       for k from 0 by 2 do
	 (setf (aref vect i)
	       (parse-integer str :start k :end (+ k 2) :radix 16)))
    vect))

(defmethod render-html (src)
  "Convert markup language to a html data using `cl-markdown'"
  (cl-markdown:render-to-stream
   (cl-markdown:markdown src :stream nil) :html nil))

(defun hex-print (value)
  "Convert uint8_t to a hex string."
  (declare ((unsigned-byte 8) value)
	   (optimize (speed 3) (safety 0)))
  (let ((hex (write-to-string value :base 16 :case :downcase)))
    (declare (simple-string hex))
    (the simple-string (if (< (length hex) 2)
			   (concatenate 'string "0" hex)
			   hex))))

(defun hex-print-1 (value &key (size 2))
  "Convert integer to a hex string."
  (declare (fixnum value size)
	   (optimize (speed 3) (safety 0)))
  (the simple-string (format nil "~v,'0X" size value)))

(defun random-array (len rrange &key (type t))
  "Create array of length `len' filled with random values in range `rrange' of
  type `type'."
  (let ((array (make-array len :element-type type)))
    (loop for x from 0 to (1- len) do
	 (setf (aref array x) (random rrange)))
    array))

(defun array-to-list (arr)
  "Convert array to list."
  (loop for x across arr collect x))

(defun list-to-array (lst &key (type t))
  "Convert list to array."
  (make-array (length lst) :initial-contents lst :element-type type))

(defun compare-lists (list1 list2 &key (test #'=))
  "Loop is cool, function wont be used often, so no sense in recursive
  variant."
  (when (= (length list1) (length list2))
    (loop for x in list1
       for y in list2
       for cond = (funcall test x y) while cond
       finally (return cond))))

(defun filter-list (src-lst dst-lst)
  "Filter for string duplicates in list"
  (if src-lst
      (filter-list (remove-if #'(lambda (x)
				  (string= (car src-lst) x)) src-lst)
		   (cons (car src-lst) dst-lst))
      (nreverse  dst-lst)))

(defun list-to-str (lst str)
  (declare (list lst)
	   (simple-string str)
	   (optimize (speed 3) (safety 0)))
  (if lst
      (list-to-str (cdr lst) (concatenate 'string str "," (car lst)))
      (when (> (length str) 0)
	(subseq str 1))))

(defmacro with-html ((&key (prologue nil) (indent t)) &body body)
  `(with-html-output-to-string
    (*standard-output* nil :prologue ,prologue :indent ,indent)
    ,@body))

(defmacro with-authentication (&body body)
  `(if (null (hunchentoot:session-value :auth))
       (redirect "/login")
       (progn ,@body)))

(defun blog-post-gen-link (host uid)
  (concatenate 'string "http://" host "/post?id=" uid))

(defun parse-tags (input)
  (mapcar #'(lambda (x)
	      (string-trim " " x))
	  (filter-list (split-str input #\,) '())))

(defun tags-list-to-str (tags)
  (list-to-str tags ""))
