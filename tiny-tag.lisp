;;; tiny-tag.lisp --- generate short unique identifiers.

;; Copyright (C) 2019 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

#-(and)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :tiny-tag-dbi *features*))

(defpackage :tiny-tag
  (:use :common-lisp
	:iterate)
  (:shadow *random-state*)
  (:documentation "Generate short unique identifiers."))

(in-package :tiny-tag)

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like `defconstant' except that the initially set value
is reused when the `defconst' form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(export '*sqlite-database*)
(defparameter *sqlite-database* (uiop:merge-pathnames*
				 (uiop:parse-native-namestring
				  "tiny-tag.sqlite"))
  "SQLite database file name (a pathname).")

(export '*sqlite-busy-timeout*)
(defparameter *sqlite-busy-timeout* 500
  "SQLite database timeout.

Value is the number of milliseconds to wait when the database is busy.
Null means to return immediately.")

(defvar *random-state* (make-random-state t)
  "Random state object for generating tags.")

;; Exclude digits 0 (o), 1 (l), 2 (z), 5 (s), 6 (b), and 9 (g, q).
(defconst +alphabet+ "abcdefghijklmnopqrstuvwxyz3478"
  "Set of valid characters.")

(defconst +base+ (length +alphabet+)
  "Number base.")

(defun make-tag (length)
  "Create a tag of length LENGTH."
  (check-type length (integer 1))
  (iter (repeat length)
	(collect (aref +alphabet+ (random +base+)) :result-type 'string)))
#-(and)
(make-tag 8)

(defmacro with-database-connection (conn &body body)
  #+tiny-tag-dbi
  `(dbi:with-connection (,conn :sqlite3
			       :database-name *sqlite-database*
			       :busy-timeout *sqlite-busy-timeout*)
     ,@body)
  #-tiny-tag-dbi
  `(sqlite:with-open-database (,conn *sqlite-database* :busy-timeout *sqlite-busy-timeout*)
     ,@body))

(defun %length-limits (conn)
  (let ((sql "select minlen, maxlen from param"))
    #+tiny-tag-dbi
    (let ((result (dbi:fetch (dbi:execute (dbi:prepare conn sql)))))
      (values (getf result :|minlen|) (getf result :|maxlen|)))
    #-tiny-tag-dbi
    (sqlite:execute-one-row-m-v conn sql)))

(export 'length-limits)
(defun length-limits ()
  "Return the minimum length and maximum length as multiple values."
  (with-database-connection conn
    (%length-limits conn)))
#-(and)
(length-limits)

(export 'tiny-tag)
(defun tiny-tag (&optional length)
  "Allocate a short unique identifier.

Optional argument LENGTH is the length of the identifier.  Value is
 an integer or null.  The later means to automatically choose a valid
 identifier length.  Otherwise, the identifier length is limited to
 the valid range (see the ‘length-limits’ function).

Return value is the newly allocated identifier."
  (check-type length (or null integer))
  (let ((cl:*random-state* *random-state*))
    (with-database-connection conn
      (multiple-value-bind (min-len max-len)
	  (%length-limits conn)
	(let ((sql "insert into tags values (?, ?)")
	      ;; A function which returns the tag length.
	      (tag-len (cond ((null length)
			      (lambda ()
				(+ min-len (random (1+ (- max-len min-len))))))
			     ((<= min-len length max-len)
			      (constantly (max min-len (min length max-len))))
			     (t
			      (error "Invalid tag length ‘~A’." length)))))
	  (iter (for len = (funcall tag-len))
		(for tag = (make-tag len))
		(handler-case
		    (progn
		      #+tiny-tag-dbi
		      (dbi:execute (dbi:prepare conn sql) tag len)
		      #-tiny-tag-dbi
		      (sqlite:execute-non-query conn sql tag len))
		  (:no-error (&rest arg)
		    (return tag)))
		))))))
#-(and)
(tiny-tag)

;;; tiny-tag.lisp ends here
