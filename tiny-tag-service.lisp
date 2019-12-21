;;; tiny-tag-service.lisp --- tiny tag web service.

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

(defpackage :tiny-tag-service
  (:use :common-lisp
	:iterate
	:tiny-tag)
  (:documentation "Tiny tag web service."))

(in-package :tiny-tag-service)

;;;; Landing Page

(defun base-url (acceptor)
  "Return the base URL of a web service."
  (format nil "~(~A~)://~(~A~)"
	  (if (hunchentoot:acceptor-ssl-p acceptor)
	      :https
	    :http)
	  (hunchentoot:host)))

(export '*home-template*)
(defparameter *home-template* (uiop:merge-pathnames*
			       (uiop:parse-native-namestring
				"index.html.in"))
  "HTML template for the landing page.

Value should be a pathname, stream, or string.")

(defun home-values (tag length)
  "HTML template values for the landing page.

First argument TAG is the allocated identifier.
Second argument LENGTH is the selected tag length.

Value is a property list suitable for use with
‘html-template:fill-and-print-template’."
  (let (values)
    ;; Base URL of the service.
    (push (list :base (base-url hunchentoot:*acceptor*)) values)
    ;; Tag length options are lists of the form
    ;; (:value INTEGER :checked BOOLEAN :example STRING).
    (multiple-value-bind (min-len max-len)
	(tiny-tag:length-limits)
      (let* ((ex '(( 5 . "gfizj")
		   ( 6 . "vmq4fq")
		   ( 7 . "zhx4fba")
		   ( 8 . "r3fqotd4")
		   ( 9 . "7srlnxmud")
		   (10 . "dvxce43zcv")))
	     (form (nconc
		    (iter (for k :from min-len :to max-len)
			  (collect (list :value k
					 :checked (= k (or length 0))
					 :example (cdr (assoc k ex)))))
		    (list (list :value nil
				:checked (null length))))))
	(push (list :form form) values)))
    (when (stringp tag)
      (push (list :tag tag) values))
    (push (list :length length) values)
    ;; Make VALUES a property list.
    (reduce #'nconc (nreverse values))))

(defun home ()
  "Create the landing page."
  (hunchentoot:no-cache)
  (let (tag length)
    ;; The GET parameter sets the user's preference but the POST
    ;; parameter always has precedence.
    (let ((value (if (eq (hunchentoot:request-method*) :post)
		     (hunchentoot:post-parameter "length")
		   (hunchentoot:get-parameter "length"))))
      (handler-case
	  (progn
	    (when (and value (plusp (length value)))
	      (setf length (parse-integer value)))
	    (when (eq (hunchentoot:request-method*) :post)
	      (setf tag (tiny-tag length))))
	(error ()
	  (hunchentoot:parameter-error "Invalid tag length."))))
    (let ((html-template:*string-modifier* #'identity)
	  (html-template:*ignore-empty-lines* t))
      (with-output-to-string (stream)
	(html-template:fill-and-print-template
	 *home-template* (home-values tag length)
	 :stream stream)))))

;;;; REST API

(snooze:defroute _tag (:get :text/* &key length)
  (handler-case
      (format nil "~A~%" (tiny-tag length))
    (error ()
      (snooze:http-condition hunchentoot:+http-bad-request+))))

(snooze:defroute _tag (:get :application/json &key length)
  (handler-case
      (let ((tag (tiny-tag length)))
	(cl-json:encode-json-plist-to-string
	 (list :tag tag :length (length tag))))
    (error ()
      (snooze:http-condition hunchentoot:+http-bad-request+))))

;;;; Web Service

(export '*http-port*)
(defparameter *http-port* 5080
  "HTTP port number.")

(export '*https-port*)
(defparameter *https-port* 5443
  "HTTPS port number.")

(export '*redirect-http-to-https*)
(defparameter *redirect-http-to-https* nil
  "Whether or not to redirect all HTTP requests to HTTPS.")

(defvar http-server nil
  "The HTTP server.")

(defun make-http-server ()
  ;; The :name keyword argument is required for redirecting HTTP
  ;; requests to the HTTPS server.
  (make-instance 'hunchentoot:easy-acceptor
    :port *http-port*
    :name 'http-server))

(defvar https-server nil
  "The HTTPS server.")

(defun make-https-server ()
  (make-instance 'hunchentoot:easy-ssl-acceptor
    :ssl-certificate-file (uiop:merge-pathnames*
			   (uiop:parse-native-namestring
			    "example.crt"))
    :ssl-privatekey-file (uiop:merge-pathnames*
			  (uiop:parse-native-namestring
			   "example.key"))
    :port *https-port*
    :name 'https-server))

(defun redirect-to-https ()
  "Redirect a request to the HTTPS server."
  (hunchentoot:redirect (hunchentoot:request-uri*)
			:protocol :https
			:port (hunchentoot:acceptor-port https-server)
			:code hunchentoot:+http-moved-permanently+))

(defun redirect-dispatcher (request)
  (when (eq (hunchentoot:acceptor-name hunchentoot:*acceptor*) 'http-server)
    #'redirect-to-https))

(defun static-dispatcher (request)
  (let ((path (hunchentoot:script-name request)))
    (when (or (string= path "/index.html")
	      (string= path "/"))
      #'home)))

(export 'stop-service)
(defun stop-service ()
  "Stop the web service."
  (when (and http-server (hunchentoot:started-p http-server))
    (hunchentoot:stop http-server))
  (when (and https-server (hunchentoot:started-p https-server))
    (hunchentoot:stop https-server)))

(export 'start-service)
(defun start-service ()
  "Start the web service."
  ;; Stop the servers.
  (stop-service)
  ;; Create the servers.
  (cond ((null *http-port*)
	 (setf http-server nil))
	((or (null http-server)
	     (/= (hunchentoot:acceptor-port http-server) *http-port*))
	 (setf http-server (make-http-server))))
  (cond ((null *https-port*)
	 (setf https-server nil))
	((or (null https-server)
	     (/= (hunchentoot:acceptor-port https-server) *https-port*))
	 (setf https-server (make-https-server))))
  ;; Initialize the dispatch table.
  (setf hunchentoot:*dispatch-table* (list
				      ;; Landing page.
				      #'static-dispatcher
				      ;; REST API.  This has to be
				      ;; the last element since Snooze
				      ;; signals an error if no route
				      ;; matches.
				      (snooze:make-hunchentoot-app)))
  ;; If HTTP redirection is enabled, it has to be the first element.
  (when (and *redirect-http-to-https* http-server https-server)
    (push #'redirect-dispatcher hunchentoot:*dispatch-table*))
  ;; Start the servers.
  (when (and http-server (not (hunchentoot:started-p http-server)))
    (hunchentoot:start http-server))
  (when (and https-server (not (hunchentoot:started-p https-server)))
    (hunchentoot:start https-server)))

#-(and)
(progn
  (setf *redirect-http-to-https* nil)
  (setf *redirect-http-to-https* t)
  (start-service)
  (stop-service)
  (values))

;;; tiny-tag-service.lisp ends here
