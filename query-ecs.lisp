(in-package :org.iodb.amazon.ecs)
;;; this file contains functions for making queries to the amazon
;;; e-commerce service.

(defun item-lookup (&rest op-params)
  (let ((parsed-query-root (apply 'perform-operation :item-lookup op-params)))
    (values (response-items parsed-query-root)
	    parsed-query-root)))

(defun item-search (&rest op-params)
  (let ((parsed-query-root (apply 'perform-operation :item-search op-params)))
    (values (response-items parsed-query-root)
	    parsed-query-root)))

;(find-class 'item-lookup-response)
(defparameter *last-request-time* (get-universal-time))
(defparameter *request-lock* (bordeaux-threads:make-lock "amazon-http-requester"))

(defun amazon-http-request (uri)
  "Requests the given URI and returns a stream with the result.  Thread friendly
and avoids sending at more than 1 per second."
  (bordeaux-threads:acquire-lock *request-lock*)
  (unwind-protect
       (progn
	 (when (< (- (get-universal-time) *last-request-time*)
		  1)
	   (sleep 1))
	 (drakma:http-request uri :want-stream t))
    (setf *last-request-time* (get-universal-time))
    (bordeaux-threads:release-lock *request-lock*)))

(defparameter *default-possible-root-elements*
  (mapcar #'find-class '(item-lookup-response item-search-response)))

(defun amazon-request-and-parse (&key
				 parameters
				 parameters-sortedp
				 (possible-root-elements *default-possible-root-elements*)
				 (secret-access-key *secret-access-key*))
  (let* ((query-with-signature (agnostic-parameters-string
				:parameters parameters
				:sortedp parameters-sortedp
				:secret-access-key secret-access-key))
	 (uri (concatenate 'string
			   (uri-for-post)
			   "?"
			   query-with-signature)))
    (format t "Amazon Request URI:~%~A~%" uri)
    (let ((parsed-query
	   (request-and-parse uri
			      #'(lambda (s)
				  (xml-mop:parse-xml-stream s possible-root-elements)))))
      (first parsed-query))))

(defun request-and-parse (uri parse-fn)
  (let ((stream (amazon-http-request uri)))
    (unwind-protect (funcall parse-fn stream)
      (close stream))))

