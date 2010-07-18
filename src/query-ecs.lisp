(in-package :org.iodb.amazon.ecs)
;;; this file contains functions for making queries to the amazon
;;; e-commerce service.

(defun item-lookup (&rest op-params)
  (let ((parsed-query-root (apply 'perform-operation :item-lookup op-params)))
    (values (when parsed-query-root (first (response-items parsed-query-root)))
	    parsed-query-root)))

(defun item-search (&rest op-params)
  (let ((parsed-query-root (apply 'perform-operation :item-search op-params)))
    (values (when parsed-query-root (first (response-items parsed-query-root)))
	    parsed-query-root)))

(defun cart-create (&rest op-params)
  (let ((parsed-query-root (apply 'perform-operation :cart-create op-params)))
    (values (response-cart parsed-query-root)
	    parsed-query-root)))

(defun extra-keys-from-cart-or-cart-id (cart-or-cart-id)
  (if (typep cart-or-cart-id 'cart)
      (list :cart-id (cart-id cart-or-cart-id)
            :hmac (cart-hmac cart-or-cart-id))
      (list :cart-id cart-or-cart-id)))

(defun cart-add (cart-or-cart-id &rest op-params)
  (let* ((parsed-query-root (apply 'perform-operation :cart-add
                                   (append (extra-keys-from-cart-or-cart-id cart-or-cart-id)
                                           op-params))))
    (values (response-cart parsed-query-root)
	    parsed-query-root)))


(defun cart-modify (cart-or-cart-id &rest op-params)
  (let* ((parsed-query-root (apply 'perform-operation :cart-modify
                                   (append (extra-keys-from-cart-or-cart-id cart-or-cart-id)
                                           op-params))))
    (values (response-cart parsed-query-root)
	    parsed-query-root)))


(defun cart-get (cart-or-cart-id &rest op-params)
  (let* ((parsed-query-root (apply 'perform-operation :cart-get
                                   (append (extra-keys-from-cart-or-cart-id cart-or-cart-id)
                                           op-params))))
    (values (response-cart parsed-query-root)
	    parsed-query-root)))

;(find-class 'item-lookup-response)
(defparameter *last-request-time* (get-universal-time))
(defparameter *request-lock* (bordeaux-threads:make-lock "amazon-http-requester"))

(defparameter *http-request-function* #'drakma:http-request)
(defparameter *http-request-frequency* 1)

(defun amazon-http-request (uri)
  "Requests the given URI and returns a stream with the result.  Thread friendly
and avoids sending at more than 1 per second."
  (bordeaux-threads:acquire-lock *request-lock*)
  (unwind-protect
       (progn
	 (when (< (- (get-universal-time) *last-request-time*)
		  *http-request-frequency*)
	   (sleep *http-request-frequency*))
	 (funcall *http-request-function* uri :want-stream t))
    (setf *last-request-time* (get-universal-time))
    (bordeaux-threads:release-lock *request-lock*)))

(defparameter *default-possible-root-elements*
  (mapcar #'find-class '(item-lookup-response
                         item-search-response 
                         cart-create-response 
                         cart-add-response
                         cart-modify-response
                         cart-get-response)))

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
    (format t "Amazon request params:~%~S~%" parameters)
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

