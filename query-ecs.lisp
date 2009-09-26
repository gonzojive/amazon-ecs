(in-package :org.iodb.amazon.ecs)
;;; this file contains functions for making queries to the amazon
;;; e-commerce service.

(defun item-lookup (&key item-id response-group search-index
		    condition delivery-method id-type merchant-id offer-page
		    related-item-page relationship-type review-page review-sort tag-page tag-sort
		    tags-per-page variation-page ispu-postal-code)
  (let* ((uri
	  (generate-uri :response-group response-group
			:operation :item-lookup
			:parameters (bind-and-parameterize
				     &optional
				     item-id search-index condition delivery-method id-type merchant-id offer-page
				     related-item-page relationship-type review-page review-sort tag-page tag-sort
				     tags-per-page variation-page
				     (ispu-postal-code "ISPUPostalCode"))))
	 (parsed-query
	  (request-and-parse uri
			     #'(lambda (s)
				 (xml-mop:parse-xml-stream s (list (find-class 'item-lookup-response)))))))
    (values (response-items (first parsed-query))
	    (first parsed-query))))

(defun item-search (&key search-index actor artists audience-rating author availability brand browse-node
		    city composer condition conductor cuisine delivery-method director
		    disable-parent-asin-substitution ispu-postal-code item-page keywords manufacturer
		    maximum-price merchant-id minimum-price music-label neighborhood orchestra
		    postal-code power publisher related-item-page relationship-type release-date
		    review-sort sort state tag-page tag-sort tags-per-page text-stream title
		    response-group)

  (let* ((uri
	 (generate-uri :response-group response-group
		       :operation :item-search
		       :parameters (bind-and-parameterize
				    &optional
				    search-index actor artists audience-rating author availability brand browse-node
				    city composer condition conductor cuisine delivery-method director
				    disable-parent-asin-substitution  item-page keywords manufacturer
				    maximum-price merchant-id minimum-price music-label neighborhood orchestra
				    postal-code power publisher related-item-page relationship-type release-date
				    review-sort sort state tag-page tag-sort tags-per-page text-stream title
				    (ispu-postal-code "ISPUPostalCode"))))
	 (parsed-query
	  (request-and-parse uri
			     #'(lambda (s)
				 (xml-mop:parse-xml-stream s (list (find-class 'item-search-response)))))))
    (values (response-items (first parsed-query))
	    (first parsed-query))))

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
			   query-with-signature))
	 (parsed-query
	  (request-and-parse uri
			     #'(lambda (s)
				 (xml-mop:parse-xml-stream s possible-root-elements)))))
    (first parsed-query)))

(defun request-and-parse (uri parse-fn)
  (let ((stream (amazon-http-request uri)))
    (unwind-protect (funcall parse-fn stream)
      (close stream))))

