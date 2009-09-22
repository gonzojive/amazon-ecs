(in-package :org.iodb.amazon.ecs)
;;; this file contains functions for making queries to the amazon
;;; e-commerce service.

(defun item-lookup (&key response-group search-index
		    condition delivery-method id-type merchant-id offer-page
		    related-item-page relationship-type review-page review-sort tag-page tag-sort
		    tags-per-page variation-page ispu-postal-code)
  (let* ((uri
	  (generate-uri :response-group response-group
			:operation :item-search
			:parameters (bind-and-parameterize
				     search-index condition delivery-method id-type merchant-id offer-page
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

(defun request-and-parse (uri parse-fn)
  (let ((stream (drakma:http-request uri :want-stream t)))
    (unwind-protect (funcall parse-fn stream)
      (close stream))))
							     

(defgeneric official-amazon-offer? (offer)
  (:documentation "Returns whether or not the offer is an official amazon offer."))

(defmethod official-amazon-offer? ((offer offer))
  (let ((merchant (offer-merchant offer)))
    (and merchant
	 (or (string-equal (merchant-id merchant)
			   "ATVPDKIKX0DER")
	     (string-equal (merchant-id merchant)
			   "Amazon")))))

(defgeneric item-official-amazon-offer  (item)
  (:documentation "Returns the first official amazon offer for the item."))

(defmethod item-official-amazon-offer ((item amazon-item))
  (find-if #'ecs:official-amazon-offer?
	   (and (ecs:item-offers item)
		(ecs:offers (ecs:item-offers item)))))

