(in-package :org.iodb.amazon.ecs)
;;; this file contains functions for making queries to the amazon
;;; e-commerce service.

(defun base-ecs-url (language-code)
  (declare (ignore language-code))
  "http://webservices.amazon.com/onca/xml?Service=AWSECommerceService")

(defun join-string-list (string-list &optional (delimiter ""))
  "Concatenates a list of strings and puts spaces between the elements."
  (let ((control-str (concatenate 'string "~{~A~^" delimiter "~}")))
    (format nil control-str string-list)))

(defun hyphenated->camelized (hyphen-word)
  "Returns a camelized version of the hyphenated string of words."
  (let ((words (cl-ppcre:split "-" hyphen-word)))
    (join-string-list
     (mapcar #'string-capitalize words))))

(defun amazon-query-component (name value)
  (format nil "~A=~A" (hyphenated->camelized (string name)) value))

(defmacro urlize-keys (url-key-forms)
  `(list 
    ,@(mapcar
       #'(lambda (url-key-form)
	   (let* ((true-url-key (if (listp url-key-form) (second url-key-form) url-key-form))
		  (url-key-sym (if (listp url-key-form) (first url-key-form) url-key-form))
		  (url-key-value (intern (string url-key-sym))))
	     `(if ,url-key-value
	       ,(list 'amazon-query-component true-url-key url-key-value)
	       nil)))
       url-key-forms)))

(defun generate-ecs-url (&key aws-key associate-id operation
			 response-group item-id keywords search-index
			 merchant-id condition delivery-method id-type
			 ispu-postal-code version item-page)
  "Generates an ECS url with the supplied parameters."
  (join-string-list
   (list (base-ecs-url "en")
	 (join-string-list
	  (remove-if #'null 
		     (urlize-keys
		      (:search-index :operation :keywords :item-id :version
				     :response-group :merchant-id :id-type :condition :delivery-method
                                     :item-page
				     (:ispu-postal-code :i-s-p-u-postal-code)
				     (:aws-key :a-w-s-access-key-id)
				     (:associate-id :associate-tag))))
	  "&")) "&"))

(defun parse-response-stream (response-stream)
  (first
   (xml-mop::parse-xml-stream response-stream
			      (list (find-class 'item-lookup-response)
				    (find-class 'item-search-response)))))


(defun perform-amazon-search (query-url)
  (parse-response-stream
   (drakma:http-request query-url :want-stream t)))

(defun perform-amazon-request (query-url)
  (parse-response-stream
   (dotimes (i 5)
     (handler-case
	 (return (drakma:http-request query-url :want-stream t))
       (error ()
	 (format t "Error with request, retrying ~Ath time~%" i))))))



(defgeneric official-amazon-offer? (offer)
  (:documentation "Returns whether or not the offer is an official amazon offer."))

(defmethod official-amazon-offer? ((offer offer))
  (let ((merchant (offer-merchant offer)))
    (and merchant
	 (string-equal (merchant-id merchant)
		       "ATVPDKIKX0DER"))))

(defgeneric item-official-amazon-offer  (item)
  (:documentation "Returns the first official amazon offer for the item."))

(defmethod item-official-amazon-offer ((item amazon-item))
  (find-if #'ecs:official-amazon-offer?
	   (and (ecs:item-offers item)
		(ecs:offers (ecs:item-offers item)))))

