(in-package :amazon-ecs)

(defgeneric generate-operation-specific-parameters (op &key &allow-other-keys)
  (:documentation "Given an operation keyword, returns an alist of parameters to send
along specific to the particular operation.  Only things like ResponseGroup, TagSort, etc.
are returned and not things like Operation, Timestamp, and other simple params."))

(defun batch-params (op shared-args-plist independent-args-plists)
  (batch-request-parameters
   :operation op
   :shared-parameters (apply #'generate-operation-specific-parameters op shared-args-plist)
   :independent-parameters (let ((params-hash (make-hash-table :test 'equal)))
			     (labels ((canonical-key (key)     (lisp-value->url-value key))
				      (push-key-val (pair)
					(let ((key (canonical-key (car pair)))
					      (value (cdr pair)))
					  (push value (gethash key params-hash)))))
			       (loop :for independent-args-plist :in (reverse independent-args-plists)
				     :do (map nil #'push-key-val
						    (apply #'generate-operation-specific-parameters op independent-args-plist)))
			       (let ((ind-params nil))
				 (maphash #'(lambda (key values)
					      (push (cons key values) ind-params))
					  params-hash)
				 ind-params)))))
					    

(defmacro defop (op-keyword params-list)
  "Defines an operation with keyword OP-KEYWORD and params PARAMS.  Params is a list where
each item is of the form
param-symbol OR (param-symbol &key key-string initform)
where key-string defaults to the hyphen->camelized version of the symbol-name of param-symbol."
  (labels ((canonicalize-param (param-form)
	     (cond
	       ((consp param-form)  param-form)
	       ((eql '&optional param-form) param-form)
	       (t (list param-form)))))
    (let ((canonicalized-params (mapcar #'canonicalize-param params-list)))
      `(defmethod generate-operation-specific-parameters
	   ((op (eql ,op-keyword))
	    &key ,@(remove nil
			   (mapcar #'(lambda (param)
				       (when (not (eql '&optional param))
					 (destructuring-bind (param-symbol &key key-string (initform nil initform-suppliedp) listp)
					     param
					   (declare (ignore key-string listp))
					   (if initform-suppliedp
					       (list param-symbol initform)
					       param-symbol))))
				   canonicalized-params))
	    access-key-id associate-id secret-access-key version validate timestamp)
	 (declare (ignore  access-key-id associate-id secret-access-key version validate timestamp))
	 
	 (bind-and-parameterize ,@(remove nil
					  (mapcar #'(lambda (param)
						      (if (eql '&optional param)
							  param
							  (destructuring-bind (param-symbol &key key-string listp initform)
							      param
							    (declare (ignore initform))
							    (if (or key-string listp)
								(list param-symbol :key-string key-string :listp listp)
								param-symbol))))
						  canonicalized-params)))))))
							 
(defop :item-lookup (&optional item-id
			       id-type search-index response-group
			       condition delivery-method merchant-id offer-page
			       related-item-page relationship-type review-page review-sort tag-page tag-sort
			       tags-per-page variation-page
			       (ispu-postal-code :key-string "ISPUPostalCode")))

(defop :item-search (&optional search-index actor artists audience-rating author availability brand browse-node
			       city composer condition conductor cuisine delivery-method director
			       disable-parent-asin-substitution item-page keywords manufacturer
			       maximum-price merchant-id minimum-price music-label neighborhood orchestra
			       postal-code power publisher related-item-page relationship-type release-date
			       review-sort sort state tag-page tag-sort tags-per-page text-stream title
			       response-group
			       (ispu-postal-code :key-string "ISPUPostalCode")))

(defop :cart-create ((items :key-string "Item" :listp t) &optional response-group merge-cart))

(defop :cart-add ((items :key-string "Item" :listp t)
                  &optional
                  response-group merge-cart cart-id
                  (hmac :key-string "HMAC")))

(defop :cart-get ((items :key-string "Item" :listp t)
                  &optional
                  response-group merge-cart cart-id
                  (hmac :key-string "HMAC")))

(defop :cart-modify ((items :key-string "Item" :listp t)
                     &optional
                     response-group merge-cart cart-id
                     (hmac :key-string "HMAC")))

(defun perform-operation (operation
			  &rest key-args
			  &key
			  (access-key-id *access-key-id*)
			  (associate-id *associates-id*)
			  (secret-access-key *secret-access-key*)
			  (version *webservices-version*)
			  validate
			  (timestamp (formatted-timestamp))
			  &allow-other-keys)
  ""
  (declare (type string access-key-id secret-access-key)
	   (type (or string symbol) operation))
  (let* ((simple-parameters (concatenate
			     'list
			     (bind-and-parameterize ("AWSECommerceService" :key-string "Service")
						    (access-key-id :key-string "AWSAccessKeyId")
						    operation
						    timestamp
						    &optional
						    (associate-id :key-string "AssociateTag")
						    version
						    validate)
			     (apply #'generate-operation-specific-parameters operation key-args))))
    (amazon-request-and-parse :parameters simple-parameters
			      :secret-access-key secret-access-key)))

(defun simple-parameters (&key
			  (access-key-id *access-key-id*)
			  (associate-id *associates-id*)
			  (version *webservices-version*)
			  operation
			  validate
			  (timestamp (formatted-timestamp)))
			  
  ""
  (declare (type string access-key-id))
  (bind-and-parameterize ("AWSECommerceService" :key-string "Service")
			 (access-key-id :key-string "AWSAccessKeyId")
			 timestamp
			 operation
			 &optional
			 (associate-id :key-string "AssociateTag")
			 version
			 validate))

(defun perform-batch-operation (operation
				&key
				simple-parameters-args
				shared-parameters-args
				independent-parameters-args
				(secret-access-key *secret-access-key*))
  ""
  (declare (type string secret-access-key))
  (let* ((parameters
	  (concatenate 'list
		       (apply 'simple-parameters :operation operation simple-parameters-args)
		       (batch-params operation shared-parameters-args independent-parameters-args))))
    (format t "Paramters: ~%~A~%" (sort (copy-list parameters) #'string< :key #'car))
    (amazon-request-and-parse :parameters parameters
			      :secret-access-key secret-access-key)))
