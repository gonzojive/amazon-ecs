(in-package :org.iodb.amazon.ecs)

(defparameter *access-key-id* nil
  "The access key is assigned by amazon and is the public key that identifies your account.")
(defparameter *secret-access-key* nil
  "The secret key is assigned by Amazon.com and is only known to you.")
(defparameter *associates-id* "bookllamacom-20"
  "This is used to embed your money-making associate ID into links etc.")

(defparameter *webservices-domain* "webservices.amazon.com"
  "The domain to use to make requests.  Could alter this to ecs.amazon.co.uk for example.")
(defparameter *webservices-uri-path* "/onca/xml"
  "This will probably never need to change")
(defparameter *webservices-version* "2009-07-01")

(defun urlize-key-value (key value)
  "Given a key and value, returns the key=value string to put in a URI."
  (typecase value
    #+nil
    (cons
       (format nil
	       "~{~A~^&~}"
	       (loop :for i :from 1
		     :for subvalue :in value
		     :collect (urlize-key-value (format nil "~A.~A" key i) subvalue))))
    (null
       (urlize-key-value key ""))
    (symbol
       (urlize-key-value key (hyphenated->camelized (symbol-name value))))
    (string
       (format nil "~A=~A" (hunchentoot:url-encode key) (hunchentoot:url-encode value)))))
    
(defun formatted-timestamp (&optional (time-to-decode (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time-to-decode 0)
    (format nil
	    "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" ; "YYYY-MM-DDThh:mm:ssZ"
	    year month date hour minute second)))

(defun string-hmac (encoded-string key-string)
  (let* ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key-string) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array encoded-string))
    (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))
    ;(ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))
    

(defun sign (sorted-query-without-signature secret-key &key (method "GET") (domain *webservices-domain*) (uri *webservices-uri-path*))
  (let* ((string-to-sign
	  (format nil "~A~%~A~%~A~%~A"
		  method
		  domain
		  uri
		  sorted-query-without-signature))
	 (hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret-key) :sha256)))

    ;; digest the string
    (ironclad:update-hmac hmac
			  (ironclad:ascii-string-to-byte-array string-to-sign))
    ;; encode the result byte array with base64
    (let ((result-string
	   (base64:usb8-array-to-base64-string
	    (ironclad:hmac-digest hmac))))
      (values result-string
	      string-to-sign))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hyphenated->camelized (hyphen-word)
    "Returns a camelized version of the hyphenated string of words. abc-efg -> AbcEfg"
    (let ((words (cl-ppcre:split "-" hyphen-word)))
      (apply #'concatenate 'string (mapcar #'string-capitalize words))))


  (defun destructure-url-key-form (url-key-form)
    "Returns 2 values: the url-key-form symbol interned in the current package, and the
URL key to use for this particular key."
    ;;  (let ((parameter-name (if (consp url-key-form)
    ;;			    (second url-key-form)
    ;;			    (if (stringp url-key-form)
    ;;				url-key-form
    ;;				(hyphenated->camelized (symbol-name url-key-form))))))
    (when (atom url-key-form)
      (setf url-key-form (list url-key-form (hyphenated->camelized (symbol-name url-key-form)))))
    (values (first url-key-form) ;(intern (symbol-name (first url-key-form)))
	    (second url-key-form))))


(defmacro bind-and-parameterize (&rest url-key-forms)
  "Each URL Key form is either a symbol or a (evlaed-form string) where the STRING is the parameter name
encoded into the URL.  Symbol is a variable that we assume is bound. Nil-bound variables are assumed
unspecified"
  (let ((post-optional nil))
    `(remove nil
	     (list ,@(mapcar #'(lambda (url-key-form)
				 (if (eql '&optional url-key-form)
				     (progn (setf post-optional t) nil)
				     (multiple-value-bind (variable key-string)
					 (destructure-url-key-form url-key-form)
				       (if (not post-optional)
					   `(cons ,key-string ,variable)
					   (let ((var (gensym key-string)))
					     `(let ((,var ,variable))
						(when ,var (cons ,key-string ,var))))))))
			     url-key-forms)))))

(defun generate-uri (&key
		     (access-key-id *access-key-id*)
		     (associate-id *associates-id*)
		     (secret-access-key *secret-access-key*)
		     (version *webservices-version*)
		     merchant-id
		     operation
		     parameters
		     response-group
		     validate
		     (timestamp (formatted-timestamp)))
  "Generates an Amazon Advertizing API request uri.  PARAMETERS is an alist of parameters."
  (declare (type string access-key-id secret-access-key)
	   (type (or string symbol) operation))
  (let* ((query-without-signature
	  (format nil
		  "~{~A~^&~}"
		  (mapcar #'(lambda (keyval-pair)
			      (urlize-key-value (car keyval-pair) (cdr keyval-pair)))
			  (let* ((params-unsorted
				  (concatenate
				   'list
				   (bind-and-parameterize ("AWSECommerceService" "Service")
							  (access-key-id "AWSAccessKeyId")
							  operation
							  &optional
							  (associate-id "AssociateTag")
							  merchant-id
							  response-group
							  version
							  validate
							  timestamp)
				   parameters))
				 (params-sorted (sort params-unsorted #'string< :key #'car)))
			    params-sorted))))
	 (url-with-signature
	  (concatenate 'string
		       "http://"
		       *webservices-domain*
		       *webservices-uri-path*
		       "?"
		       query-without-signature
		       "&Signature="
		       (hunchentoot:url-encode (sign query-without-signature secret-access-key)))))
    (format t "URI:~%~A~%" url-with-signature)
    url-with-signature))
		      



;;;; graveyard