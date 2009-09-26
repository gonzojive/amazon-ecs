(in-package :org.iodb.amazon.ecs)

(defvar *access-key-id* nil
  "The access key is assigned by amazon and is the public key that identifies your account.")
(defvar *secret-access-key* nil
  "The secret key is assigned by Amazon.com and is only known to you.")
(defvar *associates-id* "bookllamacom-20"
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
       (format nil "~A=~A" (amazon-url-encode key) (amazon-url-encode value)))))
    
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

  (defgeneric lisp-value->url-value (lisp-value)
    (:documentation "Given a lisp value, convert it into a string that may then be
embedded in a URL and maintain the meaning of the orignal lisp value. E.G.
:dog-house would be translated into 'DogHouse'.  Such url values must further
be url-encoded.")
    (:method ((lisp-value string)) lisp-value)
    (:method ((lisp-value null)) "")
    (:method ((lisp-value t)) (format nil "~A" lisp-value))
    (:method ((lisp-value symbol)) (hyphenated->camelized (symbol-name lisp-value))))
    

  (defun destructure-url-key-form (url-key-form)
    "Returns 2 values: the url-key-form symbol interned in the current package, and the
URL key to use for this particular key."
    ;;  (let ((parameter-name (if (consp url-key-form)
    ;;			    (second url-key-form)
    ;;			    (if (stringp url-key-form)
    ;;				url-key-form
    ;;				(hyphenated->camelized (symbol-name url-key-form))))))
    (when (atom url-key-form)
      (setf url-key-form (list url-key-form (lisp-value->url-value url-key-form))))
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

(defun amazon-url-encode (string)
  "URL-encode a string according to Amazon's URL encoding rules:
* Do not URL encode any of the unreserved characters that RFC 3986 defines.
  These unreserved characters are A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ),
  and tilde ( ~ ).
* Percent encode extended UTF-8 characters in the form %XY%ZA....
* Percent encode the space character as %20 (and not +, as common encoding schemes
  do).
* Percent encode all other characters with %XY, where X and Y are hex characters 0-9 and
  uppercase A-F."
  (with-output-to-string (s)
    (loop :for c :across string
	  :for index :from 0
          :do (cond ((or (char<= #\0 c #\9)
			 (char<= #\a c #\z)
			 (char<= #\A c #\Z)
			 (find c "-_.~" :test #'char=))
		     (write-char c s))
		    ((<= 0 (char-code c) 128)
		     (format s "%~2,'0x" (char-code c)))
		    (t
		     (loop :for octet :across (flexi-streams:string-to-octets string
						    :start index
						    :end (1+ index))
			   :do (format s "%~2,'0x" octet)))))))

(defun generate-parameters-string (&key
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
  "Generates a parameters string to be either included in the GET url or POST body
of a query."
  (declare (type string access-key-id secret-access-key)
	   (type (or string symbol) operation))
  (let* ((all-parameters (concatenate
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
	 (query-with-signature (agnostic-parameters-string
				:parameters all-parameters
				:secret-access-key secret-access-key)))
    query-with-signature))
  

(defun generate-uri (&rest rest)
  "Generates an Amazon Advertizing API request uri.  PARAMETERS is an alist of parameters."
  (let* ((query-with-signature (apply 'generate-parameters-string rest))
	 (url-with-signature
	  (concatenate 'string
		       "http://"
		       *webservices-domain*
		       *webservices-uri-path*
		       "?"
		       query-with-signature)))
    (format t "URI:~%~A~%" url-with-signature)
    url-with-signature))

(defun uri-for-post ()
  (concatenate 'string
	       "http://"
	       *webservices-domain*
	       *webservices-uri-path*))

(defun generate-batch-parameters-string (&key
					 operation
					 (access-key-id *access-key-id*)
					 (associate-id *associates-id*)
					 (secret-access-key *secret-access-key*)
					 (version *webservices-version*)
					 merchant-id
					 parameters
					 response-group
					 validate
					 (timestamp (formatted-timestamp)))
  (declare (type string access-key-id secret-access-key)
	   (type (or string symbol) operation))
  (let* ((all-parameters (concatenate
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
	 (query-with-signature (agnostic-parameters-string
				:parameters all-parameters
				:secret-access-key secret-access-key)))
    query-with-signature))
  

(defun batch-request-parameters (&key
				 operation
				 simple-parameters
				 shared-parameters
				 independent-parameters)
  "Generates the parameters for a batch query.

Each member of  SIMPLE-PARAMETERS has the following form
   (key . value)

Each member of  SHARED-PARAMETERS has the following form
   (key . value)

Each member of INDEPENDENT-PARAMETERS is formatted like so
   (key value1 valu2 value3 value4)

It is assumed that each member of INDEPENDENT-PARAMETERS is the same
length.  However, this is not validated at runtime

Each member of SIMPLE-PARAMETERS will be formatted like so:
   Parameter=Value

Each member of SHARED-PARAMETERS will be formatted like so:
   OperationName.Parameter=Value

Each member of INDEPENDENT-PARAMETERS  will be formatted like so:
   OperationName.ReferenceNumber.Parameter=Value
"
  (declare (type (or string symbol) operation))

  (let* ((params (map 'list #'identity simple-parameters))
	 (op-string (lisp-value->url-value operation)))
    (map nil
	 #'(lambda (shared-param)
	     (push (cons (format nil
				 "~A.Shared.~A"
				 op-string
				 (lisp-value->url-value (car shared-param)))
			 (cdr shared-param))
		   params))
	 shared-parameters)
    (map nil
	 #'(lambda (independent-param)
	     (let ((base-key-string (format nil
					    "~A.~A"
					    op-string
					    (lisp-value->url-value (car independent-param)))))
	       (flet ((key (ref-num) (format nil "~A.~A" base-key-string ref-num)))
		 (loop :for value :in (rest independent-param)
		       :for i :from 1
		       :do (push (cons (key i) value) params)))))
	 independent-parameters)
    params))

(defun agnostic-parameters-string (&key
				   (secret-access-key *secret-access-key*)
				   parameters
				   sortedp)
  "Generates a parameters string to be either included in the GET url or POST body
of a query."
  (declare (type string secret-access-key))
  (let* ((parameters-sorted
	  (if sortedp
	      parameters
	      (sort parameters #'string< :key #'car)))
	 (query-without-signature
	  (format nil
		  "~{~A~^&~}"
		  (mapcar #'(lambda (keyval-pair)
			      (urlize-key-value (car keyval-pair) (cdr keyval-pair)))
			  parameters-sorted)))
	 (query-with-signature
	  (concatenate 'string
		       query-without-signature
		       "&Signature="
		       (amazon-url-encode (sign query-without-signature secret-access-key)))))
    query-with-signature))


;;;; graveyard