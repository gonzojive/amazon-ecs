(in-package :org.iodb.amazon.ecs)

;;;; Class definitions for AWS responses ;;;;

;;; elementary element classes that get reused in a lot of places
(defclass numerical-text-element ()
  ()
  (:metaclass element-class)
  (:documentation "An element that has text that reduces to a number."))

(defmethod xml-mop:element-value ((element numerical-text-element))
  (parse-number:parse-number (xml-mop:element-text element)))

;; simple text element is one that is xml-evaluated 
(defclass simple-text-element () () (:metaclass element-class))

(defmethod xml-mop:element-value ((element simple-text-element))
  (xml-mop:element-text element))

;; an xml element that xml-evaluates to a date
(defclass date-element () () (:metaclass element-class))

(defmethod xml-mop:element-value ((element date-element))
  (my-parse-date (element-text element)))

;; a boolean element that avaluates to T r NIL
(defclass yes-no-element (simple-text-element) () (:metaclass element-class))

(defmethod xml-mop:element-value ((element yes-no-element))
  (let ((text (xml-mop:element-text element)))
    (cond
      ((member text '("True" "1") :test #'string-equal) t)
      ((member text '("False" "0") :test #'string-equal) nil))))


;; some sort of numerical element with units
(defclass numerical-measurement-element (numerical-text-element)
  ((units :attribute "Units" :accessor distance-units :initform ""))
  (:metaclass element-class))

;(defclass digital-distance-element (numerical-measurement-element) () (:metaclass element-class))

(defclass distance-element (numerical-measurement-element) () (:metaclass element-class))

(defclass weight-element (numerical-measurement-element) () (:metaclass element-class))

(defclass key-value-element ()
  ((arg-name :accessor argument-name :initform "" :initarg :name :attribute ("Name"))
   (arg-value :accessor argument-name :initform "" :initarg :name :attribute ("Value")))
  (:metaclass element-class)
  (:documentation "Arguments element in Amazon ECS response"))

(defclass price-element ()
  ((amount :accessor price-amount :initform 0 :initarg :amount
	   :subelement (numerical-text-element :alias "Amount"))
   (currency-code :accessor price-currency-code :initform 0 :initarg :currency-code
		  :subelement (simple-text-element :alias "CurrencyCode"))
   (formatted-price :accessor price-formatted :initform 0 :initarg :formatted-price
		   :subelement (simple-text-element :alias "FormattedPrice")))
  (:metaclass element-class)
  (:documentation "Parent of all elements that contain price information"))

(defmethod print-object ((obj price-element) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (write-sequence (price-formatted obj) stream)))

(defgeneric price-in-cents (price))
(defgeneric price-in-dollars (price))
(defgeneric price-as-string (price))
(defmethod price-in-cents ((price price-element))
  (price-amount price))
(defmethod price-in-dollars ((price price-element))
  (/ (price-in-cents price) 100))
(defmethod price-as-string ((price price-element))
  (format t "$~,2F" (price-in-dollars price)))

(defclass image-element ()
  ((url :subelement (simple-text-element :alias "URL") :accessor image-url :initform "")
   (width :subelement (distance-element :alias "Width") :accessor image-width :initform "")
   (height :subelement (distance-element :alias "Height") :accessor image-height :initform ""))
  (:metaclass element-class))

;;; Root AWS responses
(defclass abstract-root-response ()
  ((xmlns :accessor response-xmlns :initform "" :initarg :xmlns
	  :attribute ("xmlns"))
   (operation-request :accessor response-operation-request :initform nil
		      :subelement (operation-request :alias "OperationRequest"))
   (items :accessor response-items :initform nil :initarg :items
	  :subelement (items :alias "Items")))
  (:metaclass element-class)
  (:documentation "root of most all operations response"))

(defclass item-search-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemSearchResponse"))
  (:documentation "root of item search response"))

(defclass item-lookup-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemLookupResponse"))
  (:documentation "root of item lookup responses"))

(defclass operation-request ()
  ((http-headers :accessor operation-http-headers :initform ()
		 :subelement (http-headers :alias "HTTPHeaders"))
   (request-id :accessor operation-requestid :initform "" :initarg requestid
	       :subelement (simple-text-element :alias "RequestId"))
   (arguments :accessor operation-arguments :initform () :initarg arguments
	      :subelement (operation-arguments))
   (request-processing-time :accessor request-processing-time :initform ()
			    :subelement (numerical-text-element :alias "RequestProcessingTime")))
  (:metaclass element-class)
  (:tags ("OperationRequest"))
  (:documentation "OperationResponse element in Amazon ECS response"))

(defclass operation-arguments ()
  ((arguments :accessor operation-arguments :initform nil
	      :subelement (key-value-element :alias "Argument" :multiple t)))
  (:metaclass element-class)
  (:tags ("Arguments")))

(defclass http-headers ()
  ((headers :accessor headers :initform () :initarg :headers
	    :subelement (key-value-element :alias "Header" :multiple t)))
  (:metaclass element-class)
  (:tags ("HTTPHeaders"))
  (:documentation "HTTPHeaders element in Amazon ECS response"))

;;; Information pertaining to items
(defclass items ()
  ((request :accessor request :initform nil :initarg :request :subelement
	    (:element-type items-request-info :alias "Request"))
   (items :accessor items :initform () :initarg :items
	  :subelement (amazon-item :multiple t))
   (total-results :subelement (numerical-text-element :alias "TotalResults"))
   (total-pages :subelement (numerical-text-element :alias "TotalPages")))
  (:metaclass element-class)
  (:tags ("Items"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass items-request-info ()
  ((is-valid :accessor request-is-valid :initform nil
	    :subelement (yes-no-element :alias "IsValid"))
   (errors :accessor request-errors :initform nil
	   :subelement (items-errors :alias "Errors"))
   (item-search-request :accessor request-item-search-request :initform ()
			:subelement (item-search-request))
   (item-lookup-request :accessor item-lookup-request :initform ()
			:subelement (item-lookup-request)))
  (:metaclass element-class)
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass items-errors ()
  ((errors :accessor errors :initform nil
	   :subelement (amazon-error :alias "Error" :multiple t)))
  (:metaclass element-class)
  (:documentation "Errors related to searching for or looking up items."))

(defclass amazon-error ()
  ((code :accessor error-code :initform nil
	 :subelement (simple-text-element :alias "Code"))
   (message :accessor error-message :initform nil
	    :subelement (simple-text-element :alias "Message")))
  (:metaclass element-class)
  (:documentation "Errors related to searching for or looking up items."))

(defclass item-search-request ()
  ((product-condition :accessor product-condition :initform nil :initarg :product-condition
	     :subelement (simple-text-element :alias "Condition"))
   (delivery-method :accessor delivery-method :initform nil :initarg :delivery-method
	     :subelement (simple-text-element :alias "DeliveryMethod"))
   (keywords :accessor keywords :initform nil :initarg :keywords
	     :subelement (simple-text-element :alias "Keywords"))
   (title :accessor request-title :initform nil :initarg :title
	     :subelement (simple-text-element :alias "Title"))
   (author :accessor request-author :initform nil :initarg :author
	     :subelement (simple-text-element :alias "Author"))
   (item-page :accessor item-page :initform 1 :initarg :item-page
	     :subelement (simple-text-element :alias "ItemPage"))
   (merchant-id :accessor merchant-id :initform nil :initarg :merchant-id
		:subelement (simple-text-element :alias "MerchantId"))
   (response-group :accessor request-response-group :initform nil
	       :subelement (simple-text-element :alias "ResponseGroup"))
   (review-sort :accessor request-review-sort :initform nil
	       :subelement (simple-text-element :alias "ReviewSort"))
   (searchindex :accessor search-index :initform () :initarg :search-index
		:subelement (simple-text-element :alias "SearchIndex")))
  (:metaclass element-class)
  (:tags ("ItemSearchRequest"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass item-lookup-request ()
  ((item-id :accessor request-item-id :initform nil :initarg :item-id
	       :subelement (simple-text-element :alias "ItemId"))
   (id-type :initform nil :accessor request-id-type
	    :subelement (simple-text-element :alias "IdType"))
   (search-index :initform nil :accessor request-search-index
	    :subelement (simple-text-element :alias "SearchIndex"))
   (condition :initform nil :accessor request-condition
	    :subelement (simple-text-element :alias "Condition"))
   (delivery-method :initform nil :accessor request-delivery-method
		    :subelement (simple-text-element :alias "DeliveryMethod"))
   (offer-page :initform nil :accessor request-offer-page
		    :subelement (numerical-text-element :alias "OfferPage"))
   (review-page :initform nil :accessor request-review-page
		:subelement (numerical-text-element :alias "ReviewPage"))
   (review-sort-page :initform nil :accessor request-review-sort
		     :subelement (simple-text-element :alias "ReviewSort"))
   (variation-page :initform nil :accessor request-variation-page
		   :subelement (simple-text-element :alias "VariationPage"))
   (response-groups :accessor response-groups :initform () :initarg :response-groups
	       :subelement (simple-text-element :alias "ResponseGroup" :multiple t))
   (merchant-id :accessor merchant-id :initform () :initarg :merchant-id
	       :subelement (simple-text-element :alias "MerchantId")))
  (:metaclass element-class)
  (:tags ("ItemLookupRequest"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass amazon-errors ()
  ((errors :accessor errors :initarg :errors :initform nil))
  (:metaclass element-class)
  (:tags ("Request")))

;;; Amazon Item
(defgeneric title (item-like-thing)
  (:documentation "Gives the title of an item-like thing, e.g. ItemAttributes or Item"))
(defgeneric author (item-like-thing)
  (:documentation "Gives the author of an item-like thing, e.g. ItemAttributes or Item"))
(defgeneric isbn (item-like-thing)
  (:documentation "Gives the isbn of an item-like thing, e.g. ItemAttributes or Item"))

(defclass amazon-item ()
  ((item-attributes :accessor item-attributes :initform nil :initarg :item-attributes
		    :subelement (item-attributes))
   (asin :accessor item-asin :initform "" :initarg :asin :subelement (simple-text-element :alias "ASIN"))
   (detail-page-url :accessor item-detail-page-url :initform "" :initarg :detail-page-url
		    :subelement (simple-text-element :alias "DetailPageURL"))
   (sales-rank :initform 0 :subelement (numerical-text-element :alias "SalesRank"))
   (large-image :initform nil :subelement (image-element :alias "LargeImage") :accessor item-large-image)
   (small-image :initform nil :subelement (image-element :alias "SmallImage") :accessor item-small-image)
   (medium-image :initform nil :subelement (image-element :alias "MediumImage") :accessor item-medium-image)
   (image-sets :subelement (image-set-collection :alias "ImageSets") :accessor item-image-sets)
   (alternate-versions :accessor alternate-versions :initform nil :initarg :alternate-versions
		       :subelement (alternate-versions :alias "AlternateVersions"))
   (item-links :accessor item-links :initform nil :initarg :item-links
		       :subelement (item-links :alias "ItemLinks"))
   (offer-summary :accessor item-offer-summary :accessor offer-summary :initform () :initarg :offer-summary
		  :subelement (offer-summary))
   (editorial-reviews :accessor offer-editorial-reviews :initform ()
		      :subelement (editorial-review-collection :alias "EditorialReviews"))
   (offers :accessor item-offers :initform ()
	   :subelement (offers :alias "Offers")))
  (:metaclass element-class)
  (:tags ("Item"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defmethod print-object ((obj amazon-item) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (write-sequence (item-asin obj) stream)))

(defclass alternate-versions ()
  ((versions-list :accessor alternate-versions :initform nil :initarg :versions
		  :subelement (alternate-version :alias "AlternateVersion" :multiple t)))
  (:metaclass element-class)
  (:tags ("AlternateVersions")))

(defmethod xml-mop:element-value ((obj alternate-versions))
  (alternate-versions obj))

(defclass alternate-version ()
  ((title :accessor alternate-version-title :initform nil
	  :subelement (simple-text-element :alias "Title"))
   (binding :accessor alternate-version-binding :initform nil
	    :subelement (simple-text-element :alias "Binding"))
   (asin :accessor alternate-version-asin :initform nil
	 :subelement (simple-text-element :alias "ASIN")))
  (:metaclass element-class))

(defmethod print-object ((obj alternate-version) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A" (alternate-version-binding obj))))

(defclass item-links ()
  ((list :accessor item-links :initform nil :initarg :item-links
	 :subelement (item-link :alias "ItemLink" :multiple t)))
  (:metaclass element-class)
  (:tags ("ItemLinks")))

(defmethod xml-mop:element-value ((obj item-links))
  (item-links obj))

(defclass item-link ()
  ((description :accessor item-link-description :initform nil
	  :subelement (simple-text-element :alias "Description"))
   (uri :accessor item-link-uri :initform nil
	:subelement (simple-text-element :alias "URL")))
  (:metaclass element-class))

(defclass image-set ()
  ((category :initform nil :accessor image-set-category :attribute "Category")
   (swatch-image :accessor image-set-swatch-image
		 :subelement (image-element :alias "SwatchImage")
		 :initform nil)
   (large-image  :accessor image-set-large-image
		 :subelement (image-element :alias "LargeImage")
		 :initform nil)
   (small-image :subelement (image-element :alias "SmallImage")
		:accessor image-set-small-image
		:initform nil)
   (tiny-image :subelement (image-element :alias "TinyImage")
		:accessor image-set-Tiny-image
		:initform nil)
   (thumbnail-image :subelement (image-element :alias "ThumbnailImage")
		    :initform nil
		    :accessor image-set-thumbnail-image)
   (medium-image :subelement (image-element :alias "MediumImage") :accessor image-set-item-medium-image))
  (:metaclass element-class)
  (:documentation "Contains a set of images.  user contributed i guess?"))

(defclass image-set-collection ()
  ((merchant-id :initform nil :accessor image-sets-merchant-id
		:subelement (simple-text-element :alias "MerchantId"))
   (image-sets :initform nil :accessor image-sets
	       :subelement (image-set :alias "ImageSet" :multiple t)))
  (:metaclass element-class)
  (:documentation "Contains a set of images.  user contributed i guess?"))
	     
(defclass item-price-description-mixin ()
  ((lowest-new-price :accessor lowest-new-price :initform nil
		     :subelement (price-element :alias "LowestNewPrice"))
   (lowest-collectible-price :accessor lowest-collectible-price :initform nil
			     :subelement (price-element :alias "LowestCollectiblePrice"))
   (lowest-used-price :accessor lowest-used-price :initform nil
		    :subelement (price-element :alias "LowestUsedPrice"))
   (lowest-refurbished-price :accessor lowest-refurbished-price :initform nil
			     :subelement (price-element :alias "LowestRefurbishedPrice")))
  (:metaclass element-class))

(defclass offer-summary (item-price-description-mixin)
  ((totalnew :accessor summary-total-new :initform nil :initarg :total-new
	     :subelement (numerical-text-element :alias "TotalNew"))
   (total-used :accessor lowest-used-price :initform nil :initarg :total-used
	       :subelement (numerical-text-element :alias "TotalUsed"))
   (total-collectible :accessor lowest-used-price :initform nil :initarg :total-collectible
		      :subelement (numerical-text-element :alias "TotalCollectible"))
   (total-refurbished :accessor lowest-used-price :initform nil :initarg :total-refurbished
		      :subelement (numerical-text-element :alias "TotalRefurbished")))
  (:metaclass element-class)
  (:tags ("OfferSummary"))
  (:documentation "Summary of offers for a particular item"))

(defclass creator ()
  ((role :attribute "Role"))
  (:metaclass element-class)
  (:tags "Creator")
  (:documentation "Summary of offers for a particular item"))


(defclass item-attributes (item-price-description-mixin)
  ((authors :accessor item-authors :initform nil :initarg :authors
	    :subelement (simple-text-element :alias "Author" :multiple t))
   (features :accessor item-features :initform nil :initarg :features
	    :subelement (simple-text-element :alias "Feature" :multiple t))
   (height :accessor item-height :initform nil :initarg :height
	   :subelement (distance-element :alias "Height"))
   (length :accessor item-length :initform nil :initarg :length
	   :subelement (distance-element :alias "Length"))
   (width :accessor item-width :initform nil
	  :subelement (distance-element :alias "Width"))
   (weight :accessor item-weight :initform nil
	   :subelement (weight-element :alias "Weight"))
   (package-dimensions :accessor item-package-dimensions :initform nil
		       :subelement (dimensional-element :alias "PackageDimensions"))
   (list-price :accessor item-list-price :initform nil
	       :subelement (price-element :alias "ListPrice"))
   (title :accessor item-title :initform nil
	  :subelement (simple-text-element :alias "Title"))
   (upc :accessor item-upc :initform nil
	:subelement (simple-text-element :alias "UPC"))
   (ean :accessor item-ean :initform nil
	:subelement (simple-text-element :alias "EAN"))
   (sku :accessor item-sku :initform nil
	:subelement (simple-text-element :alias "SKU"))
   (isbn :accessor item-isbn :initform nil
	 :subelement (simple-text-element :alias "ISBN"))
   (edition :accessor item-edition :initform nil
	    :subelement (simple-text-element :alias "Edition"))
   (format :accessor item-format :initform nil
	    :subelement (simple-text-element :alias "format"))
   (publication-date :accessor item-publication-date :initform nil
		     :subelement (date-element :alias "PublicationDate"))
   (release-date :accessor item-release-date :initform nil
		 :subelement (date-element :alias "ReleaseDate"))
   (publisher :accessor item-publisher :initform nil
	      :subelement (simple-text-element :alias "Publisher"))
   (studio :accessor item-studio :initform nil
	   :subelement (simple-text-element :alias "Studio"))
   (label :accessor item-label :initform nil
	    :subelement (simple-text-element :alias "Label"))
   (number-of-pages :accessor item-number-of-pages :initform nil
	    :subelement (numerical-text-element :alias "NumberOfPages"))
   (reading-level :subelement (simple-text-element :alias "ReadingLevel"))
   (operating-system :subelement (simple-text-element :alias "OperatingSystem"))
   (platform :subelement (simple-text-element :alias "Platform"))
   (binding :accessor item-binding :initform nil
	    :subelement (simple-text-element :alias "Binding"))
   (dewey-decimal-number :accessor item-dewey-decimal-number :initform nil
			 :subelement (simple-text-element :alias "DeweyDecimalNumber"))
   (hardware-platform :accessor item-hardware-platform :initform "" :initarg :hardware-platform
		      :subelement (simple-text-element :alias "HardwarePlatform"))
   (item-dimensions :accessor item-dimensions :initform nil
		    :subelement (dimensional-element :alias "ItemDimensions"))
   (creators :accessor creators :initform nil
	     :subelement (creator :alias "Creator" :multiple t))
   (actors :accessor actors :initform () :initarg :actors)
   (directors :accessor directors :initform nil :initarg :directors)
   (number-of-items :accessor item-number-of-items :initform nil
		    :subelement (numerical-text-element :alias "NumberOfItems"))
   (manufacturer :accessor manufacturer :initform "" :initarg :manufacturer
		 :subelement (simple-text-element :alias "Manufacturer"))
   (product-group :accessor item-product-group :initform nil
		  :subelement (simple-text-element :alias "ProductGroup"))
   (product-type-name :accessor item-product-type-name :initform nil
		      :subelement (simple-text-element :alias "ProductTypeName"))
   (languages :accessor item-languages :initform nil
	      :subelement (languages-collection :alias "Languages")))

  (:metaclass element-class)
  (:tags "ItemAttributes")
  (:documentation "HTTPHeader element in Amazon ECS response"))

(define-chained-accessors (amazon-item item-attributes)
  item-authors item-features item-height item-length item-width
  item-weight item-package-dimensions item-list-price
  item-title item-upc item-ean item-isbn item-edition item-format item-publication-date item-release-date
  item-publisher item-studio item-label item-number-of-pages item-binding item-dewey-decimal-number
  creators actors directors item-number-of-items manufacturer item-product-group item-languages)

(defclass languages-collection ()
  ((languages :accessor languages :initform ()
	      :subelement (language :alias "Language" :multiple t)))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass language ()
  ((language-name :accessor language-name :initform nil
		  :subelement (simple-text-element :alias "Name"))
   (language-type :accessor language-type :initform nil
		  :subelement (simple-text-element :alias "Type")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass dimensional-element ()
  ((height :accessor dimension-height :initform nil
	   :subelement (distance-element :alias "Height"))
   (length :accessor dimension-length :initform nil
	   :subelement (distance-element :alias "Length"))
   (width :accessor dimension-width :initform nil
	  :subelement (distance-element :alias "Width"))
   (weight :accessor dimension-weight :initform nil
	   :subelement (weight-element :alias "Weight")))
  (:metaclass element-class))


(defclass offers ()
  ((total-offers :accessor offers-total-offers :initform nil
		 :subelement (numerical-text-element :alias "TotalOffers"))
   (total-offer-pages :accessor offers-total-pages :initform nil
		      :subelement (numerical-text-element :alias "TotalOfferPages"))
   (offers :accessor offers :initform ()
	   :subelement (offer :alias "Offer" :multiple t)))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass offer ()
  ((merchant :accessor offer-merchant :initform nil
	     :subelement (merchant :alias "Merchant"))
   (offer-attributes :accessor offer-attributes :initform nil
		     :subelement (offer-attributes :alias "OfferAttributes"))
   (seller :accessor offer-seller :initform nil :initarg :seller
	    :subelement (seller :alias "Seller"))
   (offer-listing :accessor offer-listing :initform ()
		  :subelement (offer-listing :alias "OfferListing")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass vendor-like-mixin ()
  ((average-feedback-rating :accessor average-feedback-rating :initform nil
			    :subelement (numerical-text-element :alias "AverageFeedbackRating"))
   (total-feedback :accessor total-feedback :initform nil
		   :subelement (numerical-text-element :alias "TotalFeedback")))
  (:metaclass element-class)
  (:documentation "Mixed into seller and vendor to provide shared slots for the most part"))

(defclass merchant (vendor-like-mixin)
  ((merchant-id :accessor merchant-id :initform nil
		:subelement (simple-text-element :alias "MerchantId"))
   (merchant-name :accessor merchant-name :initform nil
		  :subelement (simple-text-element :alias "Name"))
   (glancepage :accessor glance-page :initform nil
	       :subelement (simple-text-element :alias "GlancePage")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defmethod print-object ((obj merchant) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~A" (merchant-name obj))))

(defclass seller (vendor-like-mixin)
  ((seller-id :accessor seller-id :initform nil :initarg :seller-id
	      :subelement (simple-text-element :alias "SellerId")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass offer-attributes ()
  ((condition :accessor offer-condition :initform nil
	   :subelement (simple-text-element :alias "Condition"))
   (condition-note :accessor condition-note :initform nil
	   :subelement (simple-text-element :alias "ConditionNote"))
   (will-ship-expedited :accessor will-ship-expedited :initform nil
	   :subelement (simple-text-element :alias "WillShipExpedited"))
   (will-ship-international :accessor will-ship-international :initform nil
			    :subelement (simple-text-element :alias "WillShipInternational"))
   (subcondition :accessor subcondition :initform nil
		 :subelement (simple-text-element :alias "SubCondition")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))


(defclass offer-listing ()
  ((offer-listing-id :accessor offer-listing-id :initform nil
		   :subelement (simple-text-element :alias "OfferListingId"))
   (price :accessor price :initform nil
	  :subelement (price-element :alias "Price"))
   (availability :accessor availability :initform nil
		 :subelement (simple-text-element :alias "Availability"))
   (availability-attributes :accessor availability-attributes :initform nil
			    :subelement (availability-attributes :alias "AvailabilityAttributes"))
   (amount-saved :accessor amount-saved :initform nil
		 :subelement (price-element :alias "AmountSaved"))
   (percentage-saved :accessor percentage-saved :initform nil
		     :subelement (simple-text-element :alias "PercentageSaved"))
   (exchange-id :accessor exchange-id :initform nil 
		:subelement (simple-text-element :alias "ExchangeId"))
   (quantity :accessor quantity :initform nil
	     :subelement (numerical-text-element :alias "Quantity"))
   (eligible-for-saver-shipping :accessor eligible-for-saver-shipping? :initform nil
				:subelement (yes-no-element :alias "IsEligibleForSuperSaverShipping")))
   (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass availability-attributes ()
  ((availability-type :accessor availability-type :initform nil :initarg :availability-type
		      :subelement (simple-text-element :alias "AvailabilityType"))
   
   (availability-minimum-hours :accessor availability-minimum-hours :initform nil
			       :subelement (numerical-text-element :alias "MinimumHours"))
   (availability-maximum-hours :accessor availability-maximum-hours :initform nil
			       :subelement (numerical-text-element :alias "MaximumHours")))
  (:metaclass element-class)
  (:documentation "HTTPHeader element in Amazon ECS response"))



(defmacro define-chained-accessors ((from-class relation) &body accessors)
  `(progn
     ,@(mapcar (lambda (accessor)
		 (let ((obj-var (gensym "object")))
		   `(defmethod ,accessor ((,obj-var ,from-class))
		      (,accessor (,relation ,obj-var)))))
	       accessors)))

(define-chained-accessors (offer offer-attributes)
    offer-condition condition-note will-ship-expedited will-ship-international subcondition)

(define-chained-accessors (offer offer-listing)
    offer-listing-id price exchange-id quantity eligible-for-saver-shipping? availability amount-saved)


(defclass editorial-review ()
  ((source :accessor review-source :initform nil
	   :subelement (simple-text-element :alias "Source"))
   (content :accessor review-content :initform nil
	   :subelement (simple-text-element :alias "Content"))
   (link-suppress? :accessor review-link-suppressed? :initform nil
		   :subelement (yes-no-element :alias "IsLinkSuppressed")))
  (:metaclass element-class))    

(defclass editorial-review-collection ()
  ((reviews :accessor editorial-reviews  :initform nil
	    :subelement (editorial-review :alias "EditorialReview" :multiple t)))
  (:metaclass element-class))

;;;; utility
(defun my-parse-date (ugly-date-string)
  (labels ((parse-single-year (ugly-date-string)
	     (cl-ppcre:register-groups-bind
		 (single-year-string)
		 ("^([0-9]{4,4})$" ugly-date-string)
	       (if single-year-string
		   (encode-universal-time 0 0 0 1 1 (parse-number:parse-number single-year-string))
		   nil))))
    (handler-case   (parse-time ugly-date-string)
      (type-error ()
	(handler-case
	    (let ((second-attempt (parse-single-year ugly-date-string)))
	      second-attempt)
	  ;; failed to parse date
	  (type-error () nil))))))

	
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

