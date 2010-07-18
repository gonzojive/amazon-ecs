(in-package :org.iodb.amazon.ecs)

(defun soft-sublist (list end)
  (declare (type list list)
	   (type integer end))
  (loop :for item :in list
	:for index :from 0 :upto (- end 1)
	:collect item))

(defun partition-list (list max-chunk-size)
  "Partitions list into a number of chunks, sized at most
MAX-CHUNK-SIZE.  All elements are crammed into lists of size
MAX-CHUNK-SIZE except for the last."
  (loop :for tail :on list :by (lambda (x) (nthcdr max-chunk-size x))
        :for sublist = (soft-sublist tail max-chunk-size)
        :collect sublist))

(defun multiplexed-batch-item-lookup (asins &key shared-parameters-args independent-parameters-args)
  "Given a list of items and shared/independent paramters for a batch
request, returns a list where the nth item of the list is a list of
ITEM objects that correspond to the nth ASIN in ASINS.  

e.g. (ecs:multiplexed-batch-item-lookup '(\"0321486129\" \"0131873210\") ...) =>
\(\(#<ORG.IODB.AMAZON.ECS:AMAZON-ITEM 0321486129>
  #<ORG.IODB.AMAZON.ECS:AMAZON-ITEM 0321486129>\)
 \(#<ORG.IODB.AMAZON.ECS:AMAZON-ITEM 0131873210>
  #<ORG.IODB.AMAZON.ECS:AMAZON-ITEM 0131873210>\)\)
"
  (declare (optimize (debug 3)))
  (mapcan #'(lambda (ten-asins)
              (when-let ((response-items-collections
                          (response-items
                           (perform-batch-operation
                            :item-lookup
                            :shared-parameters-args `(:item-id ,(format nil "~{~A~^,~}" ten-asins)
                                                               ,@shared-parameters-args)
                            :independent-parameters-args independent-parameters-args))))
                (apply #'mapcar #'list
                       (mapcar  #'items response-items-collections))))
          (partition-list asins 10)))


;;; In an ItemLookup request, you can include a list of up to ten
;;; comma-separated values for ItemId.  We take advantage of this to
;;; multiplex many item-lookup requests into bulk requests that
;;; perform up to ten item lookups at a time.

#+nil
(defclass multiplexed-batch-item-lookup ()
  ((asins :initarg :asins :accessor muxed-asins :initform nil)
   (shared-args :initarg :shared-parameters-args :initform nil :accessor muxed-shared-parameters-args)
   (independent-args :initarg :independent-parameters-args :initform nil :accessor muxed-independent-parameters-args)
   (lock :initform (bordeaux-threads:make-lock) :accessor muxed-lock
	 :documentation "Must be held to read/write the ASINS slot.")))


#+nil
(defun ensure-muxed-lookup-object (shared-parameters-args independent-parameters-args)
  "Given some shared parameters and independent paramters, ensures
  that a MULTIPLEXED-BATCH-ITEM-LOOKUP object exists that will handle
  the lookup.  If not, creates one."
  (make-instance 'multiplexed-batch-item-lookup
		 :independent-parameters-args independent-parameters-args
		 :shared-parameters-args shared-parameters-args))

;;; There is a thread that will perform a muxed batch lookup at most
;;; every second.  When MULTIPLEXED-BATCH-ITEM-LOOKUP is called, it
;;; adds the item ids 
;(defun multiplexed-batch-item-lookup (asin &key shared-parameters-args independent-parameters-args)
;  "Given an ASIN and shared/independent parameters for a batch
;request, returns a list of ITEM objects that correspond to the batch
;request components for that item."
;  ;; 1. add 
;  (let ((muxed-lookup (ensure-muxed-lookup-object shared-parameters-args independent-parameters-args)))
;    (bordeaux-threads:with-lock-held ((muxed-lock muxed-lookup))
;      (push asin (muxed-asins muxed-lookup)))
;    (
;  )




		    

