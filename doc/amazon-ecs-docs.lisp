(defpackage :amazon-ecs.doc
    (:use :docdown :amazon-ecs :cl))

(in-package :amazon-ecs.doc)

(progn
  (defdoc index :page
    (:title "amazon-ecs")
    (:systems :amazon-ecs); :cl-tidy.doc)
    (:content
     "#### Amazon Advertising API library for Common Lisp

## Synopsis

")

    (:sections
     (defdoc download :section
       (:title "Download and Installation")
       (:content "All the code is maintained in a git repository.  To
obtain the library, use the following command:

    git clone git://github.com/gonzojive/amazon-ecs.git

You can also browse the code at [http://github.com/gonzojive/amazon-ecs](http://github.com/gonzojive/amazon-ecs).
"))
     (defdoc functions :section
       (:title "Operations for querying Amazon.com")
       (:content "The following methods make requests to ")
       (:children
	(defdoc amazon-ecs:multiplexed-batch-item-lookup :function)
	(defdoc amazon-ecs:item-lookup :function)
	(defdoc amazon-ecs:item-search :function)
	(defdoc amazon-ecs:cart-create :function)
	))))

  (output-docs))

(defun output-docs ()
  (with-open-file (stream (asdf:system-relative-pathname (asdf:find-system :amazon-ecs)
							 "doc/index.html")
			  :direction :output :if-exists :supersede)
    (write-string (generate-html-page 'index) stream)))
   
  