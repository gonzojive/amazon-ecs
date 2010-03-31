;;; -*- Lisp -*- mode
(defpackage #:org.iodb.amazon.ecs-system
  (:use #:cl #:asdf))
(in-package :org.iodb.amazon.ecs-system)

(defsystem :amazon-ecs
  :description "Amazon Product Advertising API library for Common Lisp"
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "LLGPL (Lisp LGPL)"
  :components ((:file "package")
	       (:file "ecs-response-model" :depends-on ("package" ))
	       (:file "generate-uri" :depends-on ("package"))
	       (:file "query-ecs" :depends-on ("ecs-response-model" "generate-uri"))
	       (:file "operations" :depends-on ("ecs-response-model" "generate-uri" "query-ecs"))
	       (:file "mux-itemlookup" :depends-on ("operations")))
  :depends-on ("cl-ppcre" "trivial-http" "net-telent-date" "xml-mop" "parse-number" "drakma" "ironclad"
			  "hunchentoot" "bordeaux-threads" "alexandria"))

(setf (asdf:component-property (asdf:find-system :amazon-ecs) :website)
      "http://github.com/gonzojive/amazon-ecs")