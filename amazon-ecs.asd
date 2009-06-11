;;; -*- Lisp -*- mode
(defpackage #:org.iodb.amazon.ecs-system
  (:use #:cl #:asdf))
(in-package :org.iodb.amazon.ecs-system)

(defsystem :amazon-ecs
  :description "Amazon E-Commerce Service Library"
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:file "package")
	       (:file "ecs-response-model" :depends-on ("package" ))
	       (:file "query-ecs" :depends-on ("ecs-response-model")))
  :depends-on ("cl-ppcre" "trivial-http" "net-telent-date" "xml-mop" "parse-number" "drakma"))
