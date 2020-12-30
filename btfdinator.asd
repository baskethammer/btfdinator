;;;; btfdinator.asd

(asdf:defsystem #:btfdinator
  :description "BTFD-inator finds candidates for dip buying"
  :author "Baskethammer <bh@baskethammer.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:yeodl-cl)
  :components ((:file "package")
	       (:file "indicators")
	       (:file "btfdinator")))

