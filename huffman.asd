(in-package :asdf)

(defsystem huffman
  :name    "huffman"
  :version "0.0.1"
  :author  "Takeru Ohta"
  :description "Common Lisp Huffman coding library"
  
  :serial t
  :components ((:file "package")
	       (:file "length-limited-huffman")))
