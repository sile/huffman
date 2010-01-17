(in-package :asdf)

(defsystem huffman
  :name    "huffman"
  :version "0.1.0"
  :author  "Takeru Ohta"
  :description "Common Lisp Huffman coding library"

  :depends-on (:bitop)

  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "obj")
	       (:file "heap")
	       (:file "huffman")
	       (:file "length-limited-huffman")
	       (:file "encode")))
		      

