(in-package :asdf)

(defsystem huffman
  :name    "huffman"
  :version "0.0.1"
  :author  "Takeru Ohta"
  :description "Common Lisp Huffman coding library"

  :depends-on (:bitop)

  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "obj")
	       (:file "length-limited-huffman")
	       (:file "heap")
	       (:file "huffman")))

