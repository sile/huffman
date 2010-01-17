(defpackage :huffman
  (:use :common-lisp :bitop)
  (:export gen-encode-table
	   restore-encode-table
	   make-decoder))
(in-package :huffman)

(deftype array-index () '(integer 0 #.array-total-size-limit))
