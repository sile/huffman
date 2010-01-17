(defpackage :huffman
  (:use :common-lisp :bitop)
  (:export gen-encode-table
	   restore-encode-table
	   make-decoder))
(in-package :huffman)

(deftype array-index () '(integer 0 #.array-total-size-limit))
(deftype simple-octets () `(simple-array (unsigned-byte 8))) 
(deftype octet () '(unsigned-byte 8))

(defparameter *optimize* '(optimize (speed 3) (safety 0) (compilation-speed 0)))
