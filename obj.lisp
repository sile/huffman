(in-package :huffman)

(defstruct obj
  (cost  0 :type fixnum :read-only t))

(defstruct (code-obj (:include obj))
  (code 0 :type fixnum :read-only t))

(declaim (inline obj<))
(defun obj< (obj1 obj2)
  (< (obj-cost obj1) (obj-cost obj2)))
