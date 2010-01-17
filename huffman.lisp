(in-package :huffman)
(declaim #.*optimize*)

(defun count-code-bitlength (huffman-tree code-limit)
  (declare (fixnum code-limit))
  (let ((table (make-array code-limit :element-type 'octet :initial-element 0)))
    (nlet self ((tree huffman-tree) (depth 0))
      (declare (fixnum depth))
      (when tree
	(if (not (packaged-obj-p tree))
	    (setf (aref table (code-obj-code tree)) depth)
	  (destructuring-bind (left . right) (packaged-obj-pair tree)
	    (self left  (1+ depth))
	    (self right (1+ depth))))))
    table))

(defun calc-code->bitlength-table (#1=code-frequency-table)
  (declare ((simple-array fixnum) #1#))
  (let ((heap (make-heap #1#)))
    (labels ((iter (1st 2nd end?)
               (if end?
		   (count-code-bitlength (package-obj 1st 2nd) (length #1#))
		 (multiple-value-call #'iter (heap-push-and-pop2 (package-obj 1st 2nd) heap)))))
      (multiple-value-call #'iter (heap-pop2 heap)))))
