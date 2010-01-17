(in-package :huffman)
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)))

(defun count-code-bitlength (huffman-tree code-limit)
  (declare (fixnum code-limit))
  (let ((table (make-fixnum-array code-limit)))
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

(defun restore-encode-table (bitlengths &optional reverse)
  ;; TODO: check-type
  (declare ((simple-array fixnum) bitlengths))
  (let* ((bitlen-limit (1+ (loop FOR len ACROSS bitlengths MAXIMIZE len)))
	 (len-count    (make-fixnum-array bitlen-limit))
	 (code-limit   (length bitlengths)))
    ;;
    (loop FOR len ACROSS bitlengths 
	  WHEN (plusp len)
	  DO   (incf (aref len-count len)))
    ;;
    (let ((base-num-per-len (make-fixnum-array bitlen-limit)))
      (loop FOR i FROM 1 BELOW bitlen-limit DO
        (setf (aref base-num-per-len i)
	      (ash (+ (aref base-num-per-len (1- i))
		      (aref len-count        (1- i)))
		   1)))
      ;;
      (let ((codes (make-array code-limit :initial-element (fixbyte 0 0) :element-type 'fixbyte)))
	(loop FOR i FROM 0 BELOW code-limit
	      FOR code-bit-len = (aref bitlengths i)
	      WHEN (plusp code-bit-len) DO
	  (setf (aref codes i)
		(fixbyte (aref base-num-per-len code-bit-len) code-bit-len :reverse reverse))
	  (incf (aref base-num-per-len code-bit-len)))
	codes))))

(defun gen-encode-table (#1=code-frequency-table &key length-limit reverse)
  (check-type #1# (simple-array fixnum))
  (let ((code->bitlen (if (null length-limit)
			  (calc-code->bitlength-table #1#)
			(calc-code->bitlength-table-under-limitation #1# length-limit))))
    (restore-encode-table code->bitlen reverse)))

(defun ready-decode-table (encode-table)
  (declare ((simple-array fixbyte) encode-table))
  (let* ((bitlen-limit (1+ (loop FOR fb ACROSS encode-table MAXIMIZE (fixbyte-length fb))))
	 (decode-table (make-array bitlen-limit :initial-element nil))
	 (last-code-per-len (make-array bitlen-limit :initial-element -1 :element-type 'fixnum)))

    (loop FOR fb  ACROSS encode-table 
	  FOR len = (fixbyte-length fb) 
	  FOR code OF-TYPE fixnum = 0 THEN (1+ code)
	  WHEN (plusp len) DO
      (push code (aref decode-table len))
      (setf (aref last-code-per-len len) (fixbyte-fixnum fb)))

    (dotimes (i bitlen-limit (values decode-table last-code-per-len))
      (setf #1=(aref decode-table i) (coerce (the list #1#) 'vector)))))
    
(defun make-decoder (encode-table)
  (let ((len 0)
	(num 0))
    (declare (fixnum len num))
    (multiple-value-bind (decode-table last-code-per-len) 
			 (ready-decode-table encode-table)
      (declare ((simple-array fixnum) last-code-per-len))
      (lambda (bit)
	(declare (bit bit))
	(incf len)
	(setf num (+ (ash num 1) bit))
	(let ((upper (aref last-code-per-len len)))
	  (when (<= num upper)
	    (prog1
		(svref (aref decode-table len) (- upper num))
	      (setf len 0
		    num 0))))))))