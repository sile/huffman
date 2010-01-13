(in-package :huffman)

(defstruct obj
  (cost  0 :type fixnum :read-only t))

(defstruct (code-obj (:include obj))
  (code 0 :type fixnum :read-only t))

(defstruct (packaged-obj 
	    (:include obj)
	    (:constructor package-obj (a b &aux (cost (+ (obj-cost a) (obj-cost b)))
					        (pair (cons a b)))))
  (pair '() :type list  :read-only t))

(defun package-and-merge (objs next-level-objs)
  (merge 'list (loop FOR (1st 2nd) ON objs BY #'cddr
		     WHILE 2nd
		     COLLECT (package-obj 1st 2nd))
	       next-level-objs
	 #'< :key #'obj-cost))

(defmacro objs-each ((obj objs loop-cnt) &body body)
  (let ((self (gensym))
	(cnt  (gensym)))
    `(labels ((,self (,obj)
                (if (packaged-obj-p ,obj)
		    (progn (,self (car (packaged-obj-pair ,obj)))
			   (,self (cdr (packaged-obj-pair ,obj))))
		  (progn ,@body))))
       (let ((,cnt ,loop-cnt))
	 (declare (fixnum ,cnt))
	 (dolist (,obj ,objs)
	   (when (minusp (decf ,cnt))
	     (return))
	   (,self ,obj))))))

(defun calc-code->bitlength-table (#1=code-frequency-table bit-length-limit)
  (declare (fixnum bit-length-limit)
	   #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((len (length #1#))
	 (src-objs (sort (loop FOR i FROM 0 BELOW len 
			       WHEN (plusp (aref #1# i))
			       COLLECT (make-code-obj :code i :cost (aref #1# i)))
			 #'< :key #'code-obj-cost))
    	 (bitlen-table (make-array len :initial-element 0 :element-type 'fixnum)))

    (loop REPEAT bit-length-limit 
	  FOR objs = (package-and-merge objs (copy-list src-objs)) 
	  FINALLY
	    (setf objs (package-and-merge objs '()))
	    (objs-each (o objs (1- (count-if #'plusp #1#)))
	      (incf (aref bitlen-table (code-obj-code o)))))
    bitlen-table))