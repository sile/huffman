(in-package :huffman)
(declaim #.*optimize*)

(defun package-and-merge (objs next-objs)
  (merge 'list (packaging objs) next-objs #'< :key #'obj-cost))

(defmacro objs-each ((obj objs) &body body)
  (let ((self (gensym)))
    `(labels ((,self (,obj)
                (if (packaged-obj-p ,obj)
                    (progn (,self (car (packaged-obj-pair ,obj)))
                           (,self (cdr (packaged-obj-pair ,obj))))
                  (progn ,@body))))
       (dolist (,obj ,objs)
         (,self ,obj)))))

(defun calc-code->bitlength-table-under-limitation (#1=code-frequency-table bit-length-limit)
  (declare ((simple-array fixnum) #1#)
	   (fixnum bit-length-limit))
  (let ((src-objs (sort (loop FOR i FROM 0 BELOW (length #1#)
                              WHEN (plusp (aref #1# i))
                              COLLECT (make-code-obj :code i :cost (aref #1# i)))
                        #'< :key #'obj-cost))
        (bitlen-table (make-array (length #1#) :initial-element 0 :element-type 'octet)))

    (loop REPEAT bit-length-limit 
          FOR objs = (package-and-merge objs (copy-list src-objs)) 
          FINALLY
            (objs-each (o (packaging objs))
              (incf (aref bitlen-table (code-obj-code o)))))
    bitlen-table))