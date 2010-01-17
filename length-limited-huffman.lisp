(in-package :huffman)
(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)))

(defstruct (packaged-obj 
            (:include obj)
            (:constructor package-obj (a b &aux (cost (+ (obj-cost a) (obj-cost b)))
                                                (pair (cons a b)))))
  (pair '() :type list :read-only t))

(defun packaging (objs &aux (head (cons :head objs)))
  "Equivalent to below (concise) expression but this function save consing.
     (loop FOR (1st 2nd) IN objs
           WHEN 2nd 
           COLLECT (package-obj 1st 2nd))"
  (labels ((recur (objs rlt &aux (1st  (first  objs))
                                 (2st  (second objs))
                                 (rest (cddr   objs)))
             (if (null 2st)
                 (setf (cdr rlt) nil)
               (progn (setf (second rlt) (package-obj 1st 2st))
                      (recur rest (cdr rlt))))))
    (recur objs head))
  (cdr head))

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
        (bitlen-table (make-array (length #1#) :initial-element 0 :element-type 'fixnum)))

    (loop REPEAT bit-length-limit 
          FOR objs = (package-and-merge objs (copy-list src-objs)) 
          FINALLY
            (objs-each (o (packaging objs))
              (incf (aref bitlen-table (code-obj-code o)))))
    bitlen-table))