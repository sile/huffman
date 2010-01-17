(in-package :huffman)

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

(defun traverse-obj (fn obj &optional (depth 0))
  (if (packaged-obj-p obj)
      (progn (traverse-obj fn (car (packaged-obj-pair obj)) (1+ depth))
             (traverse-obj fn (cdr (packaged-obj-pair obj)) (1+ depth)))
    (funcall fn obj depth)))

(defun count-each-level-obj (toplevel-objs level-limit)
  (let ((table (make-array level-limit :initial-element 0)))
    (dolist (o toplevel-objs)
      (traverse-obj 
       (lambda (_ level)
	 (declare (ignore _))
	 (incf (aref table level)))
       o))
    table))

(defun count-obj (objs &aux (cnt -1))
  (dolist (obj objs)
  (traverse-obj (lambda (_ __) (declare (ignore _ __)) (incf cnt))
                obj))
  cnt)

(defun to-list(obj)
  (if (packaged-obj-p obj)
      (list (to-list (car (packaged-obj-pair obj)))
            (to-list (cdr (packaged-obj-pair obj))))
    (code-obj-code obj)))

(defun calc-code->bitlength-table (#1=code-frequency-table bit-length-limit)
  (declare (fixnum bit-length-limit))
  (let ((src-objs (sort (loop FOR i FROM 0 BELOW (length #1#)
                              WHEN (plusp (aref #1# i))
                              COLLECT (make-code-obj :code i :cost (aref #1# i)))
                        #'< :key #'obj-cost))
        (bitlen-table (make-array (length #1#) :initial-element 0 :element-type 'fixnum)))
#|
    (assert (>= bit-length-limit (ceiling (log (length src-objs) 2)))
            ()
            "~A個のコードを符号化するには、最低~Aビット必要です。(指定ビット長は~A)"
            (length src-objs) (ceiling (log (length src-objs) 2)) bit-length-limit)
|#
    (loop REPEAT bit-length-limit 
          FOR objs = (package-and-merge objs (copy-list src-objs)) 
          FINALLY
	  (print (count-each-level-obj (packaging (copy-list objs)) (1+ bit-length-limit)))
          (objs-each (o (packaging objs))
            (incf (aref bitlen-table (code-obj-code o)))))
    bitlen-table))

(defun optimal-bit-length (table)
  (declare ((simple-array fixnum) table))
  (multiple-value-bind (min sum)
      (loop FOR cnt ACROSS table 
                             SUMMING  cnt INTO sum
            WHEN (plusp cnt) MINIMIZE cnt INTO min
            FINALLY (return (values min sum)))
      ;;(error (zerop sum) () "符号化
    (ceiling (- (log (/ min sum) 2)))))
          

(defun calc-bit-length-from-tree (huffman-tree bitlength-table)
  (traverse-obj  
   (lambda (obj optimal-bit-length &aux (code (code-obj-code obj)))
     (setf (aref bitlength-table code) optimal-bit-length))
   huffman-tree)
  bitlength-table)


