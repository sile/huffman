(in-package :huffman)

(defstruct obj
  (cost  0 :type fixnum :read-only t))

(defstruct (code-obj (:include obj))
  (code 0 :type fixnum :read-only t))

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
          

(defun calc-code->bitlength-table.normal-ver (#1=code-frequency-table)
  (let ((src-objs (make-code-obj-list-from-frequency-table #1#))
        (bitlen-table (make-array (length #1#) :initial-element 0 :element-type 'fixnum)))

    (loop WITH huffman-trees = src-objs
          UNTIL (<= (length huffman-trees) 1)
          FOR (1st 2nd . rest) = huffman-trees 
      DO
        (setf huffman-trees (package-and-merge (list 1st 2nd) rest))
      FINALLY 
        (let ((huffman-tree (car huffman-trees)))
          (return (calc-bit-length-from-tree huffman-tree bitlen-table))))))

(defun make-code-obj-list-from-frequency-table (#1=code-frequency-table)
  (sort (loop FOR i FROM 0 BELOW (length #1#)
              WHEN (plusp (aref #1# i))
              COLLECT (make-code-obj :code i :cost (aref #1# i)))
        #'< :key #'code-obj-cost))

(defun calc-bit-length-from-tree (huffman-tree bitlength-table)
  (traverse-obj  
   (lambda (obj optimal-bit-length &aux (code (code-obj-code obj)))
     (setf (aref bitlength-table code) optimal-bit-length))
   huffman-tree)
  bitlength-table)


#|
[メモ]
普通のハフマン符号化と長さ制限付きハフマン符号化の計算コストを考えると、
(制限付きハフマン符号を求めるために)コードの頻度表と最大ビット長が渡された場合、
最大ビット長をオーバーしないようなら、普通のハフマン符号を求めるロジックを使った方が、効率が良さそう。

普通のハフマン符号を用いた場合に、指定ビット長をオーバするかどうか(or オーバしそうかどうか)は
多分、頻度表の各要素を一回走査するだけで求められるような気がする。
|#