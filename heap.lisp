(in-package :huffman)

(defvar *init-obj* (make-obj :cost 0))
(defstruct (heap2 (:constructor mkheap2 (size-limit
					 &aux (buf (make-array (1+ size-limit)
						     :initial-element *init-obj*
					             :element-type 'obj)))))
  (size 0 :type fixnum)
  (buf #() :type (simple-array obj)))

(defun upheap (k buf)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
	   ((simple-array obj) buf)
	   (fixnum k))
  (loop WITH v = (aref buf k)
	FOR  k/2 = (ash k -1)
	WHILE (obj< v (aref buf k/2))
    DO
      (setf (aref buf k) (aref buf k/2)
	    k k/2)
    FINALLY
      (setf (aref buf k) v)))

(defun hp2-insert (obj hp)
  (with-slots (size buf) (the heap2 hp)
    (setf (aref buf (incf size)) obj)
    (upheap size buf)))

(defun downheap (k size buf &aux (v (aref buf k)))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
	   ((simple-array obj) buf)
	   (fixnum k size))
  (loop WITH size/2 = (ash size -1)
	WHILE (<= k size/2) DO
    (let ((j (+ k k)))
      (and (< j size) 
	   (not (obj< (aref buf j) (aref buf (1+ j))))
	   (incf j))
      (when (obj< v (aref buf j))
	(return))
      (setf (aref buf k) (aref buf j)
	    k j)))
  (setf (aref buf k) v))

(defun hp2-pop (hp)
  (with-slots (size buf) (the heap2 hp)
    (prog1 (aref buf 1)
      (setf (aref buf 1) (aref buf size))
      (decf size)
      (downheap 1 size buf))))

(defun make-heap2 (table &aux (len (count-if #'plusp table)))
  (let ((hp (mkheap2 len)))
    (loop FOR code FROM 0 BELOW (length table)
	  FOR freq OF-TYPE fixnum = (aref table code)
	  WHEN (plusp freq) 
	  DO (setf (aref (heap2-buf hp) (incf (heap2-size hp))) (make-code-obj :cost freq :code code)))
    (loop FOR k FROM (ash (heap2-size hp) -1) DOWNTO 1 DO
      (downheap k (heap2-size hp) (heap2-buf hp)))
    hp))

(defun heap2-pop2 (hp)
  (values (hp2-pop hp) (hp2-pop hp)
	  (= 0 (heap2-size hp))))

(defun heap2-push-and-pop2 (obj hp)
  (hp2-insert obj hp)
  (heap2-pop2 hp))


(defstruct (heap (:constructor mkheap (list)))
  list)

(defun make-heap (objs)
  (mkheap (sort objs #'< :key #'obj-cost)))

(defun heap-pop2 (heap)
  (with-slots (list) (the heap heap)
    (values (pop list) (pop list) (null list))))

(defun heap-insert (obj head &aux (cost1 (obj-cost obj)))
  (labels ((self (head &aux (list (cdr head)))
             (if (or (null list)
		     (< cost1 (obj-cost (first list))))
		 (setf (cdr head) (cons obj list))
	       (self (cdr head)))))
    (self head)))

(defun heap-push-and-pop2 (obj heap)
  (with-slots (list) (the heap heap)
    (heap-insert obj list)
    (heap-pop2 heap)))

#|
(defpackage :pairing-heap
  (:use :cl)
  (:nicknames :heap)
  (:shadow :push :pop)
  (:export :make-heap
	   :push  
	   :pop
	   :empty?))
(in-package :pairing-heap)

;;; struct
(defstruct heap 
  node
  (test #'<)) ; 比較関数

(defstruct node
  element
  childs)     ; nodeのリスト

;;; internal function
;; nodeをマージ
(defun merge-node (n1 n2 <)
  (cond ((null n1) n2)
	((null n2) n1)
	(t (if (funcall < #1=(node-element n1) #2=(node-element n2))
	       (make-node :element #1# :childs (cons n2 (node-childs n1)))
	     (make-node :element #2# :childs (cons n1 (node-childs n2)))))))

;; nodeのリストをマージ
(defun merge-pairs (nodes <)
  (if (null (cdr nodes))
      (car nodes)
    (destructuring-bind (first second . rest) nodes
      (merge-node (merge-node first second <) 
		  (merge-pairs rest <)
		  <))))

;;; external function
(defun push (element heap) 
  (setf (heap-node heap) (merge-node (make-node :element element) 
				     (heap-node heap) 
				     (heap-test heap)))
  heap)

(defun pop (heap)
  (let ((root (heap-node heap)))
    (when root
      (prog1 (node-element root)
	(setf (heap-node heap) (merge-pairs (node-childs root) (heap-test heap)))))))

(defun empty? (heap)
  (null (heap-node heap)))
|#