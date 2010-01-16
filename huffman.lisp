(in-package :huffman)

(deftype fixnum/2 () '(integer 0 #.(ash most-positive-fixnum -1)))

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
	 (inline make-node frequency element freq< freq+)
	 (ftype (function (list) fixnum/2) frequency)
	 (ftype (function (list list) fixnum) freq+)) 

;; ハフマン木のノード用の関数群
(defun make-node (frequency element) (cons frequency element))  
(defun frequency (node)              (car node)) 
(defun element   (node)              (cdr node)) ; element = character or list(sub-tree)
(defun freq<     (node1 node2)       (< (frequency node1) (frequency node2)))
(defun freq+     (node1 node2)       (+ (frequency node1) (frequency node2)))
#|
(defun count-code-bit-length (huffman-tree code-limit)
  (declare (fixnum code-limit))
  (let ((table (make-array code-limit :initial-element 0 :element-type 'fixnum)))
    (labels ((self (tree depth)
	       (declare (fixnum depth))
               (when tree
		 (if (atom #1=(element tree))
		     (setf (aref table #1#) depth)
		   (destructuring-bind (left . right) #1#
		     (self left  (1+ depth))
		     (self right (1+ depth)))))))
      (self huffman-tree 0))
    table))

;; ハフマン木を作成
(defun make-huffman-tree (#1=code-frequency-table &aux (code-limit (length #1#)))
  #+ SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((hp (heap:make-heap :test #'freq<)))
    ;; 1] 初期heap作成
    (dotimes (code code-limit)
      (unless (zerop (the fixnum (aref #1# code)))
	(heap:push (make-node (aref #1# code) code) hp)))

    ;; 2] heapが空になるまで、「頻度最小の二つのnodeを取り出し、併合して、heapに戻す」という操作を繰り返す
    (do ((node #2=(heap:pop hp) #2#))
        ((heap:empty? hp) (count-code-bit-length node code-limit)) ; heapに最後に残ったnodeがハフマン木
      (let ((node2 (heap:pop hp)))
        (heap:push (make-node (freq+ node node2) (cons node node2)) hp)))))
|#


(defun count-code-bit-length (huffman-tree code-limit)
  (declare (fixnum code-limit))
  (let ((table (make-array code-limit :initial-element 0 :element-type 'fixnum)))
    (labels ((self (tree depth)
	       (declare (fixnum depth))
               (when tree
		 (if (not (packaged-obj-p tree))
		     (setf (aref table (code-obj-code tree)) depth)
		   (destructuring-bind (left . right) (packaged-obj-pair tree)
		     (self left  (1+ depth))
		     (self right (1+ depth)))))))
      (self huffman-tree 0))
    table))

#|
;; ハフマン木を作成
(defun make-huffman-tree (#1=code-frequency-table &aux (code-limit (length #1#)))
  #+ SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((hp (heap:make-heap :test (lambda (a b)
				    (< (obj-cost a) (obj-cost b))))))
    ;; 1] 初期heap作成
    (dotimes (code code-limit)
      (unless (zerop (the fixnum (aref #1# code)))
	(heap:push (make-code-obj :cost (aref #1# code) :code code) hp)))

    ;; 2] heapが空になるまで、「頻度最小の二つのnodeを取り出し、併合して、heapに戻す」という操作を繰り返す
    (do ((node #2=(heap:pop hp) #2#))
        ((heap:empty? hp) (count-code-bit-length node code-limit)) ; heapに最後に残ったnodeがハフマン木
      (let ((node2 (heap:pop hp)))
        (heap:push (package-obj node node2) hp)))))
|#
(defun make-huffman-tree (#1=code-frequency-table &aux (code-limit (length #1#)))
  #+ SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((hp (make-heap (loop FOR code FROM 0 BELOW code-limit 
			     FOR freq OF-TYPE fixnum = (aref #1# code)
			     WHEN (plusp freq) 
			     COLLECT (make-code-obj :cost freq :code code)))))
    (labels ((self (obj1 obj2 end?)
               (if end?
		   (count-code-bit-length (package-obj obj1 obj2) code-limit)
		 (multiple-value-call #'self (heap-push-and-pop2 (package-obj obj1 obj2) hp)))))
      (multiple-value-call #'self (heap-pop2 hp)))))

(defun make-huffman-tree2 (#1=code-frequency-table &aux (code-limit (length #1#)))
  #+ SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((hp (make-heap2 #1#)))
    (labels ((self (obj1 obj2 end?)
               (if end?
		   (count-code-bit-length (package-obj obj1 obj2) code-limit)
		 (multiple-value-call #'self (heap2-push-and-pop2 (package-obj obj1 obj2) hp)))))
      (multiple-value-call #'self (heap2-pop2 hp)))))