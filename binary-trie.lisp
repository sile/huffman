(in-package :huffman)

(defstruct (trie (:constructor make-trie ()))
  value
  (subtries (vector nil nil) :type (simple-vector 2)))

(declaim (ftype (function (trie) fixnum) element-count))
(defmethod print-object ((o trie) stream)
  (declare (optimize (speed 0)))
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~S ~D" :count (element-count o))))

(defmacro get-node1 (key trie) `(svref (trie-subtries ,trie) ,key))
(defmacro get-elem1 (trie)     `(trie-value ,trie))

(defmacro each-bit-from-lower ((bit fbyte &optional result-form) &body body)
  (let ((num (gensym))
	(len (gensym))
	(i   (gensym)))
    `(let ((,num (fixbyte-fixnum ,fbyte))
	   (,len (fixbyte-length ,fbyte)))
       (dotimes (,i ,len ,result-form)
	 (let ((,bit (ldb (byte 1 ,i) ,num)))
	   ,@body)))))

(defmacro a.if (expr then else)
  `(let ((it ,expr))
     (if it
	 ,then
       ,else)))

(defun gen-node-force (fbyte trie)
  (each-bit-from-lower (bit fbyte trie)
    (a.if #1=(get-node1 bit trie)
	(setf trie it)
      (setf trie (setf #1# (make-trie))))))

(defun get-node (fbyte trie)
  (each-bit-from-lower (bit fbyte trie)
    (a.if (get-node1 bit trie)
        (setf trie it)
      (return-from get-node))))

(defun get-elem (fbyte trie)
  (a.when (get-node fbyte trie)
    (values (get-elem1 it) it)))

(defsetf get-elem (fbyte trie) (new-value)
  `(let ((terminal-node (get-node-force ,fbyte ,trie)))
     (setf (get-elem1 terminal-node) ,new-value)
     ,new-value))

