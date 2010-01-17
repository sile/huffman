(in-package :huffman)
(declaim (inline obj<))

(defstruct obj
  (cost  0 :type fixnum :read-only t))

(defstruct (code-obj (:include obj))
  (code 0 :type fixnum :read-only t))

(defstruct (packaged-obj 
            (:include obj)
            (:constructor package-obj (a b &aux (cost (+ (obj-cost a) (obj-cost b)))
                                                (pair (cons a b)))))
  (pair '() :type list :read-only t))

(defun obj< (obj1 obj2)
  (< (obj-cost obj1) (obj-cost obj2)))

(defun packaging (objs &aux (head (cons :head objs)))
  "Equivalent to below (concise) expression but this function save consing.
     (loop FOR (1st 2nd) IN objs
           WHEN 2nd 
           COLLECT (package-obj 1st 2nd))"
  (nlet recur ((objs objs) (rlt head))
    (let ((1st  (first  objs))
	  (2st  (second objs))
	  (rest (cddr   objs)))
      (if (null 2st)
	  (setf (cdr rlt) nil)
	(progn (setf (second rlt) (package-obj 1st 2st))
	       (recur rest (cdr rlt))))))
  (cdr head))


