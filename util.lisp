(in-package :huffman)

(defun formalize-letargs (args)
  (mapcar (lambda (a) (if (atom a) (list a) a)) args))

(defmacro nlet (fn-name letargs &body body)
  (setf letargs (formalize-letargs letargs))
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

