(in-package :huffman)
(declaim #.*optimize*
	 (ftype (function (array-index) (simple-array obj)) init-heap-buf)
         (inline upheap downheap heap-push heap-pop heap-pop2 heap-push-and-pop2))

(defstruct (heap (:constructor heap (size-limit &aux (buf (init-heap-buf size-limit)))))
  (size 0   :type array-index)
  (buf  #() :type (simple-array obj))) 

(defun init-heap-buf (size-limit)
  (make-array (1+ (the fixnum size-limit))
              :initial-element (load-time-value (make-obj :cost 0) t)
              :element-type 'obj))

(defun upheap (i buf)
  (declare ((simple-array obj) buf)
           (fixnum i))
  (loop WITH obj = (aref buf i)
        FOR  i/2 = (ash i -1)
        WHILE (obj< obj (aref buf i/2))
    DO
      (setf (aref buf i) (aref buf i/2)
                      i            i/2)
    FINALLY
      (setf (aref buf i) obj)))

(defun heap-push (obj heap)
  (with-slots (size buf) (the heap heap)
    (setf (aref buf (incf size)) obj)
    (upheap size buf)))

(defun downheap (i size buf &aux (obj (aref buf i)))
  (declare ((simple-array obj) buf)
           (fixnum i size))
  (loop WITH size/2 = (ash size -1)
        WHILE (<= i size/2) DO
    (let ((child (+ i i)))
      (when (and (< child size) 
                 (obj< (aref buf (1+ child)) (aref buf child)))
        (incf child))
      (when (obj< obj (aref buf child))
        (return))
      (setf (aref buf i) (aref buf child)
                      i            child)))
  (setf (aref buf i) obj))

(defun heap-pop (heap)
  (with-slots (size buf) (the heap heap)
    (prog1 (aref buf 1)
      (setf (aref buf 1) (aref buf size))
      (decf size)
      (downheap 1 size buf))))

(defun make-heap (#1=code-frequency-table)
  (declare ((simple-array fixnum) #1#))
  (let ((heap (heap (count-if #'plusp #1#))))
    (with-slots (buf size) heap
      (loop FOR code FROM 0 BELOW (length #1#)
            FOR freq OF-TYPE fixnum = (aref #1# code)
            WHEN (plusp freq) 
            DO (setf (aref buf (incf size)) (make-code-obj :cost freq :code code)))
      
      (loop FOR k FROM (ash size -1) DOWNTO 1 
            DO (downheap k size buf)))
    heap))

(defun heap-pop2 (heap)
  (values (heap-pop heap) (heap-pop heap) (zerop (heap-size heap))))

(defun heap-push-and-pop2 (obj heap)
  (heap-push obj heap)
  (heap-pop2 heap))