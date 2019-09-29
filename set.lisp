(defclass set ()
  "\
Class representative of set data structure, utilizes
an inner hash table to verify presence of certain values"
  ((inner-hash-table
    :initform (make-hash-table :test 'equal)
    :reader inner-hash-table)))


(defgeneric set-in ((set set) value))
(defgeneric set-put ((set set) value))
(defgeneric set-remove ((set set) value))
(defgeneric set-map ((set set) func))


(defmethod set-in ((set set) value)
  (nth-value 1 (gethash (inner-hash-table set) value)))

(defmethod set-put ((set set) value)
  (setf (gethash (inner-hash-table set) value) t))

(defmethod set-remove ((set set) value)
  (remhash value (inner-hash-table set)))

(defmethod set-map ((set set) func)
  (let ((new-set (make-instance 'set)))
    (maphash
     #'(lambda (a b)
	 (put set (funcall func a)))
    new-set))
