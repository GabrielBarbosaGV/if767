(defclass queue
    ((removal-list
      :initarg :removal-list
      :initform nil
      :reader removal-list)
     (addition-list
      :initarg :addition-list
      :initform nil
      :reader addition-list)))

(defgeneric queue-pop ((queue queue)))
(defgeneric queue-append ((queue queue) value))
(defgeneric queue-empty ((queue queue)))

(defmethod queue-pop ((queue queue))
  (cond
    ((and
      (eq () (removal-list queue))
      (eq () (addition-list queue)))
     ()) ;Return empty list if neither list has storage
    ((eq () removal-list queue)
     (pop
      (make-instance
       'queue
       :removal-list (nreverse (addition-list queue))))) ;Transform addition list into removal-list
    (t (pop removal-list)))) ;Simply remove element from removal-list

(defmethod queue-append ((queue queue) value)
  (push value (addition-list queue)))

(defmethod queue-empty ((queue queue))
  ((and
    (eq () (removal-list queue))
    (eq () (addition-list queue)))))
