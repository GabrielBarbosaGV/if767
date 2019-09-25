(require 'util (or (probe-file #p"util.fasl") (probe-file #p"util.lisp")))

(defun get-goodsuffix-borders (pattern)
  (let* ((m (length pattern))
	 (b (make-array
	     m
	     ;; b will contain numbers as large as m, and thus we calculate
	     ;; the required number of bytes for these numbers
	     :element-type (unsigned-byte (1+ (round (/ m 256))))
	     :initial-element 0))
	 (i 1)
	 (j 0))
    (setf (aref b 0) -1)
    (do () ((< i m) b)
      (do () ((or
	       (>= j m)
	       (>= (+ i j) m)
	       (not (eq (aref pattern (+ i j)) (aref pattern j)))))
	(incf j)
	(setf (aref b (+ i j)) j)))))

(defun get-goodsuffix-table (pattern)
  (let* ((m (length pattern))
	 (pi (get-goodsuffix-borders pattern))
	 (pir (get-goodsuffix-borders (reverse pattern)))
	 (gs (make-array m :initial-element (- m (aref pie m)))))
    (do ((l 1 (1+ l)))
	((> l m) gs)
      (let ((j (- m 1 (aref pir l))))
	(when (< (- l (aref pir l)) (aref gs j))
	  (setf (aref gs j) (- l (aref pir l)))))))
