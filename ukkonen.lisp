(require 'util (or (probe-file #p"util.fasl") (probe-file #p"util.lisp")))
(require 'hash-set (or (probe-file #p"hashset.fasl") (probe-file #p"hashset.lisp")))
(require 'queue (or (probe-file #p"queue.fasl") (probe-file #p"set.lisp")))

(defun next-column (pattern
		    column
		    error-size
		    character)
  (let* ((m (length pattern))
	 (new-column (make-array (1+ m) :initial-value 0)))
    (do ((i 1 (1+ i))) ((> m i) new-column)
      (setf
       (aref new-column i)
       (min
	(1+ (aref column i))
	(1+ (aref new-column (1- i)))
	(+
	 (aref column (1- i))
	 (phi character (aref pattern (1- i))))
	(1+ error-size))))))


(defun make-finite-state-machine (pattern
				  error-size
				  alphabet)
  (let* ((m (length pattern))
	 (states (make-hash-table :test 'equal))
	 (delta (make-hash-table :test 'equal))
	 (queue (make-instance 'queue))
	 (final (make-instance 'hash-set))
	 (q-zero (make-array (1+ m)))
	 (state-count 1))
    (do ((i 0 (1+ i))) ((> i m))
      (setf (aref q-zero i) (min i (1+ err))))
    (setf (gethash q-zero states) 0)
    (queue-append queue q-zero)
    (do () ((not (queue-empty queue)) (cons delta final))
      (let* ((state (queue-pop queue))
	     (i-state (gethash state queue)))
	(do ((i 0 (1+ i))) ((>= i (length alphabet)))
	  (let ((next-state (next-column
			     pattern
			     state
			     (aref alphabet i)
			     error-size))
		(i-next state-count))
	    (if (not (nth-value 1 (gethash next-state states)))
		(progn
		  (setf (gethash next-state states) state-count)
		  (incf state-count)
		  (queue-append queue next-state)
		  (when (<= (aref next-state m) error-size)
		    (hash-set-put final i-next)))
		(setf i-next (gethash next-state states)))
	    (setf (gethash (cons i-state (aref alphabet i)) delta)
		  i-next)))))))
