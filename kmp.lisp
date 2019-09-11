(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'borders (or (probe-file "borders.fasl") (probe-file "borders.lisp")))

(defun knuth-morris-pratt (&optional text pattern)
  (when (null text) (setf text (get-text)))
  (when (null pattern)
    (setf pattern (elt *posix-argv* 2)))
  (let ((occ nil) (borders (get-borders pattern)) (len (length pattern)))
    (loop
       for i from 0 upto (- (length text) len) do
	 (let ((mismatch-position (get-mismatch-position (subseq text i (+ i len)) pattern)))
	 (if (= -1 mismatch-position)
	     (push i occ)
	     (incf i (aref borders mismatch-position)))))
    (nreverse occ)))

(print (knuth-morris-pratt))
