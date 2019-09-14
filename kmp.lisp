(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'borders (or (probe-file "borders.fasl") (probe-file "borders.lisp")))

(defun kmp-increment (text pattern)
  (let ((borders (get-borders pattern)))
    (lambda (i)
      (let* ((slice (subseq text i (+ i (length pattern))))
	     (mismatch-position (get-mismatch-position slice pattern)))
	(1+ (+ i (aref borders (max mismatch-position 0))))))))

(defun knuth-morris-pratt (&optional text pattern)
  (when (null text) (setf text (get-text)))
  (when (null pattern)
    (setf pattern (elt *posix-argv* 2)))
  (get-occurrences text pattern
		   :increment-function (kmp-increment text pattern)))

(print (knuth-morris-pratt))
