(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))

(defun brute-force (&optional text pattern)
  "Finds matches between text and pattern by brute-force"
  (when (null text) (setf text (get-text)))
  (when (null pattern)
    (setf pattern (elt *posix-argv* 2)))
  (let ((occ nil) (len (length pattern)))
    (loop
       for i from 0 upto (- (length text) len)
       when (string-equals (subseq text i (+ i len)) pattern) do (push i occ))
    (nreverse occ)))
