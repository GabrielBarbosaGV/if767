(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))

(defun brute-force (&optional text pattern)
  "Finds matches between text and pattern by brute-force"
  (when (null text) (setf text (get-text)))
  (when (null pattern)
    (setf pattern (elt *posix-argv* 2)))
  (get-occurrences text pattern :increment-function #'(lambda (i) (1+ i))))

(print (brute-force))
