(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'badchar (or (probe-file "badchar.fasl") (probe-file "badchar.lisp")))

(defun badchar-increment (text pattern)
  (let ((badacc (get-badchar-hashfunc pattern)))
    (lambda (i)
      (let* ((slice (subseq text i (length pattern)))
	     (mismatch-position (get-mismatch-position slice pattern))
	     (c (aref pattern mismatch-position)))
	(- mismatch-position (funcall badacc c)))))))

(defun boyer-moore (&optional text pattern)
  (unless text (setf text (get-text)))
  (unless pattern
    (setf pattern (elt *posix-argv* 2)))
  (get-occurrences text pattern
		   :increment-function (badchar-increment text pattern)))
