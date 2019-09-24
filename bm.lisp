(require 'util (or (probe-file "util.fasl") (probe-file "util.lisp")))
(require 'badchar (or (probe-file "badchar.fasl") (probe-file "badchar.lisp")))

(defun badchar-increment (text pattern)
  (let ((badacc (get-badchar-hashfunc pattern)))
    (lambda (mp i)
      (let ((char-pos
	     (funcall badacc (aref text (max 1 (+ mp i))))))
	(max 1 (- mp char-pos))))))

(defun boyer-moore (text pattern)
  (get-occurrences
   text pattern
   :backwards t
   :increment-function (badchar-increment text pattern)))
