(defun get-mismatch-position (first second)
  "Get index of mismatch position between two equally long strings,
returns -1 if not found"
  (unless (not (eql (length first) (length second)))
    (or
     (loop
	for i from 0
	for a across first
	for b across second
	when (not (eql a b)) return i) -1)))

(defun string-equals (first second)
  "Determines if two strings are equal"
  (= (get-mismatch-position first second) -1))

(defun get-text ()
  "Reads file with with filepath = (elt *posix-argv* 1) and returns it as
a string"
  (with-open-file (in (elt *posix-argv* 1))
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))
