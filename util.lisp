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

(defun get-occurrences (text pattern &key (increment-function (lambda (i) (1+ i))))
  "\
Loops over text attempting to match pattern against it,
returning occurrences's indices, increment-function is used to
increment search index. If not provided, defaults to
incrementing by one, in equivalence to the
brute-force approach"
  (let ((occ nil))
    (do ((i 0 (funcall increment-function i))) ((> i (- (length text) (length pattern))))
      (when (string-equals (subseq text i (+ i (length pattern))) pattern)
	(push i occ)))
    (nreverse occ)))

(defun submit (occurrences)
  "Imprime valores de occurrences"
  (format t "~{~a ~}" occurrences))
