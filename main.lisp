(require 'bruteforce (or (probe-file "bruteforce.fasl") (probe-file "bruteforce.lisp")))
(require 'kmp (or (probe-file "kmp.fasl") (probe-file "kmp.lisp")))
(require 'boyer-moore (or (probe-file "bm.fasl") (probe-file "bm.lisp")))
(require 'sellers (or (probe-file "sellers.fasl") (probe-file "sellers.lisp")))
(require 'ukkonnen (or (probe-file "ukkonnen.fasl") (probe-file "ukkonen.lisp")))

(defvar *needs-help* nil
  "If help should be printed")

(defvar *edit-distance* nil
  "How much given pattern can be edited")

(defvar *pattern-file* nil
  "Path to pattern file")

(defvar *algorithm_name* nil
  "Name of algorithm to execute")

(defvar *positional-args-start* 1
  "Position in argument list where positional arguments start")

(defvar *unknown-options* nil
  "List of unknown passed options")


(defun is-among (value &rest values)
  "\
Tests for membership of value among values,
used for brevity"
  (member value values :test 'equal))

(defun process-opts ()
  (when (= (length *posix-argv* 1)) (setf *needs-help* t))
  (do ((arg (cdr *posix-argv*) (cdr arg))) ((or *needs-help* (null arg)))
    (let ((opt-str (car arg)))
      (when (eq (aref opt-str 0) #\-)
	  (process-option opt-str (cdr arg))))))

(defun process-option (opt-str list-cdr)
  (let ((option (remove-leading-dashes opt-str))
	(parameter (car list-cdr)))
    (configure-parameter option parameter)))

(defun remove-leading-dashes (opt-str)
  (do ((i 0 (1+ i)))
      ((not (eq #\- (aref opt-str i)))
       (subseq opt-str i))))

(defun configure-parameter (option parameter)
  "\
Switch function to determine what parameters to set according
to functions"
  (cond
    ((is-among option "help" "h")
     (setf *needs-help* t)
     (incf *positional-arguments-start*))
    ((is-among option "edit" "e")
     (setf *edit-distance* (parse-integer parameter))
     (incf *positional-arguments-start* 2))
    ((is-among option "pattern" "p")
     (setf *pattern-file* parameter)
     (incf *positional-arguments-start* 2))
    ((is-among option "algorithm" "a")
     (setf *algorithm-name* parameter)
     (incf *positional-arguments-start* 2))
    ((is-among option "count" "c")
     (setf *count-only* t)
     (incf *positional-arguments-start*))
    (t (push
	(concatenate 'string "Unknown option:" parameter)
	*unknown-options*))))

(defun run-algorithm-for-files ()
  (let ((file-paths (get-file-paths)))
    (do ((cur-file-path file-paths (cdr cur-file-path)))
	((null cur-file-path))
      (run-algorithm-for-file (car cur-file-path)))))

(defun get-file-paths ()
  (let ((start-position (if (null *pattern-string*)
			    *positional-arguments-start*
			    (1+ *positional-arguments-start*))))
    (do ((c (elt *posix-argv* start-position) (cdr c))
	 (files nil))
	((null c) (nreverse files))
      (push (car c) files))))

(defun run-algorithm-for-file (file-path)
  (let ((patterns (get-patterns))
	(algorithms (decide-algorithms patterns))
	(occlists nil))
    (with-open-file (in file-path)
      (do ((l (read-line in nil) (read-line in nil))) ((null l))
	(do ((pattern patterns (cdr pattern)))
	    ((null pattern))
	  (push
	   (funcall (gethash (car pattern) algorithms)
		    l pattern *error-size*)
	   occlists))
	(format
	 t
	 "~{~{~a: ~a~^ ~} ~}~%~a"
	 (mapcar #'list patterns occlists)
	 l)))))

(defun get-patterns ()
  (if (null *pattern-file*) (list (elt *posix-argv* *positional-arguments-start*))
      (with-open-file (in *pattern-file*)
	(do ((l (read-line in nil) (read-line in nil))
	     (patterns nil))
	    ((null l) (nreverse patterns))
	  (push l patterns)))))

(defun decide-algorithms (patterns)
  (if (not (null *algorithm-name))
      (decide-algorithms-by-name patterns)
      (decide-algorithms-by-arguments patterns)))

(defun decide-algorithms-by-name (patterns)
  (cond
    ; TODO TODO TODO

