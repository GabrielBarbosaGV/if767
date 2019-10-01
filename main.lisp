(require 'bruteforce (or (probe-file "bruteforce.fasl") (probe-file "bruteforce.lisp")))
(require 'kmp (or (probe-file "kmp.fasl") (probe-file "kmp.lisp")))
(require 'boyer-moore (or (probe-file "bm.fasl") (probe-file "bm.lisp")))
(require 'sellers (or (probe-file "sellers.fasl") (probe-file "sellers.lisp")))
(require 'ukkonnen (or (probe-file "ukkonnen.fasl") (probe-file "ukkonen.lisp")))

(defvar *needs-help* nil
  "If help should be printed")

(defvar *edit-distance* 0
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

(defun remove-leading-dashes (opt-str)
  (do ((i 0 (1+ i))) ((eq #\- (aref opt-str i)) (subseq opt-str i))))

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

(defun process-option (opt-str list-cdr)
  (let ((option (remove-leading-dashes opt-str))
	(parameter (car list-cdr)))
    (configure-parameter option parameter)))

(defun process-opts ()
  (when (= (length *posix-argv* 1)) (setf *needs-help* t))
  (do ((arg (cdr *posix-argv*) (cdr arg))) ((or *needs-help* (null arg)))
    (let ((opt-str (car arg)))
      (when (eq (aref opt-str 0) #\-)
	  (process-option opt-str (cdr arg))))))
