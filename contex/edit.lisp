(load "contex/util.lisp")

(defun build-replacements (filelines)
    (let ((replacement-pairs (mapcar (lambda (line) (split line #\:)) filelines)))
        (loop for pair in replacement-pairs
            append (loop for key in (split (elt pair 0))
                collect (list key (elt pair 1))))))

(defun remove-redundant (replacements line)
    (loop for replacement in replacements
        do (let ((a (elt replacement 0))
            (b (elt replacement 1)))
            (setf line (replace-all line a b))))
    (return-from remove-redundant line))

(defparameter *sentence-chars* (list #\! #\. #\?))
(defparameter *sentence-strs*  (mapcar 'to-str *sentence-chars*))

(defun sentences (line)
    (split-with line *sentence-chars* t))

(defun unsentences (sentences)
  (let ((line ""))
    (loop for s in sentences
          when (find s *sentence-strs*) do (setf line (concatenate 'string line " "))
          (setf line (concatenate 'string line s))))

(defun re-capitalize (line)
  (string-upcase line :start 0 :end 1))

(defun fix-sentences (line)
  (sentences line))

(defun edit-lines (replacements inputlines)
    (mapcar (lambda (line) (remove-redundant replacements line)) inputlines))

(show (re-capitalize "this sentence no verb"))
(show (fix-sentences "to write. better? no"))
(show (sentences "this. sentence? is wierd."))
(show (unsentences (sentences "this. sentence? is weird.")))
