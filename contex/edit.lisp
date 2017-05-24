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

(defun sentences (line)
    (split-with line (list #\! #\. #\?)))

(defun edit-lines (replacements inputlines)
    (mapcar (lambda (line) (remove-redundant replacements line)) inputlines))

