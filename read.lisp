(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
         while line
         collect line)))

(defun write-file (filename lines)
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create )
      (dolist (line lines)
          (format out "~a~%" line))))

(defun split (line)
    "Split key value pair by seperating : "
    (let ((index (position #\: line)))
        (cond ((not index) nil)
            (t (list (subseq line (+ 1 index))
                  (subseq line 0 index)))
            )))

(defun replace-all (s part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part s
                              :start2 old-pos
                              :test test)
            do (write-string s out
                             :start old-pos
                             :end (or pos (length s)))
            when pos do (write-string replacement out)
            while pos))) 

(defun remove-redundant (replacements line)
  (loop for replacement in replacements
    do (let ((a (elt replacement 0))
             (b (elt replacement 1)))
         (setf line (replace-all line a b))))
  (return-from remove-redundant line)
  )

(let ((replacements (mapcar 'split (get-file "replacements.txt")))
      )
  (let ((new (mapcar (lambda (line) (remove-redundant replacements line)) (get-file "test.txt"))))
    (format t "~a~%" new)
    (write-file "output.txt" new)
    )
  ;; (format t "~a~%" (mapcar (lambda (line) (remove-redundant replacements line)) (get-file "test.txt")))
  )

