(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
         while line
         collect line)))

(defun write-file (filename lines)
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create )
        (dolist (line lines)
            (format out "~a~%" line))))

(defun show (s)
    (format t "~a~%" s))

(defun to-str (c)
  (format nil "~a" c))

(defun split (line &optional (delim #\,) (keep nil))
    "Split key value pair by seperating with `delim`"
    (let ((index (position delim line)))
        (cond ((not index) (list line))
            (t (append (list (string-trim " " (subseq line 0 index)))
                       (list (to-str delim))
                       (split (string-trim " " (subseq line (+ 1 index))) delim keep))))))

(defun join (items &optional (with (to-str #\linefeed)))
  (reduce (lambda (a b) (concatenate 'string a with b)) items))

(defun split-with (line delimiters &optional (keep nil))
    (cond ((not delimiters) (list line))
        (t (loop for inner in (split line (car delimiters) keep) 
            append (split-with inner (cdr delimiters) keep)))))

(defun replace-all (s part replacement &key (test #'char-equal))
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

(defun any-in (s substrings)
  (loop for substr in substrings
        when (search substr s) do (return-from any-in t))
  (return-from any-in nil))

(defun filter-list-in-two (predicate l)
  (loop for x in l
    if (funcall predicate x) collect x into yes
    else collect x into no
    finally (return (values yes no))))

(defun set-hash (k v table)
    (setf (gethash k table) v))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))
