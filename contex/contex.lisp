(load "contex/util.lisp")

(defun special-field (line)
  (return-from special-field (not (any-in line (list "Title:" "Author:" "Date:")))))

(defun split-body (filename)
  (let ((filelines (get-file filename)))
    (filter-list-in-two 'special-field filelines)))

(defun format-with (body tags templatelines)
  (loop for tline in templatelines
        if (search "__BODY__" tline) 
        append body
        else
        collect tline))

(defun create-paper (filename templatefile outfile)
  (let ((templatelines (get-file templatefile)))
    (multiple-value-bind (body tags)
      (split-body filename)
      (write-file outfile (format-with body tags templatelines)))))

(create-paper "data/example.ctex" "templates/template.tex" "output.tex")
