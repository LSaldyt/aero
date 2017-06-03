(load "contex/util.lisp")
(load "contex/edit.lisp")

(defparameter *special-fields* (list "Title:" "Author:" "Date:"))

(defun to-placeholder (field)
  (format nil "__~a__" (string-upcase (string-right-trim ":" field))))

(defun to-tag-table (tags)
  (let ((tag-table (make-hash-table)))
    (loop for (tag field) in (zip tags *special-fields*)
          do (set-hash (to-placeholder field) (replace-all tag field "") tag-table))
    tag-table))

(defun special-field (line)
  (return-from special-field (not (any-in line *special-fields*))))

(defun split-body (filename)
  (let ((filelines (get-file filename)))
    (filter-list-in-two 'special-field filelines)))

(defun format-with (body tags templatelines)
  (let ((tag-table (to-tag-table tags)))
  (set-hash "__BODY__" (join body) tag-table)
  (loop for tline in templatelines
    do (with-hash-table-iterator (it tag-table)
      (loop
        (multiple-value-bind (entry-p key value) (it)
          (if entry-p
            (setf tline (replace-all tline key value))
            (return)))))
    collect tline)))

(defun create-paper (filename templatefile outfile)
  (let ((templatelines (get-file templatefile))
        (replacements (build-replacements (get-file "data/replacements.txt"))))
    (multiple-value-bind (body tags)
      (split-body filename)
      (write-file outfile (edit-lines replacements (format-with body tags templatelines))))))

(create-paper "data/example.ctex" "templates/template.tex" "output/output.tex")
