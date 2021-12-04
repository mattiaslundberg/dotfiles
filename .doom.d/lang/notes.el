(defun ml/add-note ()
  (interactive)
  (let* ((content (read-string "Input note: ")))
    (f-append-text (format "- %s" content) 'utf-8 "~/notes/todo.md")))

(map! :leader
  (:desc "Add note" "n n" #'ml/add-note))
