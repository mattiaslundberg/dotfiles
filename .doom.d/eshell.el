;; Aliases
(setq +eshell-aliases
      (append
       +eshell-aliases
       '(
         ("mpip" "micropipenv $*")
         ("g" "git $*")
         ("dc" "docker-compose $*")
         ("d" "docker $*")
         ("vs" "vagrant ssh $*")
         ("reload" "eshell-read-aliases-list"))))

;; Use emacs as editor
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

(defun pr ()
  "Move to project root"
  (interactive)
  (cd (projectile-project-root)))

;; Prompt
(defvar-local eshell-current-command-start-time nil)

(defun eshell-current-command-start ()
  (setq eshell-current-command-start-time (current-time)))

(defun command-time ()
  (if eshell-current-command-start-time
      (let ((took (float-time
                   (time-subtract (current-time)
                                  eshell-current-command-start-time))))
        (setq eshell-current-command-start-time nil)
        (if (> took 3)
            (format-seconds "%H %M %z%.2ss " took)
          ""))
    ""))

(defun eshell-current-command-time-track ()
  (add-hook 'eshell-pre-command-hook #'eshell-current-command-start nil t))

(add-hook 'eshell-mode-hook #'eshell-current-command-time-track)

(defun eshell-prompt-fn ()
  (concat "\n"
          (propertize (command-time) 'face '((t)))
          (propertize (number-to-string eshell-last-command-status)
                      'face '((t)))
          " "
          (propertize (format-time-string "%H:%M:%S" (current-time))
                      'face '((t)))
          (if (bobp) " ")
          (+eshell-default-prompt-fn)))

(after! eshell
  (setq eshell-prompt-function #'eshell-prompt-fn))
