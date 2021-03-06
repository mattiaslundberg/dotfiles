;;; custom/eshell/config.el -*- lexical-binding: t; -*-

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
         ("mex" "iex -S mix $*")
         ("manage" "./manage.py $*")
         ("reload" "eshell-read-aliases-list"))))

;; Use emacs as editor
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

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

;; Fix mix completion
(defadvice! fix-fish-completion (raw-prompt)
  :override #'fish-completion-complete
  (while (pcomplete-here
          (let ((comp-list (fish-completion--list-completions raw-prompt)))
            (if (and comp-list (file-exists-p (car comp-list)) (not (s-starts-with? "mix" raw-prompt)) )
                ;; Completion result can be a filename.  pcomplete expects
                ;; cannonical file names (i.e. without '~') while fish preserves
                ;; non-cannonical results.  If the result contains a file, use
                ;; pcomplete completion instead of fish.
                (pcomplete-dirs-or-entries)
              ;; Remove trailing spaces to avoid it being converted into "\ ".
              (mapcar 'string-trim-right comp-list))))))
