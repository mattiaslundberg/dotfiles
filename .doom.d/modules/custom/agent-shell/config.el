;;; custom/agent-shell/config.el -*- lexical-binding: t; -*-

(after! agent-shell
  (require 'agent-shell-sidebar nil t)
  (defun ml/agent-shell--sidebar-context ()
    (let* ((sidebar-window (selected-window))
           (sidebar-buffer (and (window-live-p sidebar-window)
                                (window-buffer sidebar-window)))
           (in-sidebar (and (window-live-p sidebar-window)
                            (window-parameter sidebar-window 'window-side)
                            (buffer-live-p sidebar-buffer)
                            (with-current-buffer sidebar-buffer
                              (bound-and-true-p agent-shell-sidebar--is-sidebar)))))
      (list :window sidebar-window :in-sidebar in-sidebar)))

  (defun ml/agent-shell--sidebar-compose-edit-p (context)
    (and (plist-get context :in-sidebar)
         (derived-mode-p 'agent-shell-viewport-edit-mode)))

  (defun ml/agent-shell--set-sidebar-window-buffer (sidebar-window buffer)
    (let ((was-dedicated (window-dedicated-p sidebar-window)))
      (set-window-dedicated-p sidebar-window nil)
      (set-window-buffer sidebar-window buffer)
      (set-window-dedicated-p sidebar-window was-dedicated)
      (with-current-buffer buffer
        (when (boundp 'agent-shell-sidebar--is-sidebar)
          (setq-local agent-shell-sidebar--is-sidebar t)))
      (select-window sidebar-window)))

  (defadvice! +agent-shell--display-compose-in-sidebar-window (orig buffer)
    :around #'agent-shell--display-buffer
    (let* ((context (ml/agent-shell--sidebar-context))
           (sidebar-window (plist-get context :window)))
      (if (and (plist-get context :in-sidebar)
               (buffer-live-p buffer)
               (with-current-buffer buffer
                 (or (derived-mode-p 'agent-shell-viewport-edit-mode)
                     (derived-mode-p 'agent-shell-viewport-view-mode))))
          (ml/agent-shell--set-sidebar-window-buffer sidebar-window buffer)
        (funcall orig buffer))))

  (defadvice! +agent-shell-viewport-compose-send-and-kill-in-sidebar (orig &rest args)
    :around #'agent-shell-viewport-compose-send-and-kill
    (let* ((context (ml/agent-shell--sidebar-context))
           (sidebar-window (plist-get context :window)))
      (if (not (ml/agent-shell--sidebar-compose-edit-p context))
          (apply orig args)
        (let* ((viewport-buffer (current-buffer))
               (shell-buffer (agent-shell-viewport--shell-buffer))
               (prompt (buffer-string)))
          (unless (buffer-live-p shell-buffer)
            (user-error "No shell available"))
          (with-current-buffer shell-buffer
            (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))
          (ml/agent-shell--set-sidebar-window-buffer sidebar-window shell-buffer)
          (kill-buffer viewport-buffer)))))

  (defadvice! +agent-shell-viewport-compose-cancel-in-sidebar (orig &rest args)
    :around #'agent-shell-viewport-compose-cancel
    (let* ((context (ml/agent-shell--sidebar-context))
           (sidebar-window (plist-get context :window)))
      (if (not (ml/agent-shell--sidebar-compose-edit-p context))
          (apply orig args)
        (let* ((viewport-buffer (current-buffer))
               (shell-buffer (agent-shell-viewport--shell-buffer)))
          (unless (buffer-live-p shell-buffer)
            (user-error "No shell available"))
          (when (or (string-empty-p (string-trim (buffer-string)))
                    (y-or-n-p "Discard composed prompt? "))
            (ml/agent-shell--set-sidebar-window-buffer sidebar-window shell-buffer)
            (kill-buffer viewport-buffer))))))

  (setq agent-shell-agent-configs
        (mapcar (lambda (config)
                  (cond ((eq (map-elt config :identifier) 'codex)
                         (let ((updated (copy-tree config)))
                           (map-put! updated :default-model-id (lambda () "gpt-5.3-codex/high"))
                           (map-put! updated :default-session-mode-id (lambda () "auto"))
                           updated))
                        ((eq (map-elt config :identifier) 'cursor)
                         (let ((updated (copy-tree config)))
                           (map-put! updated :default-model-id (lambda () "opus-4.6-thinking"))
                           (map-put! updated :default-session-mode-id (lambda () "agent"))
                           updated))
                        (t config)))
                agent-shell-agent-configs))

  (map! :map agent-shell-viewport-edit-mode-map
        :n ",k" #'agent-shell-viewport-compose-cancel
        :n ",c" #'agent-shell-viewport-compose-send
        :n ",," #'agent-shell-viewport-compose-send-and-kill)

  (map! :map agent-shell-mode-map
        :localleader
        (:prefix ("a" . "Agent")
         :desc "Agent compose" "a" #'agent-shell-prompt-compose
         :desc "Agent mode"    "m" #'agent-shell-set-session-mode
         :desc "Agent model"   "n" #'agent-shell-set-session-model)))

(defun ml/agent-shell--find-sidebar-window ()
  (catch 'window
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (and (window-live-p window)
                   (window-parameter window 'window-side)
                   (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (bound-and-true-p agent-shell-sidebar--is-sidebar)))
          (throw 'window window))))
    nil))

(defun ml/agent-shell-sidebar-toggle-or-focus ()
  (interactive)
  (let ((sidebar-window (ml/agent-shell--find-sidebar-window)))
    (if (and (window-live-p sidebar-window)
             (not (eq (selected-window) sidebar-window)))
        (select-window sidebar-window)
      (call-interactively #'agent-shell-sidebar-toggle))))

(map! :leader
      :desc "Agent shell" "\\" #'ml/agent-shell-sidebar-toggle-or-focus)
