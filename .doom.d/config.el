;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Global configuration
(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se")

;; Doom
(setq doom-font (font-spec :family "Source Code Pro" :size 16.0))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 28.0))
(setq doom-theme 'doom-gruvbox)

(setq org-directory "~/Documents/org/")
(setq display-line-numbers-type 'relative)

;; Don't scroll to the edge of the window
(setq scroll-margin 3)
(setq scroll-conservatively 3)

;; Treemacs
(setq create-lockfiles nil)
(setq treemacs-indentation 2)
(setq treemacs-width 25)
(setq treemacs-show-cursor nil)
(treemacs-resize-icons 16)
(add-hook 'treemacs-mode-hook (lambda () (treemacs-fringe-indicator-mode -1)))
(after! treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

;; BM
(global-set-key (kbd "<C-tab>") 'bm-next)
(global-set-key (kbd "<C-escape>") 'bm-previous)
(global-set-key (kbd "C-=") 'bm-toggle)
(setq bm-cycle-all-buffers t)

;; Magit
(setq magit-prefer-push-default t)
(setq magit-revision-show-gravatars nil)
(add-hook 'magit-mode-hook 'emoji-cheat-sheet-plus-display-mode)
(setq-default git-magit-status-fullscreen t)
(after! magit
  (require 'forge)
  (transient-bind-q-to-quit)
  )

;; Company
(add-hook 'company-mode-hook
          (lambda()
            (local-set-key (kbd "<right>") 'company-complete)))
(setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
(after! company
  (setq company-idle-delay 0
        company-show-numbers t))
(after! company
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

;;; Global keybindings
(evil-define-key 'normal with-editor-mode-map
  ",k" 'with-editor-cancel
  ",c" 'with-editor-finish
  ",," 'with-editor-finish)


;;; Language specific
;; Python
(after! flycheck
  (setq flycheck-python-mypy-args "--ignore-missing-imports")
  (flycheck-add-next-checker 'python-flake8 'python-mypy t))
;; Make sure pipenv correctly activates
(add-hook 'projectile-after-switch-project-hook (lambda ()
                                                  (pipenv-deactivate)
                                                  (cd (projectile-project-root))
                                                  (when (pipenv-project?)
                                                    (pipenv-activate))))
;; Javascript
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;;; System specific
;; MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq shell-file-name "/usr/local/bin/bash")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))
