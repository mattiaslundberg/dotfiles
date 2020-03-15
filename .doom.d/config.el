;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se")

(setq doom-font (font-spec :family "Source Code Pro" :size 16.0))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 28.0))
(setq doom-theme 'doom-gruvbox)

(setq org-directory "~/Documents/org/")

(setq display-line-numbers-type 'relative)

"Setup magit"
(setq magit-prefer-push-default t)
(setq magit-revision-show-gravatars nil)
;; (add-hook 'magit-mode-hook 'emoji-cheat-sheet-plus-display-mode)
(setq-default git-magit-status-fullscreen t)
(with-eval-after-load 'magit
  (require 'forge)
  (transient-bind-q-to-quit)
  )

(evil-define-key 'normal with-editor-mode-map
  ",k" 'with-editor-cancel
  ",c" 'with-editor-finish
  ",," 'with-editor-finish)

"Configure js2 mode"
(with-eval-after-load 'js2-jsx-mode
  '(progn
      (add-hook 'js2-mode-hook #'prettier-js-mode)))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

"Complete using C-<right> (system remapping to right arrow)"
(add-hook 'company-mode-hook
          (lambda()
            (local-set-key (kbd "<right>") 'company-complete)))

;; Configure autocompletion
(global-company-mode)
(require 'company-tabnine)
(setq company-idle-delay 0)
(setq company-backends
      '((company-files
         company-keywords
         company-tabnine)
        ))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (set-company-backend! 'python-mode '(company-tabnine)))

;; MacOS specific fixes
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq create-lockfiles nil)
(setq treemacs-indentation 2)
(setq treemacs-width 25)
(setq treemacs-show-cursor nil)
(treemacs-resize-icons 16)
(add-hook 'treemacs-mode-hook (lambda () (treemacs-fringe-indicator-mode -1)))
(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

;; Make sure pipenv correctly activates
(add-hook 'projectile-after-switch-project-hook (lambda ()
                                     (pipenv-deactivate)
                                     (cd (projectile-project-root))
                                     (pipenv-activate)
                                     ))

(if (file-exists-p "~/.spacemacs.local") (load-file "~/.spacemacs.local"))
