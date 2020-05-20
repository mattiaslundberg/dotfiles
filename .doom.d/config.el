;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Global configuration
(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se")

;; Doom
(setq doom-font (font-spec :family "Fira Code" :size 14.0))
(setq doom-big-font (font-spec :family "Fira Code" :size 28.0))
(setq doom-theme 'doom-one)
(setq doom-modeline-buffer-file-name-style 'buffer-name)

(setq org-directory "~/Documents/org/")
(setq display-line-numbers-type nil)

;; Pretty code
(setq +pretty-code-symbols '())

;; Don't scroll to the edge of the window
(setq scroll-margin 3)
(setq scroll-conservatively 3)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Treemacs
(setq +treemacs-git-mode 'deferred)
(setq treemacs-indentation 2)
(setq treemacs-width 25)
(setq treemacs-show-cursor nil)
(treemacs-resize-icons 16)
(add-hook 'treemacs-mode-hook (lambda () (treemacs-fringe-indicator-mode -1)))
(add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

;; etags
(setq tags-add-tables nil)

;; Magit
(after! magit
  (setq magit-prefer-push-default t)
  (setq magit-revision-show-gravatars nil))
(defadvice! fix-magit-revert-buffer (buffer)
  :override #'+magit--revert-buffer
  (with-current-buffer buffer
    (setq +magit--stale-p nil)
    (when buffer-file-name
      (revert-buffer t (not (buffer-modified-p))))))

;; Company
(add-hook 'company-mode-hook
          (lambda()
            (local-set-key (kbd "<right>") 'company-complete)))

(after! company-tng
  (define-key! company-active-map
    "TAB"       #'company-complete-selection
    [tab]       #'company-complete-selection
    [backtab]   nil))

(after! company
  (setq company-idle-delay 0.1))

;; Projectile
(setq projectile-project-search-path
      (append
       (doom-files-in "~/projects" :depth 0 :type 'dirs :full t)
       `("~/tmp")))

;;; Global keybindings
;; Magit
(evil-define-key 'normal with-editor-mode-map
  ",k" 'with-editor-cancel
  ",c" 'with-editor-finish
  ",," 'with-editor-finish)

(evil-define-key 'normal magit-status-mode-map
  "z" #'magit-stash)

(map! :leader
      (:desc "Blame" "g b" #'magit-blame-addition))

;; Lookup in dash
(map! :leader (:prefix-map ("d" . "custom")
               :desc "Dash lookup" "d" #'dash-at-point) )

;; Format current buffer
(map! :leader
      (:desc "Format buffer" "f ." #'format-all-buffer)
      (:desc "Toogle format-all-mode" "f ," #'format-all-mode))

;; Regenerate tags
(map! :leader
      (:desc "Regenerate tags" "p [" #'projectile-regenerate-tags))

;;; Language specific
;; Python
(after! flycheck
  (flycheck-add-next-checker 'python-flake8 'python-mypy t))

(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  python-flake8 python-pylint emacs-lisp-checkdoc
                  )))

;; Make sure pipenv correctly activates
(after! python
  (add-hook 'projectile-after-switch-project-hook (lambda ()
                                                    (pipenv-deactivate)
                                                    (cd (projectile-project-root))
                                                    (when (pipenv-project?)
                                                      (pipenv-activate)))))

;; Javascript
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; ReasonML
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook #'refmt-before-save)))

;; Fennel
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

;; Elixir
(map! :after alchemist
      :localleader
      :map alchemist-mode-map
      "t" #'alchemist-mix-test)

;;; System specific
;; MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq shell-file-name "/usr/local/bin/bash")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (setq ns-use-native-fullscreen t))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))
