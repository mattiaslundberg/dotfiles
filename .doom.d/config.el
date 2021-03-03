;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Global configuration
(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se")

;; Look and feel
(setq doom-font (font-spec :family "Fira Code" :size 14.0))
(setq doom-big-font (font-spec :family "Fira Code" :size 28.0))
(setq doom-theme 'doom-one-light)

(custom-theme-set-faces! 'doom-one-light
 '(elixir-atom-face :foreground "SkyBlue4"))

(setq org-directory "~/Documents/org/")
(setq display-line-numbers-type nil)

(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 50))

;; Treesitter
(when (string-equal system-configuration "x86_64-apple-darwin20.3.0")
  (add-hook! 'doom-first-file-hook #'global-tree-sitter-mode))

;; Modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-buffer-encoding nil
      doom-modeline-buffer-modification-icon nil
      doom-modeline-env-version nil)

;; Don't scroll to the edge of the window
(setq scroll-margin 3)
(setq scroll-conservatively 3)

;; Doom config
(defun ml/ediff-dotfile-and-template ()
  (interactive)
  (ediff-files
    "~/.doom.d/init.el"
    "~/.emacs.d/init.example.el"))

(map! :leader
      (:desc "doom/upgrade" "h r u" #'doom/upgrade)
      (:desc "doom/diff-init" "h d i" #'ml/ediff-dotfile-and-template))

;; Disable lockfiles
(setq create-lockfiles nil)

;; LSP
(setq read-process-output-max (* 1024 1024) ;; 1mb
      lsp-file-watch-threshold 10000
      lsp-modeline-diagnostics-enable nil
      lsp-headerline-breadcrumb-enable nil
      lsp-modeline-workspace-status-enable nil
      lsp-enable-symbol-highlighting nil)

(setq custom-lsp-file-watch-ignored-directories
      '(;; Python
        "[/\\\\]__pycache__\\'"
        "[/\\\\]\\.mypy_cache\\'"
        ;; Elixir
        "[/\\\\]\\.elixir_ls\\'"
        "[/\\\\]deps\\'"
        "[/\\\\]_build\\'"
        ;; Docker
        "[/\\\\]\\.docker\\'"))

(defadvice! custom-lsp-ignored ()
  :override #'lsp-file-watch-ignored-directories
  (append lsp-file-watch-ignored-directories custom-lsp-file-watch-ignored-directories))

;; Treemacs
(setq +treemacs-git-mode 'extended
      treemacs-indentation 2
      treemacs-width 25
      treemacs-show-cursor nil)
(treemacs-resize-icons 16)
(add-hook 'treemacs-mode-hook (lambda ()
                                (treemacs-fringe-indicator-mode -1)
                                (treemacs-follow-mode)))
(add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

;; Ediff
(defun ml/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                     (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                     (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(add-hook 'ediff-keymap-setup-hook (lambda ()
  (if ediff-3way-job
    (define-key ediff-mode-map "x" #'ml/ediff-copy-both-to-C))))

;; Magit
(after! magit
  (setq magit-prefer-push-default t
        magit-revision-show-gravatars nil
        magit-display-buffer-function 'magit-display-buffer-traditional))

(map! :map with-editor-mode-map
      :n ",k" 'with-editor-cancel
      :n ",c" 'with-editor-finish
      :n ",," 'with-editor-finish)
(map! :map magit-status-mode-map
      :n "z" #'magit-stash)
(map! :map forge-post-mode-map
      :n ",k" 'forge-post-cancel
      :n ",c" 'forge-post-submit
      :n ",," 'forge-post-submit)

(map! :leader
      (:desc "Blame" "g b" #'magit-blame-addition))

;; Company
(after! company
  (setq company-idle-delay 0.1)
  (define-key! company-active-map
    "TAB"       #'company-complete-selection
    [tab]       #'company-complete-selection
    [backtab]   nil))

;; Projectile
(setq projectile-project-root-files '())
(setq projectile-project-search-path
      (doom-files-in "~/Development" :depth 0 :type 'dirs :full t))

(add-hook 'emacs-startup-hook (lambda ()
  (projectile-discover-projects-in-search-path)
  (projectile-add-known-project "~/.dotfiles")))

(map! :leader
      (:desc "Add projects from path" "p A" #'projectile-discover-projects-in-search-path))

;; IVY
(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^#\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(setq ivy-dispatching-done-hydra-exit-keys '(("C-[" nil)))

;; Tramp
(setq enable-remote-dir-locals t)
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path "~/.asdf/shims")
  (add-to-list 'tramp-remote-path "~/.pyenv/shims"))

(map! :leader
      (:desc "counsel-tramp" "s h" #'counsel-tramp))

;;; Global keybindings
(map! :leader (:prefix-map ("d" . "custom")
               :desc "Dash lookup" "d" #'dash-at-point))

(map! :m "C-]" #'+lookup/definition
      :m "M-[" #'previous-buffer
      :m "M-]" #'next-buffer)

;;; Language specific
(let ((cfs (doom-files-in "~/.doom.d/lang")))
  (dolist (cf cfs)
    (load-file cf)))

;;; System specific
;; MacOS
(when IS-MAC
  (if (file-exists-p "/opt/homebrew/bin/bash")
    (setq shell-file-name "/opt/homebrew/bin/bash"
          vterm-shell "/opt/homebrew/bin/zsh")
    (setq shell-file-name "/usr/local/bin/bash"
          vterm-shell "/usr/local/bin/zsh"))
  (setq dired-use-ls-dired nil
        mac-option-key-is-meta t
        mac-right-option-modifier nil
        ns-use-native-fullscreen t))
;; (set-frame-parameter nil 'fullscreen 'fullboth))

;; Linux
(when IS-LINUX
  (setq shell-file-name "/bin/bash")
  (after! projectile
    (menu-bar-mode -99)))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))
