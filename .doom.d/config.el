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

;; Modeline
(setq doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-buffer-encoding nil
      doom-modeline-buffer-modification-icon nil
      doom-modeline-env-version nil)

;; Don't scroll to the edge of the window
(setq scroll-margin 3)
(setq scroll-conservatively 3)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Format errors in popup
(set-popup-rule! "^\\*format-all-errors" :size 0.3 :ttl 0)
(setq +format-with-lsp t)

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode
            elixir-mode
            sql-mode
            tex-mode
            latex-mode
            sh-mode
            shell-mode))

(defun custom-format-enable-on-save-maybe-h ()
  "Enable formatting on save in certain major modes.

This is controlled by `+format-on-save-enabled-modes'."
  (unless (or (eq major-mode 'fundamental-mode)
              (cond ((booleanp +format-on-save-enabled-modes)
                     (null +format-on-save-enabled-modes))
                    ((eq (car +format-on-save-enabled-modes) 'not)
                     (memq major-mode (cdr +format-on-save-enabled-modes)))
                    ((not (memq major-mode +format-on-save-enabled-modes))))
              (not (require 'format-all nil t)))
    (format-all-mode +1)))

(add-hook 'after-change-major-mode-hook #'custom-format-enable-on-save-maybe-h)

(defun custom-format-elixir ()
  (if (eq major-mode 'elixir-mode)
      (lsp-format-buffer)))

(add-hook! 'before-save-hook
           #'custom-format-elixir)

;; LSP
(setq read-process-output-max (* 1024 1024) ;; 1mb
      lsp-file-watch-threshold 10000
      lsp-modeline-diagnostics-enable nil
      lsp-headerline-breadcrumb-enable nil)

(setq custom-lsp-file-watch-ignored-directories
      '(;; Python
        "[/\\\\]__pycache__$"
        "[/\\\\]\\.mypy_cache$"
        ;; Elixir
        "[/\\\\]\\.elixir_ls$"
        "[/\\\\]deps$"
        "[/\\\\]_build$"
        ;; Ruby
        "[/\\\\]vendor$"))

(defadvice! custom-lsp-ignored ()
  :override #'lsp-file-watch-ignored-directories
  (appendq! lsp-file-watch-ignored-directories custom-lsp-file-watch-ignored-directories))

;; Treemacs
(setq +treemacs-git-mode 'extended)
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
  (setq magit-prefer-push-default t
        magit-revision-show-gravatars nil
        magit-display-buffer-function 'magit-display-buffer-traditional))

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

;; IVY
(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^#\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;; Tramp
(setq enable-remote-dir-locals t)
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path "~/.asdf/shims")
  (add-to-list 'tramp-remote-path "~/.pyenv/shims"))

;;; Eshell
(load-file "~/.doom.d/eshell.el")

;;; Global keybindings
;; Magit
(map! :map with-editor-mode-map
      :n ",k" 'with-editor-cancel
      :n ",c" 'with-editor-finish
      :n ",," 'with-editor-finish)
(map! :map magit-status-mode-map
      :n "z" #'magit-stash)

(map! :leader
      (:desc "Blame" "g b" #'magit-blame-addition))

;; Counsel-tramp
(map! :leader
      (:desc "counsel-tramp" "s h" #'counsel-tramp))

;; Lookup in dash
(map! :leader (:prefix-map ("d" . "custom")
               :desc "Dash lookup" "d" #'dash-at-point))

;; Vagrant
(map! :leader (:prefix-map ("d v" . "Vagrant")
               (:desc "up" "u" #'vagrant-up)
               (:desc "ssh" "s" #'vagrant-ssh)
               (:desc "tramp-term" "S" #'vagrant-tramp-term)
               (:desc "halt" "x" #'vagrant-halt)
               (:desc "status" "?" #'vagrant-status)
               (:desc "edit" "e" #'vagrant-edit)
               (:desc "provision" "p" #'vagrant-provision)))
(set-popup-rule! "^\\*Vagrant" :size 0.3 :ttl 0)

;; Doom
(map! :leader
      (:desc "doom/upgrade" "h r u" #'doom/upgrade))

;; Kubernetes
(map! :leader
      (:desc "Kubernetes overview" "d k" #'kubernetes-overview))

;; Format current buffer
(map! :leader
      (:desc "Format buffer" "f ." #'format-all-buffer)
      (:desc "Toogle format-all-mode" "f ," #'format-all-mode))

;; Projectile
(map! :leader
      (:desc "Regenerate tags" "p [" #'projectile-regenerate-tags)
      (:desc "Add projects from path" "p A" #'projectile-discover-projects-in-search-path))

;;; Language specific
(let ((cfs (doom-files-in "~/.doom.d/lang")))
  (dolist (cf cfs)
    (load-file cf)))

;;; System specific
;; MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq shell-file-name "/usr/local/bin/bash")
  (setq projectile-tags-command "/usr/local/bin/ctags -R -e")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (setq ns-use-native-fullscreen t))
;; (set-frame-parameter nil 'fullscreen 'fullboth))

;; Linux
(when (string= system-type "gnu/linux")
  (setq shell-file-name "/bin/bash")
  (after! projectile
    (menu-bar-mode -99)))

(if (file-exists-p "/snap/bin/universal-ctags")
    (setq projectile-tags-command "/snap/bin/universal-ctags -R -e --exclude=.git --exclude=node_modules --exclude=elm-stuff --exclude=_build --exclude=deps --exclude=dist --exclude=bundles --exclude=collected --exclude=js_bundles --exclude=transpiled_js --exclude=__pycache__ --exclude=bundles --exclude=.cov --exclude=.cache --exclude=.terraform --exclude=.mypy_cache"))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))
