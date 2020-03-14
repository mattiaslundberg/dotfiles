;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 16.0))
(set-face-attribute 'mode-line nil :family "Source Code Pro" :height 130)
(set-face-attribute 'mode-line-inactive nil :family "Source Code Pro" :height 130)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
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

"Configure js2 mode"
(setq javascript-fmt-tool 'prettier)
(with-eval-after-load 'js2-jsx-mode
  '(progn
      (add-hook 'js2-mode-hook #'prettier-js-mode)))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq javascript-fmt-on-save nil)


"Complete using C-<right> (system remapping to right arrow)"
(add-hook 'company-mode-hook
          (lambda()
            (local-set-key (kbd "<right>") 'company-complete)))

"Activate autocompletion"
(global-company-mode)
(require 'company-tabnine)
(setq company-idle-delay 0)

"MacOS fixes"
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

"Autoformat on save"
(setq elm-format-on-save t)
(setq rust-format-on-save t)
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(setq typescript-fmt-on-save nil)
(setq go-format-before-save t)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(setq create-lockfiles nil)
(setq treemacs-indentation 2)
(setq treemacs-width 25)
(setq treemacs-show-cursor nil)
(treemacs-resize-icons 16)
(add-hook 'treemacs-mode-hook (lambda () (treemacs-fringe-indicator-mode -1)))
(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

"Fix Ctrl-w"
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  )

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))

(setq projectile-tags-command "/usr/local/bin/ctags -R -e")
;; (setq projectile-tags-command "/snap/bin/universal-ctags -R -e --exclude=dist --exclude=node_modules --exclude=.mypy_cache --exclude=.git --exclude=images")

(if (file-exists-p "~/.spacemacs.local") (load-file "~/.spacemacs.local"))

(setq pipenv-with-projectile t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
