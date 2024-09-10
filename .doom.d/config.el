;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Global configuration
(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se"
      org-directory "~/Documents/org"
      display-line-numbers-type nil)

;; Theme
(setq doom-font (font-spec :family "Fira Code" :size 14.0 :weight 'medium))
(setq doom-big-font (font-spec :family "Fira Code" :size 20.0 :weight 'medium))
(setq ml/theme-light 'doom-one-light
      ml/theme-dark 'doom-one
      doom-theme ml/theme-light)

(custom-theme-set-faces! 'doom-one-light
  '(elixir-atom-face :foreground "SkyBlue4"))

;; Automatic theme switching on macos
(defun ml/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme ml/theme-light t))
    ('dark (load-theme ml/theme-dark t))))

(when IS-MAC
  (add-hook 'ns-system-appearance-change-functions #'ml/apply-theme))

;; Better startup sizing
(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 50))

;; Compilation
(setq compilation-scroll-output t)

;; Eglot
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
;; (setq eglot-withhold-process-id 1)
;; (set-eglot-client! 'rjsx-mode `("typescript-language-server" "--stdio"))
;; (set-eglot-client! 'rjsx-mode `("~/.bin/lsp-docker.sh" "typescript-language-server --stdio"))

;; Modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-buffer-encoding nil
      doom-modeline-buffer-modification-icon nil
      doom-modeline-env-version nil)

;; Local variables
(setq enable-local-variables t)
(advice-add 'risky-local-variable-p :override #'ignore)

;; Don't scroll to the edge of the window
(setq scroll-margin 3
      scroll-conservatively 3)

;; Doom config
(defun ml/ediff-dotfile-and-template ()
  (interactive)
  (ediff-files
   "~/.doom.d/init.el"
   "~/.emacs.d/templates/init.example.el"))

(map! :leader
      (:desc "doom/upgrade" "h r u" #'doom/upgrade)
      (:desc "doom/diff-init" "h d i" #'ml/ediff-dotfile-and-template))

;; Disable lockfiles
(setq create-lockfiles nil)

;; Formatting
(setq-default ml/format-on-save t)
(setq +format-with-lsp nil)
(setq apheleia-remote-algorithm 'local)

(defadvice! ml/apheleia-format-buffer (orig-fn command &optional callback)
  :around #'apheleia-format-buffer
  (when ml/format-on-save
    (funcall orig-fn command callback)))

(map! :leader
      (:desc "Format buffer" "f ." #'apheleia-format-buffer)
      (:desc "Toogle format on save" "f ," #'apheleia-mode))

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
        magit-display-buffer-function 'magit-display-buffer-traditional
        git-commit-summary-max-length 78))

(map! :map with-editor-mode-map
      :n ",k" 'with-editor-cancel
      :n ",c" 'with-editor-finish
      :n ",," 'with-editor-finish)
(map! :map magit-status-mode-map
      :n "z" #'magit-stash)

(map! :leader
      (:desc "Blame" "g b" #'magit-blame-addition))

(defadvice! ml/magit-clone-and-invalidate (&rest args)
  :after #'magit-clone
  (projectile-invalidate-cache nil)
  (projectile-discover-projects-in-search-path))

(defadvice! ml/magit-checkout-and-invalidate (&rest args)
  :after #'magit-checkout
  (projectile-invalidate-cache nil))

(defadvice! ml/magit-pull-and-invalidate (&rest args)
  :after #'magit-pull
  (projectile-invalidate-cache nil))

;; Corfu
(after! corfu
  (setq corfu-preselect 'first))

(advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
              (shut-up (apply orig args))))

;; Projectile
(defun ml/projectile-vc-root-dir (dir)
  "Retrieve the root directory of the project at DIR using `vc-root-dir'."
  (let ((default-directory dir))
    (locate-dominating-file "." ".git")))

(setq projectile-project-root-functions '(ml/projectile-vc-root-dir))
(setq projectile-project-search-path
      (doom-files-in "~/Development" :depth 0 :type 'dirs :full t))
(setq projectile-auto-discover t)

(add-hook 'emacs-startup-hook (lambda ()
                                (projectile-add-known-project "~/.dotfiles")))
(map! :leader
      (:desc "Add projects from path" "p A" #'projectile-discover-projects-in-search-path))

;; Tramp
(setq enable-remote-dir-locals t)
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path "~/.asdf/shims"))

;;; Global keybindings
(map! :m "C-]" #'+lookup/definition
      :m "M-[" #'previous-buffer
      :m "M-]" #'next-buffer)

;;; Language specific
(let ((cfs (doom-files-in "~/.doom.d/lang")))
  (dolist (cf cfs)
    (when (s-suffix? ".el" cf)
      (load-file cf))))

;;; System specific
;; MacOS
(when IS-MAC
  (setq shell-file-name "/opt/homebrew/bin/bash"
        dired-use-ls-dired nil
        mac-option-key-is-meta t
        mac-right-option-modifier nil
        company-shell-dont-fetch-meta t
        ns-use-native-fullscreen t))

;; Linux
(when IS-LINUX
  (setq shell-file-name "/bin/bash")
  (after! projectile
    (menu-bar-mode -99)))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))
