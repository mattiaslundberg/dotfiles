;;; custom/formatting/config.el -*- lexical-binding: t; -*-
(when (featurep! +onsave)
  (add-hook 'doom-first-file-hook #'apheleia-global-mode))

(after! apheleia
  (add-to-list 'apheleia-formatters '(mixformat . ("mix" "format" "-")))
  (add-to-list 'apheleia-mode-alist '(elixir-mode . mixformat)))

(defadvice! custom-apheleia-format-buffer (orig-fn command &optional callback)
  "Run formatter from project root, this is required to get mix format to find config files"
  :around #'apheleia-format-buffer
  ;; Hide lsp ui so it doesn't lock up
  (lsp-ui-sideline--delete-ov)
  (if (string-equal "mix" (car command))
    (let ((default-directory (locate-dominating-file ".formatter.exs" default-directory)))
      (funcall orig-fn command callback))
    (funcall orig-fn command callback)))
