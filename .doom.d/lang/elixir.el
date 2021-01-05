(map! :after elixir
      :localleader
      :map elixir-mode-map
      :prefix "t"
      "A" #'exunit-verify-all-in-umbrella)

(set-popup-rule! "^\\*exunit" :size 0.3 :ttl 0)

(setq flycheck-elixir-credo-strict t)

(add-hook! 'elixir-mode-hook
  (set (make-local-variable 'prettify-symbols-alist)
       '(("fn" . ?λ)
         ("and" . ?∧)
         ("or" . ?∨))
       ))

(after! lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "language_server.sh")
                     :major-modes '(elixir-mode)
                     :remote? t
                     :server-id 'elixir-ls-remote)))
