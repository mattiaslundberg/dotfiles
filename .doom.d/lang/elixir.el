(map! :after alchemist
      :localleader
      :map alchemist-mode-map
      "t a" #'alchemist-mix-test
      "t s" #'alchemist-mix-test-stale
      "b a" #'alchemist-mix
      "b r" #'alchemist-iex-run
      "b p" #'alchemist-iex-project-run
      "b l" #'alchemist-mix-rerun-last-task)
(set-popup-rule! "^\\*Alchemist" :size 0.3 :ttl 0)
(set-popup-rule! "^\\*alchemist" :size 0.3 :ttl 0)

(setq flycheck-elixir-credo-strict t
      alchemist-test-ask-about-save  nil
      alchemist-test-status-modeline nil)

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
