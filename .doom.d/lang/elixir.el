(map! :after elixir-mode
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

(setq lsp-clients-elixir-server-executable "~/.elixir-ls/bin/language_server.sh")

;; Support Phoenix Liveview inline html
(define-hostmode poly-elixir-hostmode :mode 'elixir-mode)

(define-innermode poly-liveview-expr-elixir-innermode
  :mode 'web-mode
  :head-matcher (rx line-start (* space) (or "~E" "~L" "~H") (= 3 (char "\"'")) line-end)
  :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
  :head-mode 'host
  :tail-mode 'host
  :allow-nested nil
  :keep-in-mode 'host
  :fallback-mode 'host)

(define-polymode poly-elixir-web-mode
  :hostmode 'poly-elixir-hostmode
  :innermodes '(poly-liveview-expr-elixir-innermode))

(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))

(add-to-list 'auto-mode-alist '("\\.ex" . poly-elixir-web-mode))
