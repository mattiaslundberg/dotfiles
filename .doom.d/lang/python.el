(map! :after python
      :localleader
      :map python-mode-map
      "t x" #'python-pytest-last-failed
      "t s" #'python-pytest-file)
(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

(after! lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pyls-remote)))

(defadvice! ml/pytest-root ()
  :override #'python-pytest--project-root
  (locate-dominating-file default-directory "pytest.ini"))
