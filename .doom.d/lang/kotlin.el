(setq lsp-clients-kotlin-server-executable "~/.kotlin-language-server/server/build/install/server/bin/kotlin-language-server")


(defun ml/kotlin-test ()
  (interactive)
  (let ((default-directory (+kotlin-locate-gradlew-file))
        (compilation-read-command nil)
        (compile-command "sh gradlew clean test"))
    (call-interactively #'compile)))

(map! :after kotlin-mode
      :localleader
      :map kotlin-mode-map
      (:desc "Run all tests" "t a" #'ml/kotlin-test))
