(defun ml/kotlin-test ()
  (interactive)
  (let ((default-directory (locate-dominating-file buffer-file-name "gradlew"))
        (compilation-read-command nil)
        (compile-command "sh gradlew clean test"))
    (call-interactively #'compile)))

(map! :after kotlin-mode
      :localleader
      :map kotlin-mode-map
      (:desc "Run all tests" "t a" #'ml/kotlin-test))
