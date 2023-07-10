(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

(defadvice! ml/pytest-root ()
  :override #'python-pytest--project-root
  (locate-dominating-file default-directory "pyproject.toml"))

(fset 'extract-python-string
   (kmacro-lambda-form [?v ?a ?\" ?S ?f ?_ return escape] 0 "%d"))

(map! :after python
      :localleader
      :map python-mode-map
      "t x" #'python-pytest-last-failed
      "t s" #'python-pytest-file
      (:desc "Extract string for translation" "e" #'extract-python-string))
