(map! :after python
      :localleader
      :map python-mode-map
      "t x" #'python-pytest-last-failed
      "t s" #'python-pytest-file)
(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

(defadvice! ml/pytest-root ()
  :override #'python-pytest--project-root
  (locate-dominating-file default-directory "pytest.ini"))
