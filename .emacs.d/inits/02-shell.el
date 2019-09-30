(use-package multi-term)

(use-package vterm
  :when (and (executable-find "cmake")
             (executable-find "libtool"))
  :config
  ;; banish ansi-term :)
  (defalias 'ansi-term (lambda (&rest _) (call-interactively #'vterm)))
  (setq vterm-shell (executable-find "fish")))

(use-package vterm-toggle)
