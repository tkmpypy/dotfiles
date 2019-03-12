(use-package vue-mode
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (add-hook 'vue-mode-hook 'display-line-numbers-mode)
  (setq mmm-submode-decoration-level 0))
