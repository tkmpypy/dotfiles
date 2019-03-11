(use-package vue-mode
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq mmm-submode-decoration-level 0))
