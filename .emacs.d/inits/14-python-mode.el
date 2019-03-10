(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode))

