(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'python-mode-hook 'display-line-numbers-mode)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode))

