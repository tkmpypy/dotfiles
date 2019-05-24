(use-package python-mode
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode))

(use-package lsp-python-ms
  :after python-mode
  :config
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
          "~/takuma/work/python-language-server/output/bin/Debug/Microsoft.Python.LanguageServer"))