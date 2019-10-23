(use-package python-mode)

(use-package lsp-python-ms
  :after python-mode
  :config
  (setq lsp-python-ms-extra-paths '("/usr/local/bin" "/Users/takuma/unibo/nlp-server/nlpserver/src/main")))
