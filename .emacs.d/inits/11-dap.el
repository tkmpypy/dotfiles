(use-package dap-mode
  :after (lsp)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (setq dap-print-io t)
  (require 'dap-python)

