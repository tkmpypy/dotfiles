(use-package dap-mode
  :after (lsp)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (setq dap-print-io t)
  (require 'dap-python)

  (dap-register-debug-template "nlp"
    (list :type "python"
          :request "launch"
          :args "PYTHONPATH=/Users/takuma/work/unibo/migrate-db/deploy-new/nlp-server/nlpserver/src/main"
          :cwd "~/work/unibo/migrate-db/deploy-new/nlp-server/nlpserver/src/tests/chat"
          :name "Python :: nlp")))
