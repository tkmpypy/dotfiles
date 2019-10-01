(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4))

(use-package gotest)
