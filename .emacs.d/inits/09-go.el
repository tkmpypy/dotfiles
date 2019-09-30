(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :defer t
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4))
