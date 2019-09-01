(use-package eglot
  :hook
  (dart-mode . eglot-ensure))
;; company-mode
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t))
