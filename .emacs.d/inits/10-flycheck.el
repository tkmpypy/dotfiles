(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (setq flycheck-idle-change-delay 4)
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
