(use-package flycheck
  :hook (prog-mode . flycheck-mode))

 (use-package flycheck-posframe
   :after flycheck
   :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
