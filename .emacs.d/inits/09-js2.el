(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init
  (setq js2-basic-offset 2)
  (setq js-switch-indent-offset 2)
  (add-hook 'js2-mode-hook
            (function (lambda ()
                        (setq evil-shift-width js2-basic-offset)))))
