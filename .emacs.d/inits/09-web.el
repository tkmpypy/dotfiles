(use-package web-mode
  :mode
  ("\\.html$" . web-mode)
  ("\\.hbs$" . web-mode)
  ("\\.tsx\\'" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (add-hook 'web-mode-hook
            (function (lambda ()
                        (setq evil-shift-width web-mode-markup-indent-offset)))))

(use-package css-mode
  :init
  (setq css-indent-offset 2))
