(use-package evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(use-package evil
  :custom
  (evil-cross-lines t)
  (evil-want-C-u-scroll t))
(evil-mode t)
(use-package evil-escape)
(evil-escape-mode)
