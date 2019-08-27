(use-package evil-leader
  :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>"))

(use-package evil-escape)
(evil-escape-mode)

(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jj")
(setq evil-escape-unordered-key-sequence t)
(setq-default evil-escape-unordered-key-sequence t)
(setq-default evil-escape-excluded-major-modes '(evil-visual-state help-mode))
(push 'visual evil-escape-excluded-states)
(push 'normal evil-escape-excluded-states)

(use-package evil
  :custom
  (evil-cross-lines t)
  (evil-want-C-u-scroll t))
(evil-mode t)
