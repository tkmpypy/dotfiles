(use-package evil
  :custom
  (evil-cross-lines t)
  (evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :custom
;;   (evil-collection-company-use-tng nil)
;;   :init
;;   (evil-collection-init))

(use-package evil-magit
  :config
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank t))

(use-package evil-escape
  :after evil
  :config
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jj")
  (setq evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-excluded-major-modes '(evil-visual-state help-mode))
  (push 'visual evil-escape-excluded-states)
  (push 'normal evil-escape-excluded-states)
  (evil-escape-mode))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))
