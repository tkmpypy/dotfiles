(use-package posframe)

(use-package counsel
  :custom
    (ivy-mode 1)
    (counsel-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :custom
    (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-window-center)
        (counsel-find-file     . ivy-posframe-display-at-window-center)
        (counsel-git     . ivy-posframe-display-at-window-center)
        (counsel-ag     . ivy-posframe-display-at-window-center)
        (counsel-rg     . ivy-posframe-display-at-window-center)
        (ivy-switch-buffer     . ivy-posframe-display-at-window-center)
        (counsel-recentf     . ivy-posframe-display-at-window-center)
        (persp-switch     . ivy-posframe-display-at-window-center)
        (persp-window-switch     . ivy-posframe-display-at-window-center)
	))
  :custom
  (ivy-posframe-parameters
      '((left-fringe . 5)
        (right-fringe . 5)))
  (ivy-posframe-mode 1)
  :custom-face
  (ivy-posframe ((t (:background "#282a36"))))
    (ivy-posframe-border ((t (:background "#6272a4"))))
    (ivy-posframe-cursor ((t (:background "#61bfff")))))

(use-package prescient)
(use-package ivy-prescient
  :after prescient
  :config (ivy-prescient-mode))
(use-package company-prescient
  :after (prescient company)
  :config (company-prescient-mode))
