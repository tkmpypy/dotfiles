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
        (t               . ivy-posframe-display-at-window-center)))
  :custom
  (ivy-posframe-mode 1))
