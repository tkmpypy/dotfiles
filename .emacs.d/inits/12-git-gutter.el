;; (use-package git-gutter
;;     :custom
;;     (git-gutter:modified-sign "~")
;;     (git-gutter:added-sign    "+")
;;     (git-gutter:deleted-sign  "-")
;;     :custom-face
;;     (git-gutter:modified ((t (:background "#f1fa8c"))))
;;     (git-gutter:added    ((t (:background "#50fa7b"))))
;;     (git-gutter:deleted  ((t (:background "#ff79c6"))))
;;     :config
;;     (global-git-gutter-mode t))
(use-package git-gutter-fringe+
  :config
  (global-git-gutter+-mode)
  (set-face-foreground 'git-gutter+-modified "#f1fa8c")
  (set-face-foreground 'git-gutter+-added    "#50fa7b")
  (set-face-foreground 'git-gutter+-deleted  "#ff79c6"))
