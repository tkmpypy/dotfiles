(use-package spaceline
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (progn
    (spaceline-define-segment buffer-id
      (if (buffer-file-name)
          (let ((project-root (projectile-project-p)))
            (if project-root
                (file-relative-name (buffer-file-name) project-root)
              (abbreviate-file-name (buffer-file-name))))
        (powerline-buffer-id)))
    (spaceline-spacemacs-theme)
    (spaceline-toggle-minor-modes-off)))

;; (use-package spaceline-all-the-icons
;; 	:custom
;; 	(spaceline-all-the-icons--setup-neotree)
;; 	(spaceline-all-the-icons-theme)
;;         :config
;; 	(setq spaceline-highlight-face-func 'spaceline-highlight-face-default)
;;         (setq powerline-text-scale-factor 1.1)
;;         (setq powerline-default-separator 'bar)
;;         (setq spaceline-all-the-icons-icon-set-modified 'chain)
;;         (setq spaceline-all-the-icons-icon-set-window-numbering 'circle)
;;         (setq spaceline-all-the-icons-separator-type 'cup)
;;         (setq spaceline-all-the-icons-separators-type 'cup)
;;         (setq spaceline-all-the-icons-primary-separator "")
;; 
;; 	(spaceline-toggle-all-the-icons-buffer-size-off)
;; 	(spaceline-toggle-all-the-icons-buffer-position-off)
;; 	(spaceline-toggle-all-the-icons-vc-icon-off)
;; 	(spaceline-toggle-all-the-icons-vc-status-on)
;; 	(spaceline-toggle-all-the-icons-git-status-on)
;; 	(spaceline-toggle-all-the-icons-flycheck-status-off)
;; 	(spaceline-toggle-all-the-icons-time-on)
;; 	(spaceline-toggle-all-the-icons-battery-status-off)
;; 	(spaceline-toggle-hud-off))
