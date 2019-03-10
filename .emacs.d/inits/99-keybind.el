;; neotree
(evil-leader/set-key "ft" 'neotree-projectile-toggle)
;; ivy/counsel
(evil-leader/set-key "sw" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
(evil-leader/set-key "sf" 'counsel-find-file)
(evil-leader/set-key "<SPC>" 'counsel-M-x)
(evil-leader/set-key "sr" 'counsel-recentf)
(evil-leader/set-key "sa" 'counsel-ag)
(evil-leader/set-key "sb" 'ivy-switch-buffer)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jj")
