(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(define-key evil-motion-state-map (kbd "gd") nil)

(general-define-key
 :keymaps '(normal emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "b" '(:ignore t :which-key "Buffer")
 "bd" 'kill-buffer
 "bq" 'kill-current-buffer
 "s" '(:ignore t :which-key "Search")
 "sb" 'ivy-switch-buffer
 "sr" 'counsel-recentf
 "sa" 'counsel-ag
 "sg" 'counsel-git-grep
 "sf" 'counsel-find-file
 "sg" 'counsel-git
 "ss" 'swiper
 "sc" 'swiper-thing-at-point
 "p" '(:ignore t :which-key "Workspace")
 "pc" 'persp-copy
 "pK" 'persp-kill
 "pd" 'persp-kill-buffer
 "pD" 'persp-remove-buffer
 "pn" 'persp-next
 "pp" 'persp-prev
 "p," 'persp-rename
 "pr" 'wg-relo
 "ps" 'persp-switch
 "pS" 'persp-window-switch
 "pw" 'persp-save-state-to-file
 "pW" 'persp-save-to-file-by-names
 "pl" 'persp-load-state-from-file
 "pL" 'persp-load-from-file-by-names
 "o" '(:ignore t :which-key "Org")
 "oc" 'org-capture
 "on" '(lambda() (interactive) (show-org-buffer "notes.org"))
 "ot" '(lambda() (interactive) (show-org-buffer "todo.org"))
 "f" '(:ignore t :which-key "File")
 "ft" 'neotree-projectile-toggle
 "e" '(:ignore t :which-key "Emacs")
 "eb" 'eval-buffer
 "g" '(:ignore t :which-key "Git")
 "gs" 'magit-status
 "w" '(:ignore t :which-key "Window")
 "ww" 'ace-select-window
 "wr" 'hydra-frame-window/body
 "r" 'ivy-resume
 "SPC" 'counsel-M-x
 )

;; LSP
(general-define-key
 :keymaps '(normal emacs)
 :prefix "SPC"
 :major-modes '(python-mode go-mode typescript-mode dart-mode)
 "m" '(:ignore t :which-key "Major")
 "mp" '(:ignore t :which-key "Peek")
 "mpg" 'lsp-ui-peek-find-definitions
 "mpr" 'lsp-ui-peek-find-references
 "mpi" 'lsp-ui-peek-find-implementation
 "mt" '(:ignore t :which-key "Toggle")
 "mtt" 'lsp-ui-imenu 
 "mts" 'lsp-ui-sideline-mode
 "mtl" 'lsp-lens-mode
 "mf" 'lsp-format-buffer
 )
;; LSP
(general-define-key
 :keymaps '(normal emacs)
 :major-modes '(python-mode go-mode typescript-mode dart-mode)
 "K" 'lsp-describe-thing-at-point
 )
