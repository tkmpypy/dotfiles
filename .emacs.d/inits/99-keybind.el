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

;; evilに割り当てられているキーバインドを無効化
(define-key evil-motion-state-map (kbd "gd") nil)

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "a" '(:ignore t :which-key "Application")
 "as" '(:ignore t :which-key "Shell")
 "asa" 'ansi-term
 "ase" 'eshell
 "asv" 'vterm
 "ast" 'vterm-toggle
 "b" '(:ignore t :which-key "Buffer")
 "bd" 'kill-buffer
 "bq" 'kill-current-buffer
 "s" '(:ignore t :which-key "Search")
 "sb" 'ivy-switch-buffer
 "ss" 'swiper
 "sc" 'swiper-thing-at-point
 "se" 'engine-mode-prefixed-map
 "sr" 'vr/replace
 "sf" '(:ignore t :which-key "File")
 "sfr" 'counsel-recentf
 "sff" 'counsel-find-file
 "sfg" 'counsel-git
 "sg" '(:ignore t :which-key "Grep")
 "sgA" 'counsel-ag
 "sgR" 'counsel-rg
 "sgg" 'counsel-git-grep
 "sgr" '(:ignore t :which-key "Ripgrep")
 "sgrd" 'deadgrep
 "sgrp" 'projectile-ripgrep
 "t" '(:ignore t :which-key "Toggle")
 "ti" 'highlight-indent-guides-mode
 "p" '(:ignore t :which-key "Workspace")
 "pc" 'persp-copy
 "pK" 'persp-kill
 "pd" 'persp-kill-buffer
 "pD" 'persp-remove-buffer
 "pn" 'persp-next
 "pp" 'persp-prev
 "p," 'persp-rename
 "ps" 'persp-switch
 "pS" 'persp-window-switch
 "pw" 'persp-save-state-to-file
 "pW" 'persp-save-to-file-by-names
 "pl" 'persp-load-state-from-file
 "pL" 'persp-load-from-file-by-names
 "o" '(:ignore t :which-key "Org")
 "oc" 'org-capture
 "oo" '(:ignore t :which-key "Open")
 "oon" '((lambda() (interactive) (show-org-buffer "notes.org")) :which-key "notes")
 "oot" '((lambda() (interactive) (show-org-buffer "todo.org")) :which-key "tasks")
 "f" '(:ignore t :which-key "File")
;; "ft" 'neotree-projectile-toggle
 "ft" 'treemacs
 "ff" 'treemacs-switch-workspace
 "fe" 'treemacs-edit-workspaces
 "e" '(:ignore t :which-key "Emacs")
 "eb" 'eval-buffer
 "g" '(:ignore t :which-key "Git")
 "gs" 'magit-status
 "gd" 'magit-diff-buffer-file
 "gc" 'magit-branch-checkout
 "gr" 'vc-refresh-state
 "w" '(:ignore t :which-key "Window")
 "ww" 'ace-select-window
 "wr" 'hydra-frame-window/body
 "r" 'ivy-resume
 "SPC" 'counsel-M-x
 )

;; LSP
(general-define-key
 :states '(normal visual)
 :keymaps '(lsp-mode-map)
 :prefix "SPC"
 "m" '(:ignore t :which-key "Major")
 "mr" 'lsp-rename
 "mp" '(:ignore t :which-key "Peek")
 "mpg" 'lsp-ui-peek-find-definitions
 "mpr" 'lsp-ui-peek-find-references
 "mpi" 'lsp-ui-peek-find-implementation
 "mt" '(:ignore t :which-key "Toggle")
 "mtt" 'lsp-ui-imenu 
 "mts" 'lsp-ui-sideline-mode
 "mtd" 'tkmpypy/toggle-lsp-ui-doc
 "mtl" 'lsp-lens-mode
 "mf" 'lsp-format-buffer
 )
;; LSP
(general-define-key
 :states '(normal visual)
 :keymaps '(lsp-mode-map)
 "K" 'tkmpypy/toggle-lsp-ui-doc
 "gr" 'lsp-ui-peek-find-references
 "gd" 'lsp-ui-peek-find-definitions
 "gi" 'lsp-ui-peek-find-implementation
 )

;; Prog
(general-define-key
 :states '(normal visual)
 :keymaps '(prog-mode-map)
 "gc" 'evilnc-comment-or-uncomment-lines)

(general-define-key
 :states '(normal visual)
 :keymaps '(prog-mode-map)
 :prefix "SPC"
 "j" '(:ignore t :which-key "Goto")
 "jj" 'dumb-jump-go
 "jk" 'dumb-jump-back
 "jJ" 'dumb-jump-go-other-window
 "jK" 'dumb-jump-quick-look)

;; Execute
;; flutter
(general-define-key
 :states '(normal)
 :keymaps '(dart-mode-map)
 :prefix "SPC"
 "e" '(:ignore t :which-key "Run")
 "er" 'flutter-run-or-hot-reload
 "eR" 'flutter-hot-restart
 "eq" 'flutter-quit
 )
;; Rust
(general-define-key
 :states '(normal)
 :keymaps '(rust-mode-map)
 :prefix "SPC"
 "e" '(:ignore t :which-key "Run")
 "ec" 'rust-compile
 "er" 'rust-run)
;; Go
(general-define-key
 :states '(normal)
 :keymaps '(go-mode-map)
 :prefix "SPC"
 "e" '(:ignore t :which-key "Run")
 "er" 'go-run
 "et" '(:ignore t :which-key "Test")
 "ett" 'go-test-current-test
 "etf" 'go-test-current-file
 "etp" 'go-test-current-project)

;; Org
(general-define-key
 :states '(normal visual)
 :keymaps '(org-mode-map)
 :prefix "SPC"
 "o" '(:ignore t :which-key "Org")
 "oe" '(:ignore t :which-key "Export")
 "oem" '(:ignore t :which-key "Markdown")
 "oemb" 'org-md-export-as-markdown
 "oemf" 'org-md-export-as-markdown
 "oa" '(:ignore t :which-key "Archive")
 "oaa" 'org-archive
 "oas" 'org-archive-subtree
 "os" '(:ignore t :which-key "Schedule")
 "oss" 'org-schedule
 "osd" 'org-deadline
 "oc" '(:ignore t :which-key "Clock")
 "oci" 'org-clock-in
 "oco" 'org-clock-out
 "op" '(:ignore t :which-key "Property")
 "opp" 'org-priority
 "opt" 'org-set-tags-command
 "ot" 'org-todo
 "oA" 'org-agenda
 )

;; eww
(general-define-key
 :states '(normal visual)
 :keymaps '(eww-mode-map)
 "C-p" 'eww-back-url)

;; term
(evil-define-key 'normal term-raw-map
  "p" 'term-paste)
(general-define-key
 :states '(normal)
 :keymaps '(vterm-mode-map)
 "p" 'term-paste
 )

;; twit
(general-define-key
 :states '(normal)
 :keymaps '(twittering-mode-map)
 "i" 'twittering-update-status-interactive)
