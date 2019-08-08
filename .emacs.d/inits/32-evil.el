(use-package evil-leader
  :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")

    ;; neotree
    (evil-leader/set-key "ft" 'neotree-projectile-toggle))
    ;; ivy/counsel
    (evil-leader/set-key "sw" 'swiper)
    (evil-leader/set-key "oc" 'org-capture)
    (evil-leader/set-key "oon" '(lambda() (interactive) (show-org-buffer "notes.org")))
    (evil-leader/set-key "oot" '(lambda() (interactive) (show-org-buffer "todo.org")))

    (evil-leader/set-key "sf" 'counsel-find-file)
    (evil-leader/set-key "<SPC>" 'counsel-M-x)
    (evil-leader/set-key "sr" 'counsel-recentf)
    (evil-leader/set-key "sa" 'counsel-ag)
    (evil-leader/set-key "sb" 'ivy-switch-buffer)
    ;; lsp python
    (evil-leader/set-key-for-mode 'python-mode "gg" 'lsp-ui-peek-find-definitions)
    (evil-leader/set-key-for-mode 'python-mode "gr" 'lsp-ui-peek-find-references)
    (evil-leader/set-key-for-mode 'python-mode "gi" 'lsp-ui-peek-find-implementation)
    (evil-leader/set-key-for-mode 'python-mode "tm" 'lsp-ui-imenu)
    (evil-leader/set-key-for-mode 'python-mode "ts" 'lsp-ui-sideline-mode)
    (evil-leader/set-key-for-mode 'python-mode "tl" 'lsp-lens-mode)
    (evil-leader/set-key-for-mode 'python-mode "dd" 'lsp-ui-doc-show)
    (evil-leader/set-key-for-mode 'python-mode "fb" 'lsp-format-buffer)
    ;; lsp go
    (evil-leader/set-key-for-mode 'go-mode "gg" 'lsp-ui-peek-find-definitions)
    (evil-leader/set-key-for-mode 'go-mode "gr" 'lsp-ui-peek-find-references)
    (evil-leader/set-key-for-mode 'go-mode "gi" 'lsp-ui-peek-find-implementation)
    (evil-leader/set-key-for-mode 'go-mode "tm" 'lsp-ui-imenu)
    (evil-leader/set-key-for-mode 'go-mode "ts" 'lsp-ui-sideline-mode)
    (evil-leader/set-key-for-mode 'go-mode "tl" 'lsp-lens-mode)
    (evil-leader/set-key-for-mode 'go-mode "dd" 'lsp-ui-doc-show)
    (evil-leader/set-key-for-mode 'go-mode "fb" 'lsp-format-buffer)
    ;; lsp vue
    (evil-leader/set-key-for-mode 'vue-mode "gg" 'lsp-ui-peek-find-definitions)
    (evil-leader/set-key-for-mode 'vue-mode "gr" 'lsp-ui-peek-find-references)
    (evil-leader/set-key-for-mode 'vue-mode "gi" 'lsp-ui-peek-find-implementation)
    (evil-leader/set-key-for-mode 'vue-mode "tm" 'lsp-ui-imenu)
    (evil-leader/set-key-for-mode 'vue-mode "ts" 'lsp-ui-sideline-mode)
    (evil-leader/set-key-for-mode 'vue-mode "tl" 'lsp-lens-mode)
    (evil-leader/set-key-for-mode 'vue-mode "dd" 'lsp-ui-doc-show)
    (evil-leader/set-key-for-mode 'vue-mode "fb" 'lsp-format-buffer)
    ;; lsp typescript
    (evil-leader/set-key-for-mode 'typescript-mode "gg" 'lsp-ui-peek-find-definitions)
    (evil-leader/set-key-for-mode 'typescript-mode "gr" 'lsp-ui-peek-find-references)
    (evil-leader/set-key-for-mode 'typescript-mode "gi" 'lsp-ui-peek-find-implementation)
    (evil-leader/set-key-for-mode 'typescript-mode "tm" 'lsp-ui-imenu)
    (evil-leader/set-key-for-mode 'typescript-mode "ts" 'lsp-ui-sideline-mode)
    (evil-leader/set-key-for-mode 'typescript-mode "tl" 'lsp-lens-mode)
    (evil-leader/set-key-for-mode 'typescript-mode "dd" 'lsp-ui-doc-show)
    (evil-leader/set-key-for-mode 'typescript-mode "fb" 'lsp-format-buffer)
)

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
