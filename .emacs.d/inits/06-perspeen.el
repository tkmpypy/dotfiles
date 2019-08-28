(use-package perspeen
  :init
  (setq perspeen-use-tab t)
  (setq perspeen-ws-list t)
  :config
  ;; doom-themeの色と合わせる
  (set-face-attribute 'perspeen-selected-face nil :background "#1e1e1e" :foreground "#51afef")
  (perspeen-mode))
