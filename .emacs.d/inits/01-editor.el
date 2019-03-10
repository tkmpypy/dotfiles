(setq backup-inhibited t)
(setq ring-bell-function 'ignore)
(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)) ; column
(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))
(use-package paren
    :hook
    (after-init . show-paren-mode)
    :custom-face
    (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
    :custom
    (show-paren-style 'mixed)
    (show-paren-when-point-inside-paren t)
    (show-paren-when-point-in-periphery t))
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))
;; 行番号を表示する
(global-linum-mode t)

;; 現在の行をハイライトする
(global-hl-line-mode 1)

;; 対応するカッコをハイライトする
(show-paren-mode 1)
;;; モードラインに時間を表示する
(display-time)
;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
