(setq backup-inhibited t)
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;; バックアップファイルを作成させない
(setq make-backup-files nil)
;; 1行ごとのスクロール
(setq scroll-conservatively 35
  scroll-margin 0
  scroll-step 1)
;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)
;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)
(setq ring-bell-function 'ignore)
(use-package highlight-indentation
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indentation-current-column-mode))

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
(use-package display-line-numbers
  :hook
  ((prog-mode yaml-mode systemd-mode html-mode) . display-line-numbers-mode))

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq completion-ignore-case t)
(global-auto-revert-mode 1)
(electric-pair-mode 1)

(use-package exec-path-from-shell)
