;; Display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-auto-hide-menu-bar t)
  (setq ns-use-proxy-icon nil)
  (setq initial-frame-alist
     (append
      '((ns-transparent-titlebar . t)
    (ns-appearance . dark)
    (vertical-scroll-bars . nil)
    (internal-border-width . 0)))))

;; Font
;;(setq default-frame-alist
;;      (append (list
;;              '(font . "Cica-18"))
;;              default-frame-alist))

(let* ((size 18)
       (asciifont "Ricty Diminished Discord")
       (jpfont "Ricty Diminished Discord")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; (use-package font-lock)
;; (use-package font-lock+)
(use-package all-the-icons)

;; 行番号を表示する
;; (global-linum-mode t)
;; (global-display-line-numbers-mode t)

;; 現在の行をハイライトする
(global-hl-line-mode 1)

;; 対応するカッコをハイライトする
(show-paren-mode 1)
;;; モードラインに時間を表示する
(display-time)

;; flycheckとかでポップアップしてくれる
(use-package popup)

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode) . hide-mode-line-mode))

(use-package eyebrowse
 :config
  (setq eyebrowse-mode-line-separator " "
                 eyebrowse-new-workspace t)

  (eyebrowse-mode t))

(use-package dimmer
  :disabled
  :custom
  (dimmer-fraction 0.5)
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*company.*"
         ".*child.*"
	 ".*.*"
         ".*lsp.*"
         ".*posframe.*"
         ".*ivy.*"
         ".*NeoTree.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*LV.*"
         ".*Ilist.*"))
  :config
  (dimmer-mode t))
