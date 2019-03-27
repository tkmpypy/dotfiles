;; Display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Font
(set-default-font "Source Han Code JP")

(use-package font-lock)
(use-package font-lock+)
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

;; 非アクティブウインドウが暗くなる
;;(use-package dimmer
;;  :ensure t
;;  :init (dimmer-mode)
;;  )

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))
