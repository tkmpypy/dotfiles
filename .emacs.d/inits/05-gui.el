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
(setq default-frame-alist
      (append (list
              '(font . "Cica-18"))
              default-frame-alist))

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

;; 非アクティブウインドウが暗くなる
;;(use-package dimmer
;;  :ensure t
;;  :init (dimmer-mode)
;;  )

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package eyebrowse
 :config
  (setq eyebrowse-mode-line-separator " "
                 eyebrowse-new-workspace t)

  (eyebrowse-mode t))
