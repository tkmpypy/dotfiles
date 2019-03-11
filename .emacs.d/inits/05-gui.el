;; Display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Font
(set-default-font "Source Han Code JP M")

(use-package font-lock)
(use-package font-lock+)
(use-package all-the-icons)
 
;; flycheckとかでポップアップしてくれる
(use-package popup)

;; 非アクティブウインドウが暗くなる
;;(use-package dimmer
;;  :ensure t
;;  :init (dimmer-mode)
;;  )
