(use-package eglot
  :after flymake
  :hook
  (dart-mode . eglot-ensure))
;; company-mode
(use-package company
    :init
    (add-hook 'company-mode-hook
	      (lambda ()
		(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
		(define-key company-active-map (kbd "C-p") 'company-select-previous)
		(define-key company-search-map (kbd "C-n") 'company-select-next)
		(define-key company-search-map (kbd "C-p") 'company-select-previous)))
    :config
    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
    (setq company-idle-delay 0) ; デフォルトは0.5
    (setq company-minimum-prefix-length 1) ; デフォルトは4
    (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase nil)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (setq company-show-numbers t)
    (global-company-mode))

(use-package company-box
    :after (company all-the-icons)
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-icons-alist 'company-box-icons-all-the-icons))
(use-package company-quickhelp)
;; (use-package company-tabnine
;;     :after (company)
;;     :config
;;     (push 'company-tabnine company-backends))
