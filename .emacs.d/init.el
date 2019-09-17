(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageをインストールする
(straight-use-package 'use-package)

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq-default straight-use-package-by-default t)

;; init-loaderをインストール&読み込み
(use-package init-loader)

;; ~/.emacs.d/init/ 以下のファイルを全部読み込む
(init-loader-load)

;; Macでバックスラッシュの入力が出来るように
(define-key global-map [?\M-¥] "\\")

;; ウィンドウが上のほうにあれば縦に縮小し、下のほうにあれば縦に拡大する
(defun resize-window-up ()
  (interactive)
  (let* ((edges (window-edges (selected-window)))
         (end-y (cadddr edges)))
    (if (< end-y (frame-height))
        (shrink-window 1)
      (enlarge-window 1))))

;; ウィンドウが上のほうにあれば縦に拡大し、下のほうにあれば縦に縮小する
(defun resize-window-down ()
  (interactive)
  (let* ((edges (window-edges (selected-window)))
         (end-y (cadddr edges)))
    (if (< end-y (frame-height))
        (enlarge-window 1)
      (shrink-window 1))))

;; ウィンドウが左のほうにあれば横に拡大し、右のほうにあれば横に縮小する
(defun resize-window-right ()
  (interactive)
  (let* ((edges (window-edges (selected-window)))
         (end-x (caddr edges)))
    (if (< end-x (frame-width))
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1))))

;; ウィンドウが左のほうにあれば横に縮小し、右のほうにあれば横に拡大する
(defun resize-window-left ()
  (interactive)
  (let* ((edges (window-edges (selected-window)))
         (end-x (caddr edges)))
    (if (< end-x (frame-width))
        (shrink-window-horizontally 1)
      (enlarge-window-horizontally 1))))

(global-set-key (kbd "<M-up>") 'resize-window-up)
(global-set-key (kbd "<M-down>") 'resize-window-down)
(global-set-key (kbd "<M-right>") 'resize-window-right)
(global-set-key (kbd "<M-left>") 'resize-window-left)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(company-lsp-async t)
 '(company-lsp-cache-candidates t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 '(company-lsp-filter-candidates t)
 '(company-quickhelp-delay 0.8)
 '(counsel-mode 1)
 '(dashboard-center-content t)
 '(dashboard-items
   (quote
    ((recents . 15)
     (agenda . 5)
     (projects . 5)
     (bookmarks . 5))) t)
 '(dashboard-startup-banner 3 t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(evil-cross-lines t)
 '(evil-want-C-u-scroll t)
 '(flutter-sdk-path "~/flutter-sdk/flutter/" t)
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(highlight-indent-guides-auto-enabled t)
 '(highlight-indent-guides-method (quote column))
 '(highlight-indent-guides-responsive t)
 '(ivy-mode 1)
 '(ivy-posframe-mode 1 nil (ivy-posframe))
 '(ivy-posframe-parameters (quote ((left-fringe . 5) (right-fringe . 5))))
 '(ivy-rich-mode 1)
 '(lsp-auto-guess-root t)
 '(lsp-document-highlight nil t)
 '(lsp-document-sync-method nil)
 '(lsp-enable-completion-at-point nil)
 '(lsp-log-max 1000)
 '(lsp-prefer-flymake nil)
 '(lsp-print-io nil)
 '(lsp-print-performance t)
 '(lsp-response-timeout 15)
 '(lsp-trace nil t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 150)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-flycheck-enable nil)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position (quote top))
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify (quote on-demand))
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t)
 '(markdown-bold-underscore t)
 '(markdown-enable-math t)
 '(markdown-header-scaling t)
 '(markdown-hide-markup nil)
 '(markdown-hide-urls nil)
 '(markdown-indent-function t)
 '(markdown-italic-underscore t)
 '(org-bullets-bullet-list (quote ("" "" "" "" "" "" "" "" "" "")))
 '(paradox-enable nil t)
 '(paradox-github-token t)
 '(safe-local-variable-values (quote ((eval progn (pp-buffer) (indent-buffer)))))
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(git-gutter:added ((t (:background "#50fa7b"))))
 '(git-gutter:deleted ((t (:background "#ff79c6"))))
 '(git-gutter:modified ((t (:background "#f1fa8c"))))
 '(ivy-posframe ((t (:background "#282a36"))))
 '(ivy-posframe-border ((t (:background "#6272a4"))))
 '(ivy-posframe-cursor ((t (:background "#61bfff"))))
 '(markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
 '(markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.0))))
 '(markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
 '(markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
 '(markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
 '(markdown-list-face ((t (:foreground "mediumpurple"))))
 '(markdown-pre-face ((t (:foreground "#bd98fe"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 0.9))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
