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
