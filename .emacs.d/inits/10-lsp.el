(use-package lsp-mode
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; always send incremental document
  (lsp-response-timeout 5)
  (lsp-prefer-flymake nil)
  (lsp-enable-completion-at-point t)
  :hook
  (go-mode . lsp)
  (typescript-mode . lsp)
  (vue-mode . lsp)
  (python-mode . lsp)
  (web-mode . lsp)
  (js2-mode . lsp)
  :bind
  (:map lsp-mode-map
  ("C-c r"   . lsp-rename))
  :config
  (setq lsp-eldoc-render-all nil)
  (setq lsp-eldoc-enable-hover nil)
  (require 'lsp-clients)
  ;; LSP UI tools
  (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    (lsp-ui-doc-position 'top)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable t)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions nil)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
    ("C-c C-r" . lsp-ui-peek-find-references)
    ("C-c C-j" . lsp-ui-peek-find-definitions)
    ("C-c i"   . lsp-ui-peek-find-implementation)
    ("C-c m"   . lsp-ui-imenu)
    ("C-c s"   . lsp-ui-sideline-mode)
    ("C-c d"   . ladicle/toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode))
  ;; Lsp completion
  (use-package company-box
    :hook (company-mode . company-box-mode))
  (use-package company-quickhelp)
  (use-package company-lsp
    :custom
    (company-lsp-cache-candidates t) ;; always using cache
    (company-lsp-filter-candidates t)
    (company-lsp-async t)
    (company-lsp-enable-recompletion t)
    (company-lsp-enable-snippet t))
  (use-package company
    :init
    (add-hook 'company-mode-hook
	      (lambda ()
		(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
		(define-key company-active-map (kbd "C-p") 'company-select-previous)
		(define-key company-search-map (kbd "C-n") 'company-select-next)
		(define-key company-search-map (kbd "C-p") 'company-select-previous)))
    :config
    (global-company-mode)
    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
    (setq company-idle-delay 0) ; デフォルトは0.5
    (setq company-minimum-prefix-length 1) ; デフォルトは4
    (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase nil)
    (push 'company-lsp company-backends))
  )

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "~/work/sourcekit-lsp/.build/x86_64-apple-macosx10.10/debug/sourcekit-lsp")))
