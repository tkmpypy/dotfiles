(use-package lsp-mode
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance t)
  (lsp-log-max 1000)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 15)
  (lsp-enable-completion-at-point nil)
  :hook
  (go-mode . lsp)
  (typescript-mode . lsp)
  (vue-mode . lsp)
  (python-mode . lsp)
  (web-mode . lsp)
  (js2-mode . lsp)
  (dart-mode . lsp)
  :bind
  (:map lsp-mode-map
	("C-c r"   . lsp-rename))
  :config
  (setq lsp-use-native-json t)
  (setq lsp-json-use-lists t)
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
    (lsp-ui-sideline-enable nil)
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
    :config
    (setq lsp-ui-doc-border "violet")
    (setq lsp-ui-sideline-update-mode 'point)
    :preface
    (defun tkmpypy/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
	  (lsp-ui-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    ;;:hook
    ;;(lsp-mode . lsp-ui-mode)
    )
  ;; Lsp completion
  (use-package company-box
    :after (company all-the-icons)
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-icons-alist 'company-box-icons-all-the-icons))
  (use-package company-quickhelp)
  (use-package company-lsp
    :custom
    (company-lsp-cache-candidates t) ;; always using cache
    (company-lsp-filter-candidates t)
    (company-lsp-async t)
    (company-lsp-enable-recompletion t)
    (company-lsp-enable-snippet t))
  (use-package company-tabnine
    :after (company)
    :config
    (push 'company-tabnine company-backends))
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
    (setq company-dabbrev-downcase t)
    (setq company-tooltip-limit 10)
    (setq company-tooltip-idle-delay 0)
    (push 'company-lsp company-backends)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (setq company-show-numbers t)
    (global-company-mode))
  )

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "~/work/sourcekit-lsp/.build/x86_64-apple-macosx10.10/debug/sourcekit-lsp")))
