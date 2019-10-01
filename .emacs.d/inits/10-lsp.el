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
  (lsp-enable-completion-at-point t)
  (lsp-document-highlight nil)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil

  :hook
  (go-mode . lsp)
  (typescript-mode . lsp)
  (vue-mode . lsp)
  (python-mode . lsp)
  (js2-mode . lsp)
  (dart-mode . lsp)
  (web-mode . lsp)
  (css-mode . lsp)
  (rust-mode . lsp)

  :bind
  (:map lsp-mode-map
	("C-c r"   . lsp-rename))
  :config
  (setq lsp-eldoc-render-all nil)
  (setq lsp-eldoc-enable-hover nil)

  ;; (setq lsp-use-native-json t)
  ;; (setq lsp-json-use-lists t)
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-enable-file-watchers t)ß
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
    (lsp-ui-flycheck-enable nil)
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
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    ;;:hook
    ;;(lsp-mode . lsp-ui-mode)
    )
  ;; Lsp completion
  (use-package company-box
    :after (company all-the-icons)
    :hook (company-mode . company-box-mode)
    :init
    (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate t)
    (setq company-box-max-candidates 50)
    (setq company-box-doc-enable nil)

    (with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-fileicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
              (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
              (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
              (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
              (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
              (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
              (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
              (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
              (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
              (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))

  (use-package company-posframe
    :hook (company-mode . company-posframe-mode))

  (use-package company-quickhelp
    :defines company-quickhelp-delay
    :bind
    (:map company-active-map
	  ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :custom (company-quickhelp-delay 0.8))
  (use-package company-lsp
    :custom
    (company-lsp-cache-candidates t) ;; always using cache
    (company-lsp-filter-candidates t)
    (company-lsp-async t)
    (company-lsp-enable-recompletion t)
    (company-lsp-enable-snippet t))
  ;; (use-package company-tabnine)
  (use-package company
    :hook
    (after-init . global-company-mode)
    ((go-mode
      python-mode
      rust-mode
    typescript-mode
    js2-mode
    dart-mode
    web-mode
    css-mode
    vue-mode) . (lambda () (set (make-local-variable 'company-backends)
                        '((company-yasnippet
                            company-lsp
                            company-files
                            ;; company-dabbrev-code
                            )))))
    :config
    (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    ;;(define-key company-search-map (kbd "C-n") 'company-select-next)
    ;;(define-key company-search-map (kbd "C-p") 'company-select-previous)
    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
    (setq company-idle-delay .1) ; デフォルトは0.5
    (setq company-minimum-prefix-length 1) ; デフォルトは4
    (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
    (setq completion-ignore-case t)
    (setq company-tooltip-limit 10)
    (setq company-tooltip-idle-delay 0)
    (setq company-dabbrev-downcase nil))

  (use-package company-prescient
    :after prescient
    :config (company-prescient-mode))

  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
    (setq lsp-sourcekit-executable (expand-file-name "~/work/sourcekit-lsp/.build/x86_64-apple-macosx10.10/debug/sourcekit-lsp")))
)
