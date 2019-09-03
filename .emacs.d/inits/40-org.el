(use-package org
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.3))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1))))
  (org-level-4 ((t (:inherit outline-4 :height 1.0))))
  (org-level-5 ((t (:inherit outline-5 :height 0.9))))
  )
(use-package org-bullets
      :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
      :hook (org-mode . org-bullets-mode))

(setq org-directory "~/Google ドライブ/org")
(setq org-default-notes-file "notes.org")
(setq org-hide-emphasis-markers t)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)")))
(setq org-log-done 'time)

; Org-captureの設定

; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
    '(("n" "Note" entry (file+headline "~/Google ドライブ/org/notes.org" "Notes")
       "* %?\nEntered on %U\n %i\n %a")
    ("t" "Todo" entry (file+headline "~/Google ドライブ/org/todo.org" "INBOX")
        "* TODO %?\n %i\n %a")
    ))

; メモをC-M-^一発で見るための設定
; https://qiita.com/takaxp/items/0b717ad1d0488b74429d から拝借
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Google ドライブ/org/" file))))
