(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "notes.org")

; Org-captureの設定

; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
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
    (find-file (concat "~/Dropbox/org/" file))))
