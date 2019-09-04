;; straight.elによるorg modeのインストールに先立つ準備
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

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
(use-package org
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.3))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1))))
  (org-level-4 ((t (:inherit outline-4 :height 1.0))))
  (org-level-5 ((t (:inherit outline-5 :height 0.9))))
  :config
  (setq org-directory "~/Google ドライブ/org")
  (setq org-agenda-files '("~/Google ドライブ/org/todo.org"))
  (setq org-default-notes-file "notes.org")
  (setq org-hide-emphasis-markers t)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
	(quote (("NEXT" :foreground "red" :weight bold))))
  (setq org-log-done 'time)
  (setq org-clock-in-resume t)
  (setq org-clock-in-switch-to-state "NEXT")
  (setq org-clock-out-when-done t)
  (setq org-pretty-entities t)
  (setq org-clock-persist t)
  
  ; Org-captureの設定
  
  ; Org-captureのテンプレート（メニュー）の設定
  (setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Google ドライブ/org/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
      ("t" "Todo" entry (file+headline "~/Google ドライブ/org/todo.org" "INBOX")
          "* TODO %?\n %i\n %a")
      )))

(use-package org-bullets
      :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
      :hook (org-mode . org-bullets-mode))
