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
(use-package org
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.3))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1))))
  (org-level-4 ((t (:inherit outline-4 :height 1.0))))
  (org-level-5 ((t (:inherit outline-5 :height 0.9))))
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org/todo.org"))
  (setq org-default-notes-file "notes.org")
  (setq org-hide-emphasis-markers t)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
	(quote (
		("NEXT" :foreground "red" :weight bold)
		("WAITING" :foreground "orange" :weight bold))))
  (setq org-log-done 'time)
  (setq org-clock-in-resume t)
  ;; (setq org-clock-in-switch-to-state "NEXT")
  (setq org-clock-out-when-done t)
  (setq org-pretty-entities t)
  (setq org-clock-persist t)
  ;; アンダースコアをエクスポートしない
  (setq org-export-with-sub-superscripts t)

  ; Org-captureの設定
  ; Org-captureのテンプレート（メニュー）の設定
  (setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
      ("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "INBOX")
          "* TODO %?\n %i\n %a")
      )))

(use-package org-bullets
      :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
      :hook (org-mode . org-bullets-mode))

;; (eval-after-load 'org
;;   '(progn
;;     (defun tkmpypy/org-clock-in-if-starting ()
;;     "Clock in when the task is marked STARTED."
;; 	(when (and (string= org-state "NEXT")
;; 		(not (string= org-last-state org-state)))
;; 	(org-clock-in)))

;;     (add-hook 'org-after-todo-state-change-hook
;; 		'tkmpypy/org-clock-in-if-starting)

;;     (defadvice org-clock-in (after tkmpypy activate)
;; 	"Set this task's status to 'STARTED'."
;; 	(org-todo "NEXT"))

;;     (defun tkmpypy/org-clock-out-if-waiting ()
;;     "Clock in when the task is marked STARTED."
;; 	(when (and (string= org-state "WAITING")
;; 		(not (string= org-last-state org-state)))
;; 	(org-clock-out)))
;;     (add-hook 'org-after-todo-state-change-hook
;; 	    'tkmpypy/org-clock-out-if-waiting)))
