(use-package dashboard
    :diminish
    (dashboard-mode page-break-lines-mode)
    :custom
    (dashboard-startup-banner 2)
    (dashboard-items '((recents . 15)
               (projects . 5)
               (bookmarks . 5)
               (agenda . 5)))
    :config
    (dashboard-setup-startup-hook)
    (add-to-list 'dashboard-items '(agenda) t))
