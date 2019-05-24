(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 3)
  (dashboard-items '((recents . 15)
                     (agenda . 5)
                     (projects . 5)
                     (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))
