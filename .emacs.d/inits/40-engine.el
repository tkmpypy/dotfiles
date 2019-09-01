(use-package engine-mode
  :config
  (engine-mode t)
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
  :keybinding "a")

  (defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")

  (defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")
  )

