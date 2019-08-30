(use-package dart-mode
  :hook
  (dart-mode-hook . flycheck-mode))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "~/flutter-sdk/flutter/"))
