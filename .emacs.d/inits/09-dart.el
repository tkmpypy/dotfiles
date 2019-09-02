(use-package dart-mode
  :config
  (setq dart-enable-analysis-server nil))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "~/flutter-sdk/flutter/"))
