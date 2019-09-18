(use-package dart-mode
  :init
  (add-hook 'dart-mode-hook
            (function (lambda ()
                        (setq evil-shift-width 2))))
  :config
  (setq dart-enable-analysis-server nil)
  :hook
  (dart-mode-hook . tkmpypy/flutter-hot-reload-enable))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "~/flutter-sdk/flutter/"))

(defun tkmpypy/flutter-hot-reload()
	(interactive)
	"Send a signal to daemon to hot reload."
	(start-process "emacs-flutter-hot-reloader" nil "pkill" "-SIGUSR1" "-f" "flutter_tool"))
(defun tkmpypy/flutter-hot-restart()
	(interactive)
	"Send a signal to daemon to hot restart."
	(start-process "emacs-flutter-hot-reloader" nil "pkill" "-SIGUSR2" "-f" "flutter_tool"))
(defun tkmpypy/flutter-hot-reload-enable()
	(interactive)
	"Enable flutter hot reload on save."
	(add-hook 'after-save-hook 'tkmpypy/flutter-hot-reload t t))
(defun tkmpypy/flutter-hot-reload-disable()
	(interactive)
	"Disable flutter hot reload on save."
	(remove-hook 'after-save-hook 'tkmpypy/flutter-hot-reload t))
