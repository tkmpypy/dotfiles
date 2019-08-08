(use-package ace-window
    :functions hydra-frame-window/body
    :bind
    ("C-M-o" . hydra-frame-window/body)
    :custom
    (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
    :custom-face
    (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
    :preface
    (defvar is-window-maximized nil)
    (defun ladicle/toggle-window-maximize ()
	(interactive)
	(progn
	  (if is-window-maximized
	      (balance-windows)
	    (maximize-window))
	  (setq is-window-maximized
		(not is-window-maximized))))
    :config
    (use-package rotate
	:load-path "~/emacs.d/elisp"
	:bind
	("M-o SPC" . rotate-layout))
    (with-eval-after-load 'hydra
	(defhydra hydra-frame-window (:color pink :hint nil)
	  "
									╔════════╗
     Size^^    Zoom^^    Split^^     Frame^^               Buffer               ║ Window ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
	^_k_^       _z_      _-_       [_i_] select         _,_ ← switch → _._
	^^↑^^       ^↑^      ^↑^       [_s_] swap           [_d_] delete
    _h_ ←   → _l_   ^ ^      ^ ^       [_o_] next           [_D_] delete and kill
	^^↓^^       ^↓^      ^↓^       [_m_] maximize       [_r_] recentf
	^_j_^       _x_      _/_       [_O_] delete others  [_b_] select
  ╭──────────────────────────────────────────────────────────────────────────────╯
			   [_q_]: quit, [_<SPC>_]: rotate
"
	  ("K" kill-current-buffer :exit t)
	  ("D" kill-buffer-and-window :exit t)
	  ("O" delete-other-windows  :exit t)
	  ("F" toggle-frame-fullscreen)
	  ("i" ace-window)
	  ("s" ace-swap-window :exit t)
	  ("d" ace-delete-window)
	  ("m" ladicle/toggle-window-maximize :exit t)
	  ("x" text-scale-decrease)
	  ("z" text-scale-increase)
	  ("-" split-window-vertically)
	  ("/" split-window-horizontally)
	  ("h" shrink-window-horizontally)
	  ("k" shrink-window)
	  ("j" enlarge-window)
	  ("l" enlarge-window-horizontally)
	  ("," previous-buffer)
	  ("." next-buffer)
	  ("o" other-window)
	  ("d" kill-current-buffer)
	  ("r" counsel-recentf :exit t)
	  ("b" switch-to-buffer :exit t)
	  ("D" kill-buffer-and-window)
	  ("<SPC>" rotate-layout)
	  ("q" nil))))

(evil-leader/set-key "wr" 'hydra-frame-window/body)

