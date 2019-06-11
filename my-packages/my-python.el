;; python IDE
(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-modules
	(remove 'elpy-module-highlight-indentation
		elpy-modules))
  (setq elpy-rpc-python-command "python3")
  ;; use jedi for completion with elpy instead of rope
  (setq elpy-rpc-backend "jedi")
  (setq python-check-command "~/.local/bin/pyflakes")
  (add-hook 'python-mode-hook
			(lambda () (show-paren-mode 1)))
  (add-hook 'python-mode-hook
			(lambda ()
			  (setq tab-width 4)
			  (setq python-indent-offset 4))))

(defhydra hydra-python (:timeout my-leader-timeout
                        :columns 2
                        :exit t)
  "Python menu"
  ("c" elpy-check "Run lint checks")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" elpy-doc "See documentation on this")
  ("o" elpy-occur-definitions "See all definitions in current buffer")
  ("p" elpy-shell-switch-to-shell "Go to Python REPL")
  ("r" elpy-shell-send-region-or-buffer "Send to REPL")
  ("t" elpy-test "Run test(s)"))

;; pull up python hydra with local leader
(add-hook 'python-mode-hook
          (lambda () (general-define-key
                      :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-python/body)))

(provide 'my-python)
