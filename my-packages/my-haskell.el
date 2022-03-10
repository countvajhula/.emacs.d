(use-package haskell-mode
  :defer t)

(defvar haskell-modes (list 'haskell-mode
                            'inferior-haskell-mode))

(defun my-haskell-show-definitions ()
  "Show all definitions in the current buffer.

This includes functions, variables, constants, etc."
  (interactive)
  (occur "^[^\s]+.*="))

(defun my-haskell-go-to-repl ()
  "Go to the REPL."
  (interactive)
  (let ((proc (inferior-haskell-process)))
    (switch-to-buffer-other-window (process-buffer proc))))

(defun my-haskell-send-region-to-repl (start end)
  "Send region to the REPL."
  (interactive "r")
  (let ((proc (inferior-haskell-process)))
    (comint-send-region proc (region-beginning) (region-end))))

(defun my-haskell-run-buffer ()
  "Load current buffer into REPL."
  (interactive)
  (let ((module-name (file-name-base
                      (buffer-file-name))))
    (my-haskell-go-to-repl)
    (insert ":l" " " module-name)
    (comint-send-input)))

(defhydra hydra-haskell (:timeout my-leader-timeout
                         :columns 2
                         :exit t)
  "Haskell menu"
  ;; TODO: show references
  ("e" my-haskell-send-region-to-repl "Eval")
  ("v" my-haskell-send-region-to-repl "Eval")
  ("g" haskell-mode-jump-to-def-or-tag "Go to definition")
  ("o" my-haskell-show-definitions "Show all definitions")
  ("i" haskell-hoogle "Lookup docs (Hoogle)")
  ("?" haskell-hoogle "Lookup docs (Hoogle)")
  ("C-?" haskell-hoogle "Lookup docs (Hoogle)")
  ("r" my-haskell-go-to-repl "REPL")
  ("x" my-haskell-run-buffer "Save and evaluate buffer in REPL")
  ("\\" my-haskell-run-buffer "Save and evaluate buffer in REPL"))

(defun register-haskell-leader ()
  "Pull up haskell hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-haskell/body))

;; register haskell leader in all haskell modes
(dolist (mode-name haskell-modes)
  (let ((mode-hook (intern (concat (symbol-name mode-name)
                                   "-hook"))))
    (add-hook mode-hook 'register-haskell-leader)))

(provide 'my-haskell)
