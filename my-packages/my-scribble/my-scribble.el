(use-package scribble-mode
  :hook ((scribble-mode . (lambda ()
                            (setq indent-tabs-mode nil)))
         (scribble-mode . my-set-qi-insert-bindings)))

;; register racket leader in all racket modes
(defun my-scribble-show-definitions ()
  "Show all definitions in the current buffer.

This includes functions, variables, constants, etc."
  (interactive)
  (occur "\\(^\s*@def\\|^.*section{\\|^\s*@title{\\)"))

(defun my-scribble-open-output-file ()
  "Open HTML output."
  (interactive)
  (shell-command (string-join (list "open" " " (file-name-base (buffer-name)) ".html"))))

(defun my-scribble-compile ()
  "Compile to HTML."
  (interactive)
  (save-buffer)
  (shell-command (string-join (list "scribble" " " (buffer-file-name)))))

(defhydra hydra-scribble (:timeout my-leader-timeout
                                   :columns 2
                                   :exit t)
  "Scribble menu"
  ("o" my-scribble-show-definitions "Show all definitions")
  ("l" my-scribble-open-output-file "Open output file")
  ("i" my-racket-describe-symbol "See documentation on this")
  ("?" my-racket-describe-symbol "See documentation on this")
  ("C-?" racket-documentation-search "Search documentation")
  ("d" racket-documentation-search "Search documentation")
  ("x" my-scribble-compile "Compile to an output format")
  ("\\" my-scribble-compile "Compile to an output format"))

(defun register-scribble-leader ()
  "Pull up Scribble hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-scribble/body))

(add-hook 'scribble-mode-hook #'register-scribble-leader)

(provide 'my-scribble)
