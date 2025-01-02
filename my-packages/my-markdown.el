(use-package markdown-mode
  :hook ((markdown-mode . (lambda ()
                            (setq indent-tabs-mode nil))))
  :mode ("README\\.md\\'" . gfm-mode))

(defun my-markdown-open-output-file ()
  "Open HTML output."
  (interactive)
  (shell-command (string-join (list "open" " " (file-name-base (buffer-name)) ".html"))))

(defhydra hydra-markdown (:timeout my-leader-timeout
                          :columns 2
                          :exit t)
  "Markdown menu"
  ;; ("o" rst-toc "TOC")
  ;; markdown-preview opens a fresh file each time
  ;; and so cannot be refreshed
  ("l" markdown-insert-link "Insert link")
  ("v" my-markdown-open-output-file "Open output file")
  ;; ("g" rst-toc-follow-link "Follow link")
  ;; ("i" my-racket-describe-symbol "See documentation on this")
  ;; ("?" my-racket-describe-symbol "See documentation on this")
  ("x" markdown-export "Compile to an output format")
  ("\\" markdown-export "Compile to an output format"))

(defun register-markdown-leader ()
  "Pull up Markdown hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-markdown/body))

;; register markdown leader in all markdown modes
(let ((mode-hook (intern (concat (symbol-name 'markdown-mode)
                                 "-hook"))))
  (add-hook mode-hook 'register-markdown-leader))

(provide 'my-markdown)
