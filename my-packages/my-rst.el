(defhydra hydra-rst (:timeout my-leader-timeout
                     :columns 2
                     :exit t)
  "reStructuredText menu"
  ("o" rst-toc "TOC")
  ("g" rst-toc-follow-link "Follow link")
  ;; ("i" my-racket-describe-symbol "See documentation on this")
  ;; ("?" my-racket-describe-symbol "See documentation on this")
  ("x" rst-compile "Compile to an output format")
  ("\\" rst-compile "Compile to an output format"))

(defun register-rst-leader ()
  "Pull up rST hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-rst/body))

;; register rst leader in all rst modes
(let ((mode-hook (intern (concat (symbol-name 'rst-mode)
                                 "-hook"))))
  (add-hook mode-hook 'register-rst-leader))

(provide 'my-rst)
