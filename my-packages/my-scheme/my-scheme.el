(use-package geiser
  :config
  (setq geiser-active-implementations '(racket)))

(defun my-scheme-describe-symbol ()
  "Describe symbol at point"
  (interactive)
  (geiser-doc-symbol-at-point))

(defun my-scheme-eval-symex ()
  "Eval symex"
  (interactive)
  (geiser-eval-last-sexp nil))

(defun my-scheme-eval-region ()
  "Eval region."
  (interactive)
  (geiser-eval-region (region-beginning) (region-end))
  (deactivate-mark))

(defun my-scheme-eval-exp-or-region ()
  "Eval region or last sexp"
  (interactive)
  (if mark-active
      (progn (my-scheme-eval-region)
             (message "Evaluated region."))
    (my-scheme-eval-symex)))

(defun my-scheme-eval (what)
  "Evaluate something"
  (interactive "cwhat?")
  (cond ((equal what ?e) (my-scheme-eval-exp-or-region))
        ((equal what ?r) (my-scheme-eval-exp-or-region))
        ((equal what ?f) (geiser-eval-definition nil))
        (t nil)))

(defhydra hydra-scheme (:timeout my-leader-timeout
                        :columns 2
                        :exit t)
  "Scheme menu"
  ("e" my-scheme-eval "Eval")
  ("v" my-scheme-eval "Eval")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" my-scheme-describe-symbol "See documentation on this")
  ("?" my-scheme-describe-symbol "See documentation on this")
  ("r" geiser-mode-switch-to-repl "Go to scheme REPL"))

(defun register-scheme-leader ()
  "Pull up scheme hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-scheme/body))

;; register scheme leader in all scheme modes
(add-hook 'scheme-mode-hook 'register-scheme-leader)

(provide 'my-scheme)
