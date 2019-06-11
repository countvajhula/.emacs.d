(use-package racket-mode
  :config
  ;; explicitly indicate .rkt files are to be opened in racket-mode
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

(defvar racket-modes (list 'racket-mode
                           'racket-repl-mode))

(defun my-racket-describe-symbol ()
  "Describe symbol at point"
  (interactive)
  (racket-describe nil))

(defun racket--send-to-repl (code)
  "(Keeping this here for now pending possible incorporation into racket-mode)
Internal function to send CODE to the Racket REPL for evaluation.

Before sending the code (in string form), calls `racket-repl' and
`racket--repl-forget-errors'. Also inserts a ?\n at the process
mark so that output goes on a fresh line, not on the same line as
the prompt.

Afterwards call `racket--repl-show-and-move-to-end'."
  (racket-repl t)
  (racket--repl-forget-errors)
  (let ((proc (get-buffer-process racket--repl-buffer-name)))
    (with-racket-repl-buffer
      (save-excursion
        (goto-char (process-mark proc))
        (insert ?\n)
        (set-marker (process-mark proc) (point))))
    (comint-send-string proc code)
    (comint-send-string proc "\n"))
  (racket--repl-show-and-move-to-end))

(defun my-racket-eval-symex-pretty ()
  "Evaluate symex and render the result in a useful string form."
  (interactive)
  (let ((pretty-code (string-join
                      `("(let ([result "
                        ,(buffer-substring (racket--repl-last-sexp-start)
                                           (point))
                        "])"
                        " (cond [(stream? result) (stream->list result)]
                                  [(sequence? result) (sequence->list result)]
                                  [else result]))"))))
    (racket--send-to-repl pretty-code)))

(defun my-racket-eval-symex ()
  "Eval last sexp.

Accounts for different point location in evil vs emacs mode."
  (interactive)
  (save-excursion
    (when (equal evil-state 'normal)
      (forward-char))
    (racket-send-last-sexp)))

(defun my-racket-eval-region ()
  "Eval region"
  (interactive)
  (racket-send-region (region-beginning) (region-end))
  (deactivate-mark))

(defun my-racket-eval-exp-or-region ()
  "Eval region or last sexp"
  (interactive)
  (if mark-active
      (progn (my-racket-eval-region)
             (message "Evaluated region."))
    (my-racket-eval-symex)))

(defun my-racket-eval (what)
  "Evaluate something"
  (interactive "cwhat?")
  (cond ((equal what ?e) (my-racket-eval-exp-or-region))
        ((equal what ?r) (my-racket-eval-exp-or-region))
        ((equal what ?f) (racket-send-definition nil))
        (t nil)))

(defhydra hydra-racket (:timeout my-leader-timeout
                        :columns 2
                        :exit t)
  "Racket menu"
  ("e" my-racket-eval "Eval")
  ("v" my-racket-eval "Eval")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" my-racket-describe-symbol "See documentation on this")
  ("?" my-racket-describe-symbol "See documentation on this")
  ("r" racket-repl "Go to racket REPL")
  ("x" racket-run "Save and evaluate buffer in REPL"))

(defun register-racket-leader ()
  "Pull up racket hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-racket/body))

;; register racket leader in all racket modes
(dolist (mode-name racket-modes)
  (let ((mode-hook (intern (concat (symbol-name mode-name)
                                   "-hook"))))
    (add-hook mode-hook 'register-racket-leader)))

;; ensure that paredit delimiter behavior isn't overridden in REPL
;; by racket mode. Not sure why this happens, but it may be evil-mode related:
;; https://github.com/greghendershott/racket-mode/issues/289
;; For now, just manually override them back to paredit behavior.
;; Ideally, this fix should be removed when the cause is identified and
;; a proper solution is found.
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\(")
                           'paredit-open-round)))
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\)")
                           'paredit-close-round)))
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\[")
                           'paredit-open-square)))
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\]")
                           'paredit-close-square)))
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\{")
                           'paredit-open-curly)))
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\}")
                           'paredit-close-curly)))
;; double-quote behavior appears to be broken in racket-mode too
(add-hook 'racket-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "\"")
                           'paredit-doublequote)))
(add-hook 'racket-mode-hook
          (lambda ()
            (local-set-key (kbd "\"")
                           'paredit-doublequote)))

;; override evil jump to tag so it uses racket mode's "visit definition"
;; (add-hook 'racket-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-]")
;;                            'racket-visit-definition)))

(provide 'my-racket)
