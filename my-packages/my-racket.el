(defun my-set-qi-insert-bindings ()
  "Set insert mode bindings for the Qi language in Racket."
  (interactive)
  (local-set-key (kbd "C-;")
                 (lambda ()
                   (interactive)
                   (insert-char #x262f)))
  (local-set-key (kbd "C->")
                 (lambda ()
                   (interactive)
                   (insert "~>")))
  (local-set-key (kbd "C-<")
                 (lambda ()
                   (interactive)
                   (insert "-<")))
  (local-set-key (kbd "C-=")
                 (lambda ()
                   (interactive)
                   (insert-char #x23da)))
  (local-set-key (kbd "C-v")
                 (lambda ()
                   (interactive)
                   (insert-char #x25b3)))
  (local-set-key (kbd "C-S-v")
                 (lambda ()
                   (interactive)
                   (insert-char #x25bd))))

(defun my-enable-racket-xp-eldoc ()
  (add-function :before-until (local 'eldoc-documentation-function)
                #'racket-xp-eldoc-function))

(defun my-enable-racket-repl-eldoc ()
  (add-function :before-until (local 'eldoc-documentation-function)
                #'racket-repl-eldoc-function))

(use-package racket-mode
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . my-set-qi-insert-bindings)
         ;; WIP (disabled) attempt at trying eldoc for Racket
         ;; (racket-mode . my-enable-racket-xp-eldoc)
         ;; (racket-repl-mode . my-enable-racket-repl-eldoc)
         (racket-repl-mode . my-set-qi-insert-bindings))
  :straight
  (racket-mode
   :local-repo "~/.emacs.d/my-packages/racket-mode"
   :type git)
  :config
  ;; explicitly indicate .rkt files are to be opened in racket-mode
  ;; Somewhere (I don't know where), .rkt is already being added to
  ;; auto-mode-alist, and this causes `add-to-list` to do nothing
  ;; here, with the result that .rkt files are opened in scheme-mode
  ;; rather than racket-mode, since scheme-mode is also present in
  ;; auto-mode-alist for .rkt (who knows where this is added...),
  ;; in an earlier position in the list.
  ;; So, forcing addition at the head here by using `push` instead
  (push '("\\.rkt\\'" . racket-mode) auto-mode-alist)
  ;; indent levels for built-in forms
  ;; TODO: probably contribute upstream to racket-mode
  (put 'datum->syntax 'racket-indent-function 1)
  (put 'generator 'racket-indent-function 1)
  (put 'pattern 'racket-indent-function 1)
  ;; indent levels for minikanren forms
  (put 'run 'racket-indent-function 2)
  (put 'run* 'racket-indent-function 1)
  (put 'fresh 'racket-indent-function 1)
  (put 'conde 'racket-indent-function 0)
  ;; indent levels for syntax-spec
  (put 'syntax-spec 'racket-indent-function 0)
  (put 'nonterminal 'racket-indent-function 1)
  (put 'nonterminal/nesting 'racket-indent-function 2)
  (put 'nonterminal/two-pass 'racket-indent-function 1)
  (put 'host-interface/expression 'racket-indent-function 0)
  (put 'host-interface/definition 'racket-indent-function 1)
  (put 'host-interface/definitions 'racket-indent-function 1)
  ;; indent levels for cli forms
  (put 'program 'racket-indent-function 'defun)
  (put 'flag 'racket-indent-function 1)
  ;; indent levels for qi forms
  (put 'switch 'racket-indent-function 'defun)
  (put 'switch-lambda 'racket-indent-function 1)
  (put 'on 'racket-indent-function 1)
  (put 'π 'racket-indent-function 1)
  (put 'try 'racket-indent-function 1)
  (put '\#%deforestable 'racket-indent-function 3)
  ;; indent levels for db interaction
  (put 'query-exec 'racket-indent-function 1)
  (put 'query-value 'racket-indent-function 1)
  (put 'query-list 'racket-indent-function 1)
  ;; one-offs / project-specific
  (put 'report-syntax-error 'racket-indent-function 1)
  ;; raqit experiments
  (put 'fun 'racket-indent-function 'defun)
  (put 'loop 'racket-indent-function 'defun)
  (put 'flow 'racket-indent-function 'defun)

  ;; Rhombus config (maybe move to separate, dependent
  ;; file, eventually)
  (racket-add-back-end "~/work/racket/rhombus"
                       :racket-program "~/work/racket/racket/racket/bin/racket")
  (push '("\\.rhm\\'" . racket-mode) auto-mode-alist))

(defvar racket-modes (list 'racket-mode
                           'racket-repl-mode))

(defun my-racket-describe-symbol ()
  "Describe symbol at point"
  (interactive)
  (cond (racket-xp-mode (racket-xp-describe))
        ((eq major-mode 'racket-repl-mode) (racket-repl-describe))
        (t (error "Enable either racket-xp-mode or start the REPL!"))))

(defun my-racket-send-to-repl (code)
  "Send CODE to the Racket REPL for evaluation.

Before sending the code (in string form), calls `racket-repl' and
`racket--repl-forget-errors'. Also inserts a ?\n at the process
mark so that output goes on a fresh line, not on the same line as
the prompt.

Afterwards call `racket--repl-show-and-move-to-end'."
  (racket-repl t)
  (racket--repl-forget-errors)
  (let ((proc (get-buffer-process racket-repl-buffer-name)))
    (with-racket-repl-buffer
      (save-excursion
        (goto-char (process-mark proc))
        (insert ?\n)
        (set-marker (process-mark proc) (point))))
    (comint-send-string proc code)
    (comint-send-string proc "\n"))
  (when (fboundp 'racket--repl-show-and-move-to-end)
    (racket--repl-show-and-move-to-end)))

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
    (my-racket-send-to-repl pretty-code)))

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

(defun my-racket-show-definitions ()
  "Show all definitions in the current buffer.

This includes functions, variables, constants, etc."
  (interactive)
  (occur "\\(^\s*(define\\|^(struct\\|^(begin\\)"))

(defun my-racket-show-references ()
  "Show all references to identifier under point."
  (interactive)
  (let ((xref-prompt-for-identifier nil))
    (call-interactively #'xref-find-references)))

(defun my-racket-macro-stepper ()
  "Use the macro stepper to expand the current expression."
  (interactive)
  (let ((expr (string-join
               `("(expand/step " "#'" ,(thing-at-point 'sexp) ")"))))
    (symex--racket-send-to-repl expr)))

(defun my-racket-launch-macro-stepper ()
  "Use the macro stepper to expand the current expression."
  (interactive)
  (let ((expr1 "(require macro-debugger/stepper)")
        (expr2 (string-join
                `("(expand/step " "#'" ,(thing-at-point 'sexp) ")"))))
    (symex--racket-send-to-repl expr1)
    (symex--racket-send-to-repl expr2)))

(defun my-racket-syntax-browser ()
  "Explore a syntax object using the syntax browser."
  (interactive)
  (let ((expr (string-join
               `("(browse-syntax " ,(thing-at-point 'sexp) ")"))))
    (symex--racket-send-to-repl expr)))

(defun my-racket-launch-syntax-browser ()
  "Explore a syntax object using the syntax browser."
  (interactive)
  (let ((expr1 "(require macro-debugger/stepper)")
        (expr2 (string-join
                `("(browse-syntax " ,(thing-at-point 'sexp) ")"))))
    (symex--racket-send-to-repl expr1)
    (symex--racket-send-to-repl expr2)))

(defun my-racket-tidy-node ()
  "Rules for tidying individual nodes.

A helper used while tidying a macro expansion."
  (let ((expr (thing-at-point 'sexp)))
    (cond ((s-starts-with-p "#%app" expr)
           (symex-delete 1))
          ((equal "quote" expr)
           (symex-delete 1)
           (paredit-raise-sexp))
          ((equal "#%top" expr)
           (symex-delete 2)
           (paredit-raise-sexp)))))

(defun my-racket-tidy-expanded-macro ()
  "Make a macro expansion more readable.

The Racket Macro Stepper shows the expanion in all its gory detail
that is incredibly helpful while debugging. But sometimes you're just
curious what the macro expands to, in conventional, simple terms. E.g.
Does `cond` expand to `if`? What does `compose` expand to? In these
cases, the full expansion output contains too much information. This
simple utility uses a Symex traversal with a side effect to remove
anything in the core Racket language that we consider 'noise', so that
the output more closely resembles the usual Racket surface language."
  (interactive)
  (symex--do-while-traversing #'my-racket-tidy-node symex--traversal-preorder))


(defhydra hydra-racket (:timeout my-leader-timeout
                        :columns 2
                        :exit t)
  "Racket menu"
  ("e" my-racket-eval "Eval")
  ("v" my-racket-eval "Eval")
  ("m" my-racket-macro-stepper "Macro Stepper")
  ("M" my-racket-launch-macro-stepper "Launch macro stepper")
  ("s" my-racket-syntax-browser "Syntax Browser")
  ("S" my-racket-launch-syntax-browser "Launch syntax browser")
  ("g" evil-jump-to-tag "Go to definition")
  ("f" my-racket-show-references "Show references")
  ("o" my-racket-show-definitions "Show all definitions")
  ("i" my-racket-describe-symbol "See documentation on this")
  ("?" my-racket-describe-symbol "See documentation on this")
  ("C-?" racket-documentation-search "Search documentation")
  ("d" racket-documentation-search "Search documentation")
  ("r" (lambda ()
         (interactive)
         (racket-repl)
         (goto-char (point-max))) "Go to racket REPL")
  ("a" racket-xp-mode "Toggle Racket XP mode")
  ("x" racket-run "Save and evaluate buffer in REPL")
  ("\\" racket-run "Save and evaluate buffer in REPL"))

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
(add-hook 'racket-mode-hook
          (lambda ()
            (general-evil-define-key 'normal racket-mode-map
              (kbd "C-]") 'xref-find-definitions)
            (general-evil-define-key 'normal racket-mode-map
              (kbd "C-{") 'xref-pop-marker-stack))) ; can't rebind C-[ (treated as escape)

;; hover eldoc tooltips at point
(add-hook 'racket-mode-hook
          (lambda ()
            (eldoc-box-hover-at-point-mode 1)))

(provide 'my-racket)
