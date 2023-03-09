(require 'my-elisp-debugger)

(defvar elisp-modes (list 'lisp-interaction-mode
                          'emacs-lisp-mode
                          'inferior-emacs-lisp-mode))

(defun my-elisp-describe-symbol ()
  "Describe symbol at point"
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun my-elisp-eval-symex ()
  "Eval symex"
  (interactive)
  (eval-last-sexp nil))

(defun my-elisp-eval-region ()
  "Eval region."
  (interactive)
  (eval-region (region-beginning) (region-end))
  (deactivate-mark))

(defun my-elisp-eval-exp-or-region ()
  "Eval region or last sexp"
  (interactive)
  (if mark-active
      (progn (my-elisp-eval-region)
             (message "Evaluated region."))
    (my-elisp-eval-symex)))

(defun my-elisp-eval (what)
  "Evaluate something"
  (interactive "cwhat?")
  (cond ((equal what ?e) (my-elisp-eval-exp-or-region))
        ((equal what ?r) (my-elisp-eval-exp-or-region))
        ((equal what ?f) (eval-defun nil))
        ((equal what ?d) (edebug-defun))
        (t nil)))

(defun my-elisp-show-definitions ()
  "Show all definitions in the current buffer.

This includes functions, variables, constants, etc."
  (interactive)
  ;; TODO: maybe just so that ^(symbol-containing-"def"...
  (occur
   (concat "\\(^(def\\|"
           "^(cl-def\\|"
           "^(use-package\\|"
           "^(evil-define\\|"
           "^(symex-def\\|"
           "^(ert-def\\)")))

(defhydra hydra-elisp (:timeout my-leader-timeout
                       :columns 2
                       :exit t)
  "Elisp menu"
  ("e" my-elisp-eval "Eval")
  ("v" my-elisp-eval "Eval")
  ("d" edebug-defun "Eval fn for debug")
  ("g" evil-jump-to-tag "Go to definition")
  ("t" ert-run-tests-interactively "Run ERT tests")
  ("T" ert-delete-all-tests "Unload all ERT tests")
  ("o" my-elisp-show-definitions "Show all definitions")
  ("i" my-elisp-describe-symbol "See documentation on this")
  ("?" my-elisp-describe-symbol "See documentation on this")
  ("r" my-lisp-repl "Go to elisp REPL"))

(defun register-elisp-leader ()
  "Pull up elisp hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-elisp/body))

;; register elisp leader in all elisp modes
(dolist (mode-name elisp-modes)
  (let ((mode-hook (intern (concat (symbol-name mode-name)
                                   "-hook"))))
    (add-hook mode-hook 'register-elisp-leader)))

;; to visit definition in ielm REPL
(add-hook 'inferior-emacs-lisp-mode-hook
          (lambda ()
            (general-evil-define-key 'normal inferior-emacs-lisp-mode-map
              (kbd "C-]") 'find-function-at-point)))

(provide 'my-elisp)
