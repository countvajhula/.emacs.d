;; python IDE
(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-modules
	(remove 'elpy-module-highlight-indentation
		elpy-modules))
  (setq elpy-rpc-python-command "python3")
  ;; use ipython instead of regular shell
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  ;; use jedi for completion with elpy instead of rope
  (setq elpy-rpc-backend "jedi")
  (setq python-check-command "~/.local/bin/pyflakes")
  ;; use flycheck instead of flymake
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; (so far unsuccessful) attempt to get autocompletion in python shell
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   ;; putting elpy backend at the head to priortize
                   ;; python-specific completions. Company will stop at the first
                   ;; backend that provides a non-nil response. Some backends like dabbrev
                   ;; will always return non-nil so best to put them at the end, see:
                   ;; https://www.reddit.com/r/emacs/comments/844hnt/configuring_companymode/
                   ;; Also, a list of backends instead of a single backend groups results
                   ;; from all of those backends for the given prefix, i.e. it is a composite
                   ;; backend grouped with a logical "OR"
                   ;; Note that using just dabbrev here works in the shell :shrug:
                   ;; (cons '(company-dabbrev-code company-elpy-backend company-yasnippet) company-backends)
                   '((company-capf company-dabbrev-code company-yasnippet elpy-company-backend company-keywords)
                     company-dabbrev)))))

(defhydra hydra-python (:timeout my-leader-timeout
                        :columns 2
                        :exit t)
  "Python menu"
  ("c" elpy-check "Run lint checks")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" elpy-doc "See documentation on this")
  ("o" elpy-occur-definitions "See all definitions in current buffer")
  ("r" elpy-shell-switch-to-shell "Go to Python REPL")
  ("x" elpy-shell-send-region-or-buffer "Send to REPL")
  ("t" elpy-test "Run test(s)"))

;; pull up python hydra with local leader
(add-hook 'python-mode-hook
          (lambda () (general-define-key
                      :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-python/body)))

(provide 'my-python)
