(use-package edebug
  :defer t
  :config
  ;; retain evil movement keys, don't override h
  (define-key edebug-mode-map (kbd "h") nil)
  ;; retain binding of "g" for edebug's "Go"
  (define-key edebug-mode-map (kbd "g") 'edebug-go-mode)
  (define-key edebug-mode-map (kbd "G") 'edebug-Go-nonstop-mode))

(use-package edebug-x
  :defer t
  :after edebug)

(defhydra hydra-debugger (:color amaranth
                          :columns 2)
  "Debugger"
  ("b" edebug-set-breakpoint "Set breakpoint")
  ("B" edebug-next-breakpoint "Visit next breakpoint")
  ("u" edebug-unset-breakpoint "Unset breakpoint")
  ("d" edebug-backtrace "Show program backtrace")
  ("e" edebug-eval-expression "Evaluate an expression")
  ("E" edebug-visit-eval-list "Visit eval list")
  ("v" edebug-update-eval-list "Update eval list (;-- to separate groups)")
  ("f" edebug-forward-sexp "Run for one expression (sets temporary breakpoint)")
  ("g" edebug-go-mode "Run until next breakpoint")
  ("G" edebug-Go-nonstop-mode "Go nonstop (interrupt with S)")
  ("h" edebug-goto-here "Proceed to here (sets temporary breakpoint)")
  ("o" edebug-step-out "Step out")
  ("i" edebug-step-in "Step in")
  ("c" edebug-continue-mode "Continue, pausing at each breakpoint")
  ("C" edebug-Continue-fast-mode "Continue, alighting at each breakpoint")
  ("p" edebug-bounce-point "bounce out?")
  ("S" edebug-stop "Stop")
  ("s" edebug-step-mode "step")
  ("n" edebug-next-mode "step to next after current expression")
  ("t" edebug-trace "Trace execution with pauses")
  ("T" edebug-Trace-fast-mode "Trace execution without pauses")
  ("x" edebug-set-conditional-breakpoint "set conditional breakpoint")
  ("X" edebug-set-global-break-condition "set break condition")
  ("w" edebug-where "return to current stop point")
  ("r" edebug-previous-result "show previous result")
  ("?" edebug-help "help")
  ("q" nil "suspend menu" :exit t)
  ("<escape>" top-level "exit" :exit t))

(global-set-key (kbd "s-q") 'hydra-debugger/body)

;;(add-hook 'edebug-mode-hook (lambda () (message "EDEBUG HOOK CALLED")))
;; (add-hook 'edebug-mode-hook (lambda () (when (not edebug-entered)
;;                                          (hydra-debugger/body))))

(provide 'my-elisp-debugger)
