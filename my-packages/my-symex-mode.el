;;; -*- lexical-binding: t -*-
;;; TODO: consider using S for dragging and C for movement (and then across all modes)
;;; TODO: move back/forward through tree "at same level" without going up or down (i.e. switch branches, ideally preserving position index within branch)
;;; TODO: traverse tree with side effect (traversal-method, side-effect-fn), to use for "indent forward" on paste
;;; TODO: incorporate more clear tree-related terminology
;;; TODO: C-j to move in greedily, going forward
;;; TODO: handle "contracts" of each abstraction level, and where conditions should go, rename functions for clarity. legitimate detours vs conditional itineraries, vs conditional motions
;;; TODO: take a symex and bring it out and before/after as a peer of the parent
;;; TODO: my-tidy-symex has edge cases in indenting from evil-cp-a-form, where symex begins with : (keyword arg) or #'
(require 'lispy)
(require 'paredit)
(require 'evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)
(require 'dash-functional)

(require 'symex-data)
(require 'symex-computations)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-transformations)
(require 'symex-misc)

;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURATION ;;;
;;;;;;;;;;;;;;;;;;;;;

;; use paredit balancing behavior in insert mode
(define-key
  evil-insert-state-map
  (kbd "\(")
  'paredit-open-round)

(define-key
  evil-insert-state-map
  (kbd "\)")
  'paredit-close-round)

(define-key
  evil-insert-state-map
  (kbd "\[")
  'paredit-open-square)

(define-key
  evil-insert-state-map
  (kbd "\]")
  'paredit-close-square)

(define-key
  evil-insert-state-map
  (kbd "<backspace>")
  'paredit-backward-delete)


(defhydra hydra-symex (:idle 1.0
                       :columns 5
                       :color pink
                       :body-pre (progn (symex-select-nearest)
                                        (evil-symex-state)))
  "Symex mode"
  ("(" (lambda ()
         (interactive)
         (symex-create 'round)) "()")
  ("[" (lambda ()
         (interactive)
         (symex-create 'square)) "[]")
  ("{" (lambda ()
         (interactive)
         (symex-create 'curly)) "{}")
  ("<" (lambda ()
         (interactive)
         (symex-create 'angled)) "<>")
  ("h" symex-go-backward "previous")
  ("j" symex-go-in "enter")
  ("k" symex-go-out "exit")
  ("l" symex-go-forward "next")
  ("f" (lambda ()
         (interactive)
         (symex-traverse-forward t)) "flow forward")
  ("b" (lambda ()
         (interactive)
         (symex-traverse-backward t)) "flow backward")
  ("F" (lambda ()
         (interactive)
         (symex-traverse-backward t)) "flow backward")
  ("C-k" symex-switch-branch-backward "switch branch backward")
  ("C-j" symex-switch-branch-forward "switch branch forward")
  ("y" symex-yank "yank (copy)")
  ("p" symex-paste-after "paste after")
  ("P" symex-paste-before "paste before")
  ("x" symex-delete "delete")
  ("c" symex-change "change" :exit t)
  ("s" symex-replace "replace" :exit t)
  ("H" symex-shift-backward "move backward")
  ("L" symex-shift-forward "move forward")
  ("K" paredit-raise-sexp "raise")
  ("s-J" symex-slurp-backward "slurp backward")
  ("s-H" symex-spit-backward "spit backward")
  ("s-L" symex-spit-forward "spit forward")
  ("s-K" symex-slurp-forward "slurp forward")
  ("z" symex-swallow "swallow")
  ("e" symex-evaluate "evaluate")
  ("E" symex-evaluate-pretty "pretty evaluate")
  ("d" symex-evaluate-definition "evaluate definition")
  (":" eval-expression "eval expression")
  ("t" my-switch-to-scratch-buffer "scratch buffer" :exit t)
  ("G" my-switch-to-messages-buffer "messages buffer" :exit t)
  ("r" symex-repl "go to REPL" :exit t)
  ("|" lispy-split "split")
  ("m" symex-join "merge (join)")
  ("\\" lispy-splice "splice (join to higher level)")
  (")" symex-wrap-round "wrap with ()")
  ("]" symex-wrap-square "wrap with []")
  ("}" symex-wrap-curly "wrap with {}")
  (">" symex-wrap-angled "wrap with <>")
  ("o" symex-open-line-after "open line after" :exit t)
  ("O" symex-open-line-before "open line before" :exit t)
  ("n" symex-insert-newline "newline")
  ("J" symex-join-lines "join lines")
  ("N" (lambda ()
         (interactive)
         (symex-join-lines t)) "join lines backwards")
  ("0" symex-goto-first "go to first")
  ("M-h" symex-goto-first "go to first")
  ("$" symex-goto-last "go to last")
  ("M-l" symex-goto-last "go to last")
  ("M-k" symex-goto-outermost "go to outermost")
  ("M-j" symex-goto-innermost "go to innermost")
  ("=" symex-tidy "tidy/indent")
  ("A" symex-append-after "append after symex" :exit t)
  ("a" symex-insert-at-end "append inside symex" :exit t)
  ("i" symex-insert-at-beginning "insert inside symex" :exit t)
  ("I" symex-insert-before "insert before symex" :exit t)
  ("w" symex-wrap "wrap with symex" :exit t)
  ("g" evil-jump-to-tag "Go to definition")
  (";" symex-eval-print "eval + print")
  ;; canonical action
  ("s-;" symex-evaluate "evaluate" :exit t)
  ;; escape hatches
  ("R" evil-replace-state nil :exit t)
  ("v" evil-visual-char nil :exit t)
  ("V" evil-visual-line nil :exit t)
  ("C-v" evil-visual-block nil :exit t)
  ;; standard exits
  ("?" symex-describe "info")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)  ; since y looks like inverted lambda
(global-set-key (kbd "s-;") 'hydra-symex/body)  ; since y is hard to reach

(provide 'my-symex-mode)
