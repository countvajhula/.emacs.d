;; Vim interface
(use-package evil
  :init
  ;; these settings are required by evil-collection
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; use Emacs keybindings when in insert mode }:)
  (setcdr evil-insert-state-map nil)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; C-z goes to emacs mode in "insert" mode
  (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
  ;; use "symbols" instead of simple words in point searches
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; recenter page after goto line (like Vim; this is otherwise overridden
  ;; due to "scroll-conservatively" settings)
  (advice-add 'evil-goto-line :around #'my-recenter-view-advice)
  (advice-add 'evil-search :around #'my-recenter-view-advice)
  (advice-add 'evil-goto-mark :around #'my-recenter-view-advice)

  (defun my-autoindent (&rest args)
    "Auto-indent line"
    (indent-according-to-mode))

  ;; preserve indentation when joining lines
  (advice-add 'evil-join :after #'my-autoindent)

  ;; Vim C-x line completion emulation,
  ;; from https://stackoverflow.com/questions/17928467/full-line-completion-in-emacs
  (defun my-expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
	   '(try-expand-line)))
      (call-interactively 'hippie-expand)))

  (define-key
    ;; Vim style full line completion
    evil-insert-state-map
    (kbd "C-x C-l")
    'my-expand-lines))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  ;; make indent-level a textobject in evil, e.g. cii and dai
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  ;; use non-spaced pairs when surrounding with an opening brace
  ;; from: https://github.com/emacs-evil/evil-surround/issues/86
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; this variable affects "blocking" hints, for example when deleting - the hint is displayed,
  ;; the deletion is delayed (blocked) until the hint disappers, then the hint is removed and the
  ;; deletion executed; it makes sense to have this duration short
  (setq evil-goggles-blocking-duration 0.120) ;; default is nil, i.e. use `evil-goggles-duration' which defaults to 0.200

  ;; this variable affects "async" hints, for example when indenting - the indentation
  ;; is performed with the hint visible, i.e. the hint is displayed, the action (indent) is
  ;; executed (asynchronous), then the hint is removed, highlighting the result of the indentation
  (setq evil-goggles-async-duration 0.360) ;; default is nil, i.e. use `evil-goggles-duration' which defaults to 0.200

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-magit)

(use-package multiple-cursors
  ;; the original multiple-cursors mode, looks great but isn't
  ;; compatible with evil mode. evil-mc looks pretty similar
  :disabled t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package evil-mc
  :config
  ;; by default evil-mc creates its own keymap that overrides some
  ;; useful evil mode bindings (like C-p!). Disable all default
  ;; evil-mc keybindings here (by making the keymap empty), in favor
  ;; of a custom hydra (defined below)
  (setcdr evil-mc-key-map nil)
  (global-evil-mc-mode 1)
  ;; interface with multiple cursors via a hydra menu
  (defhydra hydra-cursors (:columns 2)
    "Multiple cursors"
    ("a" evil-mc-make-all-cursors "make all cursors")
    ("n" evil-mc-make-and-goto-next-match "mark, go to next")
    ("N" evil-mc-make-and-goto-prev-match "mark, go to previous")
    ("s" evil-mc-skip-and-goto-next-match "skip, go to next")
    ("S" evil-mc-skip-and-goto-prev-match "skip, go to previous")
    ("h" evil-mc-make-cursor-here  "make cursor here")
    ("p" evil-mc-pause-cursors "pause cursors")
    ("P" evil-mc-resume-cursors "resume cursors")
    ("j" evil-mc-make-cursor-move-next-line "mark, go down")
    ("k" evil-mc-make-cursor-move-prev-line "mark, go up")
    ("f" evil-mc-make-and-goto-first-cursor "mark, goto first cursor")
    ("F" evil-mc-make-and-goto-last-cursor "mark, goto last cursor")
    ("l" evil-mc-make-and-goto-next-cursor "mark, goto next cursor")
    ("L" evil-mc-skip-and-goto-next-cursor "skip, goto next cursor")
    ("h" evil-mc-make-and-goto-prev-cursor "mark, goto previous cursor")
    ("H" evil-mc-skip-and-goto-prev-cursor "skip, goto previous cursor")
    ("<escape>" evil-mc-undo-all-cursors "undo all cursors"))

  ;; access the multiple-cursors menu via a "body" keybinding
  (global-set-key (kbd "s-d") 'hydra-cursors/body)
  ;; retain a convenient, non-hydra, escape hatch
  (global-set-key (kbd "s-D") 'evil-mc-undo-all-cursors))

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; widely used collection of useful snippets
(use-package yasnippet-snippets)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil)  ; workaround for undotree bug
  (global-set-key (kbd "C-c u") 'undo-tree-visualize)
  (global-set-key (kbd "C-c U") 'undo-tree-visualizer-abort))

(use-package avy
  ;; although avy-goto-word-1 is probably faster,
  ;; avy-goto-char-timer is more intuitive
  :bind ("M-s" . avy-goto-char-timer)
  :ensure t)

(use-package ace-jump-buffer
  :disabled t
  :bind ("C-x b" . ace-jump-buffer))

(use-package tab-indentation
  :load-path "~/.emacs.d/my-packages/")

(use-package highlight
  :disabled t)

(use-package my-navigation
  :after evil)

;; highlight matching paren
(show-paren-mode 1)
