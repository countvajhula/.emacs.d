(progn
  ;; conveniently enter math and greek symbols

  ;; leverage built-in input methods support in emacs to enter greek letters
  ;; by using the prefix ",."
  ;; adapted from https://emacs.stackexchange.com/a/53810
  (set-input-method "greek")
  (setq greek-map (quail-map))
  ;; add a translation rule to the TeX input method to delegate to
  ;; the greek input method
  (set-input-method "TeX")
  (quail-defrule ",." greek-map)
  ;; set default input method to TeX so that it can be activated
  ;; with C-\, e.g. to enter math or other symbols in general
  (setq default-input-method "TeX"))

;; Vim interface
(use-package evil
  :custom
  ;; these settings are required by evil-collection
  (evil-undo-system 'undo-tree)
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t) ;; most usecases for Emacs C-u are best handled in normal mode
  ;; use "symbols" instead of simple words in point searches
  (evil-symbol-word-search t)
  (evil-move-cursor-back nil)

  :config
  (evil-mode 1)
  ;; use Emacs keybindings when in insert mode }:)
  (setcdr evil-insert-state-map nil)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; C-z goes to emacs mode in "insert" mode
  (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
  ;; so that W uses symbols
  ;; (while w uses words, and # and * use symbols)
  (defalias #'forward-evil-WORD #'forward-evil-symbol)
  (global-set-key (kbd "M-u") 'universal-argument) ;; close enough, if needed

  ;; recenter page after goto line (like Vim; this is otherwise overridden
  ;; due to "scroll-conservatively" settings)
  (advice-add 'evil-goto-line :around #'my-recenter-view-advice)
  (advice-add 'evil-search :around #'my-recenter-view-advice)
  (advice-add 'evil-goto-mark :around #'my-recenter-view-advice)

  (defun my-autoindent (&rest args)
    "Auto-indent line"
    ;; this is necessary because advice provides arguments
    ;; which are not accepted by the underlying function;
    ;; would be better to do it functionally with a generic
    ;; wrapper that invokes the underlying function while
    ;; ignoring passed-in args ("thunk*")
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

  :straight
  (evil-collection
   :type git
   :host github
   :repo "countvajhula/evil-collection" ; temporarily, for ripgrep config
   :branch "add-ripgrep-mode")

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
  :after hydra
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

(use-package company
  :init
  (setq company-require-match nil)            ; Don't require match, so you can still move your cursor as expected.
  (setq company-tooltip-align-annotations t)  ; Align annotation to the right side.
  (setq company-dabbrev-downcase nil)         ; No downcase when completion.
  :config
  ;; enable company mode autocompletion in all buffers
  (setq company-idle-delay 0.2)
  ;; company-capf seems to block succeeding backends even if it doesn't have a match?
  ;; this could mean that oddmuse and dabbrev are never going to be hit
  ;; use company-diag at point to debug
  (setq company-backends
        '(uucompany-bbdb company-eclim company-semantic company-clang company-xcode
                         company-cmake company-files
                         (company-capf company-dabbrev-code company-gtags company-etags company-keywords)
                         company-oddmuse company-dabbrev))
  ;; Enable downcase only when completing the completion.
  ;; This and the :init config from https://github.com/jcs-elpa/company-fuzzy
  ;; (defun jcs--company-complete-selection--advice-around (fn)
  ;;   "Advice execute around `company-complete-selection' command."
  ;;   (let ((company-dabbrev-downcase t))
  ;;     (call-interactively fn)))
  ;; (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)
  (global-company-mode 1)
  ;; (global-company-fuzzy-mode 1)
  )

(use-package company-jedi)

(use-package scroll-on-jump
  :disabled t
  :straight
  (scroll-on-jump
   :type git
   :host gitlab
   :repo "ideasman42/emacs-scroll-on-jump")

  :config
  (setq scroll-on-jump-duration 0.6)
  (with-eval-after-load 'evil
    (scroll-on-jump-advice-add evil-undo)
    (scroll-on-jump-advice-add evil-redo)
    (scroll-on-jump-advice-add evil-jump-item)
    (scroll-on-jump-advice-add evil-jump-forward)
    (scroll-on-jump-advice-add evil-jump-backward)
    (scroll-on-jump-advice-add evil-scroll-down)
    (scroll-on-jump-advice-add evil-scroll-up)
    (scroll-on-jump-advice-add my-jump-down)
    (scroll-on-jump-advice-add my-jump-up)
    (scroll-on-jump-advice-add symex-go-backward)
    (scroll-on-jump-advice-add symex-go-forward)
                                        ;(scroll-on-jump-advice-add evil-scroll-page-down)
                                        ;(scroll-on-jump-advice-add evil-scroll-page-up)
    (scroll-on-jump-advice-add evil-ex-search-next)
    (scroll-on-jump-advice-add evil-ex-search-previous)
    (scroll-on-jump-advice-add evil-forward-paragraph)
    (scroll-on-jump-advice-add evil-backward-paragraph))
  (with-eval-after-load 'goto-chg
    (scroll-on-jump-advice-add goto-last-change)
    (scroll-on-jump-advice-add goto-last-change-reverse)))

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
  :bind ("M-s" . avy-goto-char-timer))

(use-package ace-jump-buffer
  :disabled t
  :bind ("C-x b" . ace-jump-buffer))

(use-package tab-indentation
  :straight
  (tab-indentation :local-repo "~/.emacs.d/my-packages/tab-indentation" :type nil))

(use-package highlight
  :disabled t)

(use-package my-navigation
  :after evil
  :straight
  (my-navigation :local-repo "~/.emacs.d/my-packages/my-navigation" :type nil))

;; highlight matching paren
(show-paren-mode 1)
