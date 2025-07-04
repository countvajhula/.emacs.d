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
  (setq default-input-method "TeX")
  ;; mostly for Evil's sentence noun
  (setq sentence-end-double-space nil))

;; Vim interface
(use-package evil
  :straight
  (evil
   :local-repo "~/.emacs.d/my-packages/evil"
   :type git)

  :custom
  ;; these settings are required by evil-collection
  (evil-undo-system 'undo-tree)
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t) ;; most usecases for Emacs C-u are best handled in normal mode
  ;; use "symbols" instead of simple words in point searches
  (evil-symbol-word-search t)
  ;; no cursor creep - keep mode transitions idempotent
  (evil-move-cursor-back nil)
  ;; Y yanks to eol like D and C (and unlike V)
  (evil-want-Y-yank-to-eol t)

  :config
  (evil-mode 1)
  ;; use Emacs keybindings when in insert mode }:)
  (setcdr evil-insert-state-map nil)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; C-z goes to emacs mode in "insert" mode
  (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
  (define-key evil-normal-state-map ; convenient way to add a space
    (kbd "M-SPC")
    (lambda ()
      (interactive)
      (insert " ")))
  ;; convenient way to add a newlines above and below
  (define-key evil-normal-state-map
    (kbd "M-o")
    (lambda ()
      (interactive)
      (with-undo-collapse
        (save-excursion
          (end-of-line)
          (newline)))))
  (define-key evil-normal-state-map
    (kbd "M-O")
    (lambda ()
      (interactive)
      (with-undo-collapse
        (save-excursion
          (beginning-of-line)
          (newline)))))

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
    'my-expand-lines)

  ;; don't use evil bindings in Info mode
  ;; Note: doesn't work for some reason
  (evil-set-initial-state 'Info-mode 'emacs)

  ;; ** THIS IS WIP **
  ;; From: https://emacs.stackexchange.com/a/21402/24024
  ;; TODO: handle other in-word characters like
  ;; / and \ and _ and .
  ;; and ensure it only happens when the character
  ;; is _within_ the word.
  ;; is -within- the word <- here it should |-within- and then
  ;; |the
  ;; once this is working as intended, create a feature request
  ;; on the evil repo with the behavior spec in each case
  (defun my-word-separator-p (char)
    "Check whether CHAR is a word separator."
    (or (eq char ?-)
        (eq char ?.)
        (eq char ?,)
        (eq char ?/)
        (eq char ?\\)))

  (defun skip-dash-backward (n &rest foo)
    (when (my-word-separator-p (char-before (point)))
      (backward-char)))

  (defun skip-dash-forward (n &rest foo)
    (when (and (my-word-separator-p (char-after (point)))
               (not (bolp)))
      (forward-char)))

  (defun skip-dash-forward-end (n &rest foo)
    (when (my-word-separator-p (char-after (+ 1 (point))))
      (forward-char)))

  ;; (advice-add 'evil-forward-word-begin :after #'skip-dash-forward)
  ;; (advice-add 'evil-forward-word-end :before #'skip-dash-forward-end)
  ;; (advice-add 'evil-backward-word-begin :before #'skip-dash-backward)
  ;; (advice-remove 'evil-forward-word-begin #'skip-dash-forward)
  ;; (advice-remove 'evil-forward-word-end #'skip-dash-forward-end)
  ;; (advice-remove 'evil-backward-word-begin #'skip-dash-backward)
  )

(use-package evil-collection
  :after evil

  :straight
  (evil-collection
   :local-repo "~/.emacs.d/my-packages/evil-collection"
   :type git)

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
  (evil--add-to-alist
   evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-textobj-line)

(use-package evil-textobj-entire
  :config
  (define-key evil-outer-text-objects-map
    evil-textobj-entire-key
    #'evil-entire-entire-buffer)
  (define-key evil-inner-text-objects-map
    evil-textobj-entire-key
    #'evil-entire-entire-buffer))

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

(use-package pubsub
  :straight
  (pubsub
   :local-repo "~/.emacs.d/my-packages/pubsub"
   :type git)
  :config)

(use-package mantra
  :after pubsub
  :straight
  (mantra
   :local-repo "~/.emacs.d/my-packages/mantra"
   :type git)
  :config
  (mantra-connect))

(use-package virtual-ring
  :straight
  (virtual-ring
   :local-repo "~/.emacs.d/my-packages/virtual-ring"
   :type git)
  :config)

(use-package repeat-ring
  :after (mantra virtual-ring)
  :straight
  (repeat-ring
   :local-repo "~/.emacs.d/my-packages/repeat-ring"
   :type git))

(defun my-company-complete-to-selection ()
  "Insert the selected candidate but retain the completion menu."
  (interactive)
  (when (and (company-manual-begin) company-selection)
    (let ((result (nth company-selection company-candidates)))
      (if (and (not (cdr company-candidates))
               (equal result (car company-candidates)))
          (company-complete-selection)
        (company--insert-candidate result)))))

;; desired behavior:
;; "company complete common strict or cycle with preview"
;; abc {abc abcdef abcdefg}
;; tab -> abc
;; tab -> abcdef
;; tab -> abcdefg
;; abc {abcd abcdef abcdefg}
;; tab -> abcd
;; tab -> abcdef
;; tab -> abcdefg
;; So on Tab:
;; First complete common. *If already max common, then do nothing.*
;; Then complete to first match.
;; Then cycle to next match.
;; Also, ideally, cycling should insert the candidate in the buffer so that
;; we can just hit spacebar to continue typing, instead of hitting Enter first
;; First get it to work without this requirement since it might be that
;; inserting it into the buffer would lose the company completions list a priori
;; maybe this link for inspiration on how to write the completion function:
;; https://gist.github.com/rswgnu/85ca5c69bb26551f3f27500855893dbe
;; Another possibility is to modify complete-common-or-cycle just to add
;; the strict behavior of doing nothing the first time if already at max common
;; Yes, the latter is probably simplest, and acceptable.
;; Also:
;; it would be great if recent candidates containing _any part_ of the typed word
;; show up near the top. This is fuzzy matching I believe.
;; So, recent+fuzzy prioritized over prefix, but no fuzzy matching aside from
;; recent ones.
(use-package company
  :init
  (setq company-require-match nil)            ; Don't require match, so you can still move your cursor as expected.
  (setq company-tooltip-align-annotations t)  ; Align annotation to the right side.
  (setq company-dabbrev-downcase nil)         ; Don't downcase completion candidates
  (setq company-dabbrev-ignore-case nil)      ; Consider prefix case significant
  (setq company-dabbrev-other-buffers nil)    ; Doom config suggests this prevents lag with many open buffers
  :bind (:map company-active-map
              ("<tab>" . #'company-complete-common)
              ("S-<tab>" . #'my-company-complete-to-selection))
  :config
  ;; enable company mode autocompletion in all buffers
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)      ; show completions when 2 characters typed
  ;; When multiple backends return completion candidates, they are deduplicated
  ;; based on both the completion string itself as well as any annotations by
  ;; the backend. This means that the completion set may include duplicates if
  ;; two backends find the same completion but one includes metadata and the
  ;; other doesn't, which isn't what we want. Since the backends are already
  ;; ordered in terms of decreasing specificity, we get the results we want
  ;; by favoring the first result whenever there are such duplicates.
  ;; See https://github.com/company-mode/company-mode/issues/528 for more info
  (add-to-list 'company-transformers #'delete-consecutive-dups)
  ;; company-capf seems to block succeeding backends even if it doesn't have a match?
  ;; this could mean that oddmuse and dabbrev are never going to be hit
  ;; use company-diag at point to debug
  (setq company-backends
        '(uucompany-bbdb company-eclim company-semantic company-clang company-xcode
                         company-cmake company-files
                         (company-capf company-dabbrev-code company-gtags company-etags company-keywords)
                         company-oddmuse company-dabbrev))

  ;; From @xgqt, show yasnippets in completions:
  (setq company-backends
        (mapcar (lambda (backend)
                  (if (and (listp backend)
                           (member 'company-yasnippet backend))
                      backend
                    (append (if (consp backend) backend (list backend))
                            '(:with company-yasnippet))))
                company-backends))

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

(use-package company-prescient
  :config
  (company-prescient-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

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
  (setq undo-tree-auto-save-history nil) ; seems to default to t in recent versions
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

(require 'my-navigation)

;; highlight matching paren
(show-paren-mode 1)

;; ASCII emojis from https://gitlab.com/fommil/dotfiles/blob/master/.emacs.d/init.el
(defun my-insert-shruggie ()
  "Insert ASCII shrug dude."
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun my-insert-table-flip ()
  "Insert an ASCII table flip."
  (interactive)
  (insert "(╯°□°）╯︵ ┻━┻)"))

(global-set-key (kbd "C-c m") #'recompile)
