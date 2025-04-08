(require 'constants)

;; convenient list- and functional-related macros
(use-package dash)

;; convenient dict-related macros
(use-package ht)

;; intuitive "state machine" menus
(use-package hydra)

(use-package macrostep)

(use-package general
  ;; general is a package that provides various
  ;; resources and utilities for defining keybindings
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)

  (defhydra hydra-unicode (:columns 4
                           :exit t)
    "Unicode characters"
    ("y" (lambda () (interactive) (insert-char #x262f)) "☯"))

  (defhydra hydra-leader (:timeout my-leader-timeout
                          :columns 2
                          :exit t)
    "Quick actions"
    ("a" org-agenda "Org agenda")
    ("c" hydra-unicode/body "Unicode characters")
    ("n" (lambda ()
           (interactive)
           (save-excursion
             (execute-kbd-macro (kbd ":%s///gn<return>"))))
     "count search results") ; this gets buried
    ("d" dictionary-lookup-definition "lookup in dictionary")
    ("f" my-current-dir "dir")
    ("g" magit-status "Magit (git)")
    ("l" my-lisp-repl "Lisp REPL")
    ("m" my-switch-to-messages-buffer "Go to Messages buffer")
    ("o" sunrise-cd "Sunrise Commander")
    ("p" company-mode "Toggle Company completions")
    ("s" my-shell "Shell")
    ("r" ripgrep-regexp "recursive search")
    ("t" dired-sidebar-toggle-sidebar "Nav Sidebar")
    ("u" undo-tree-visualize "Undo tree"))

  ;; define global vim-style "leader" key
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-leader/body)
  (general-define-key
   :states '(insert emacs)
   :keymaps 'override
   "C-c SPC" 'hydra-leader/body))

;; zoom entire frame including status bar (works by modifying font faces)
(use-package zoom-frm)

(use-package dictionary
  :defer t)

(use-package etymology-of-word
  :straight
  (etymology-of-word :local-repo "~/.emacs.d/my-packages/etymology-of-word" :type nil))

;; mark columns visually
(use-package column-marker
  :straight
  (column-marker :local-repo "~/.emacs.d/my-packages/column-marker" :type nil))

(use-package auto-dim-other-buffers)

(require 'my-python)

;; to discover Elisp functions by providing examples
(use-package suggest)

(require 'my-elisp)

(require 'my-scheme)

(require 'my-racket)

(use-package clojure-ts-mode)

(use-package mindstream
  :straight
  (mindstream
   :local-repo "~/.emacs.d/my-packages/mindstream"
   :type git)

  :custom
  (mindstream-path
   (concat (file-name-as-directory user-home-directory)
           "tmp/mindstream"))
  (mindstream-save-session-path
   (concat (file-name-as-directory user-home-directory)
           "log/mindstream"))
  (mindstream-archive-path
   (concat (file-name-as-directory user-home-directory)
           "log/archive"))
  (mindstream-live-action '(racket-mode racket-run))
  (mindstream-preferred-template '(racket-mode "racket"))
  (mindstream-persist t)
  (mindstream-unique nil)

  :config
  (mindstream-mode)

  (advice-add 'mindstream-new
              :after
              (lambda (&rest _args)
                ;; Ignore whatever `racket-repl-buffer-name-function' just did to
                ;; set `racket-repl-buffer-name' and give this its own REPL.
                (setq-local racket-repl-buffer-name "*scratch - Racket REPL*"))))

(use-package my-scribble
 :after (racket-mode evil)
 :straight
 (my-scribble
  :local-repo "~/.emacs.d/my-packages/my-scribble"
  :type nil))

(require 'my-haskell)

(require 'my-rst)

(require 'my-markdown)

(require 'my-texinfo)

(use-package cider
  :defer t)

(use-package slime
  :defer t
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  (setq slime-auto-start 'always))

(use-package dynaring
  :straight
  (dynaring
   :local-repo "~/.emacs.d/my-packages/dynaring"
   :type git))

(use-package buffer-ring
  :after dynaring
  :straight
  (buffer-ring
   :local-repo "~/.emacs.d/my-packages/buffer-ring"
   :type git))

(use-package arc
  :defer t
  :straight
  (arc
   :type git
   :host github
   :repo "countvajhula/arc.el")
  :config
  (setq arc-source-path "~/work/lisp/arc/arc3.2"))

(use-package lithium
  :straight
  (lithium
   :local-repo "~/.emacs.d/my-packages/lithium"
   :type git)
  :config
  (lithium-mode))

(defvar my-elisp-modes '(lisp-interaction-mode
                         emacs-lisp-mode
                         inferior-emacs-lisp-mode))

(defun my-local-leader ()
  "Launch local leader menu."
  (interactive)
  (let ((leader
         ;; TODO: maybe use a hash
         (cond ((eq major-mode 'racket-mode) 'hydra-racket/body)
               ((memq major-mode my-elisp-modes) 'hydra-elisp/body)
               ((eq major-mode 'scheme-mode) 'hydra-scheme/body))))
    (call-interactively leader)))

(defun my-global-leader ()
  "Launch global leader menu"
  (interactive)
  (hydra-leader/body))

(use-package symex
  :after (evil lithium)
  :straight
  (symex
   :local-repo "~/.emacs.d/my-packages/symex"
   :type git)
  :custom
  (symex-modal-backend 'evil)
  (symex-quote-prefix-list (list "'" "#'" "`" "#`" "#"))
  (symex-unquote-prefix-list (list "," "#," ",@" "#,@"))
  :config
  (symex-initialize)
  (global-set-key (kbd "s-y") #'symex-mode-interface) ; since y looks like inverted lambda
  (global-set-key (kbd "s-;") (kbd "s-y")) ; since y is hard to reach
  (define-key
   symex-mode-map
   (kbd "C-w")
   (lambda ()
     (interactive)
     (execute-kbd-macro (kbd "("))))
  (add-to-list 'Info-directory-list
               (expand-file-name "~/work/symex/doc/"))
  (lithium-define-keys symex-editing-mode
                       (("u" evil-undo)
                        ("C-r" evil-redo)
                        ("\"" evil-use-register)
                        ("." evil-repeat)
                        ("q" evil-record-macro)
                        ("@" evil-execute-macro)
                        ("m" evil-set-marker)
                        ("'" evil-goto-mark-line)
                        ("/" evil-search-forward)
                        ("?" evil-search-backward)
                        ("#" evil-search-word-backward)
                        ("*" evil-search-word-forward)
                        ("n" evil-search-next)
                        ("N" evil-search-previous)
                        ("M" my-switch-to-messages-buffer)
                        ("t" mindstream-enter-anonymous-session)
                        ("C-d" evil-scroll-down)
                        ("C-u" evil-scroll-up)
                        ("C-e" my-scroll-down)
                        ("C-y" my-scroll-up)
                        ("C-]" evil-jump-to-tag)
                        ("C-i" evil-jump-forward)
                        ("C-o" evil-jump-backward)
                        ("\\" my-local-leader)
                        ("SPC" my-global-leader))))

(use-package php-mode
  :defer t)

(require 'my-latex)

;; ido mode
(use-package ido
  ;; disabled since using ivy
  :disabled t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

(use-package sublimity
  :disabled t
  :config
  (sublimity-mode 1))

(use-package minimap
  :disabled t)

(use-package ivy
  ;; company is for in-buffer auto-completion,
  ;; ivy is for application-level on-demand completion
  :config
  (ivy-mode 1)
  ;; use fuzzy-style matching in all cases except swiper (from SX)
  ;; (setq ivy-re-builders-alist
  ;; 	'((swiper . ivy--regex-plus)
  ;; 	  (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)

  ;; don't wait for extra confirmation on matches
  (define-key
   ivy-minibuffer-map
   (kbd "TAB")
   'ivy-alt-done))

(use-package selectrum
  :disabled t
  :after ivy counsel ; just in case it needs to set/unset any config
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :disabled t
  :after prescient
  :config
  (selectrum-prescient-mode 1))

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :bind ("C-c k" . counsel-unicode-char)
  :config
  (counsel-mode 1))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy-hydra)

(use-package ivy-rich
  :config
  (ivy-rich-mode t))

(use-package prescient
  :custom
  (prescient-history-length 512)
  :config
  (prescient-persist-mode t))

(use-package ivy-prescient
  :after (counsel prescient ivy)
  :config
  (ivy-prescient-mode t))

;; looks like smex (smart command history in M-x) is used by counsel just
;; by being installed, and doesn't need to be explicitly invoked here
(use-package smex
  ;; disabled since using ivy-prescient
  :disabled t
  :config
  (smex-initialize))

(use-package magit
  :custom
  (magit-bury-buffer-function #'magit-mode-quit-window)
  (magit-diff-refine-hunk 'all)
  :config
  ;; use side-by-side view for blame -- this doesn't work atm
  ;; (setq magit-blame--style (nth 1 magit-blame-styles))
  (define-key (current-global-map)
              (kbd "C-x g")
              'magit-status)
  (define-key (current-global-map)
              (kbd "C-x M-g")
              'magit-dispatch-popup))

(use-package git-timemachine)

(use-package tabbar
  :disabled t
  :config
  ;; turn on the tabbar
  (tabbar-mode t)
  (define-key (current-global-map)
              (kbd "s-{")
              'tabbar-backward)
  (define-key (current-global-map)
              (kbd "s-}")
              'tabbar-forward))

(use-package evil-tabs
  :disabled t
  :config
  (global-evil-tabs-mode t))

;; NOTE: initially, you need to run M-x all-the-icons-install-fonts
;; in order for the fonts to appear
(use-package all-the-icons)

(use-package centaur-tabs
  :after rigpa
  :demand
  :custom
  (centaur-tabs-style "bar")
  ;; (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (setq x-underline-at-descent-line t)
  (centaur-tabs-change-fonts "monaco" 130)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match) ; so that the unused portion of the "headline" matches the used portion
  :bind
  ;; note that these are hardcoded to the s-t binding for tab mode
  ;; could be better to define a global rigpa modes entry bindings
  ;; lookup table that is used everywhere
  ("s-{" . (lambda ()
             (interactive)
             (execute-kbd-macro (kbd "s-t h <escape>"))))
  ("s-}" . (lambda ()
             (interactive)
             (execute-kbd-macro (kbd "s-t l <escape>")))))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; (use-package sunrise-commander
;;   :after evil
;;   :straight
;;   (sunrise-commander
;;    :type git
;;    :host github
;;    :repo "sunrise-commander/sunrise-commander")
;;   :config
;;   ;; tab in sunrise commander should alternate pane
;;   ;; TODO: add this to evil-collection eventually,
;;   ;; incl. / as its behavior in sr-mode
;;   ;;
;;   ;; this (below) doesn't enter normal state for some reason,
;;   ;; probably related to why evil-collection doesn't
;;   ;; set the initial state as normal, either
;;   ;; (add-hook 'sr-mode-hook #'evil-normal-state)

;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'sr-mode-map
;;    "<tab>" 'sr-change-window)
;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'sr-mode-map
;;    "/" 'sr-goto-dir)
;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'sr-mode-map
;;    "y" 'sr-synchronize-panes)
;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'sr-mode-map
;;    "<backspace>" 'sr-dired-prev-subdir)
;;   (general-define-key
;;    :states '(normal visual motion)
;;    :keymaps 'sr-mode-map
;;    "q" 'sr-quit))

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 25))

(use-package popwin
  :disabled t
  :config
  (popwin-mode t))

(use-package ibuffer
  ;; replace oldschool buffer-list, as recommended here:
  ;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)))
  ;;(ibuffer-switch-to-saved-filter-groups "default"))))
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-vc
  ;; organize buffers by version-controlled repo
  ;; note: there's projectile-ibuffer
  :disabled t
  :init
  ;; TODO: C-k doesn't go up filter groups as expected
  ;; (add-hook 'ibuffer-hook
  ;;           (lambda ()
  ;;             (define-key
  ;;               (current-local-map)
  ;;               (kbd "C-k")
  ;;               'ibuffer-backward-filter-group)))
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-sidebar
  :disabled t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :bind ("C-c b" . ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

(use-package eldoc-box
  :config
  (add-hook 'eldoc-box-buffer-hook #'my-disable-line-numbers))

(use-package smart-mode-line
  :disabled t
  :config
  (sml/setup)
  (setq sml/theme 'dark))

(use-package telephone-line
  :after rigpa
  :config
  ;; define faces for rigpa modes
  ;; (list-colors-display)
  (defface telephone-line-evil-char
    '((t (:background "forest green" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Char state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-word
    '((t (:background "lime green" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Word state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-line
    '((t (:background "sea green" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Line state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-symex
    '((t (:background "SlateBlue3" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Symex state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-view
    '((t (:background "light sea green" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in View state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-window
    '((t (:background "medium aquamarine" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Window state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-file
    '((t (:background "dark turquoise" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in File state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-buffer
    '((t (:background "turquoise" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Buffer state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-application
    '((t (:background "powder blue" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Application state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-system
    '((t (:background "light sky blue" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in System state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-activity
    '((t (:background "spring green" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Activity state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-text
    '((t (:background "cyan" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Text state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-tab
    '((t (:background "black" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Tab state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-history
    '((t (:background "powder blue" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in History state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-mode
    '((t (:background "firebrick" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Mode state."
    :group 'telephone-line-evil)

  (defface telephone-line-evil-tower
    '((t (:background "blue" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Tower state."
    :group 'telephone-line-evil)

  (telephone-line-mode t))

;; cozy time
(use-package fireplace)

;; virtual caps lock since actual one is remapped to Esc
(use-package caps-lock)

;; so you don't lose the cursor
(use-package beacon
  :config
  (beacon-mode 1))

(use-package subed
  :defer t
  :straight
  (subed
   :type git
   :host github
   :repo "sachac/subed"
   :files ("subed/*.el"))

  :config
  ;; Disable automatic movement of point by default
  (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
  ;; Remember cursor position between sessions
  (add-hook 'subed-mode-hook 'save-place-local-mode)
  ;; Break lines automatically while typing
  (add-hook 'subed-mode-hook 'turn-on-auto-fill)
  ;; Break lines at 40 characters
  (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40))))

(require 'my-familiar)

(require 'my-general-behavior)

(require 'my-look-and-feel)


;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; map Mac's Command key to Emacs/Lisp's Super key
(setq mac-command-modifier 'super)
;; make Fn key do Hyper [coz, why not]
(setq mac-function-modifier 'hyper)

; Note: "define-key (current-global-map)" is the same as global-set-key

;; default "emacs leader" is \, but rebinding that (Vim default) key
;; to be a local leader for different modes (like python) since that's
;; more useful. Move emacs leader to s-\ instead
(global-set-key (kbd "s-\\") 'evil-execute-in-emacs-state)

(define-key
 ;; info on the current buffer
 (current-global-map)
 (kbd "C-c b")
 'my-buffer-info)

(define-key
 ;; info on the current region
 (current-global-map)
 (kbd "C-c g")
 'count-words-region)

(define-key
 ;; navigation sidebar
 (current-global-map)
 (kbd "C-c t")
 'dired-sidebar-toggle-sidebar)

(define-key
 ;; open a new empty buffer
 (current-global-map)
 (kbd "C-c n")
 'my-new-empty-buffer)

(define-key
 ;; drop into a shell (preserves path)
 (current-global-map)
 (kbd "C-c s")
 'my-shell)

(define-key
 ;; lookup in dictionary
 (current-global-map)
 (kbd "C-c d")
 'dictionary-lookup-definition)

(define-key
 ;; open an elisp shell
 (current-global-map)
 (kbd "C-c l")
 'my-lisp-repl)

(define-key
 ;; emulate caps lock -- alternative to an actual CAPS LOCK key
 (current-global-map)
 (kbd "H-<escape>")
 'caps-lock-mode)

(define-key
 ;; calculator mode
 (current-global-map)
 (kbd "C-+")
 'calc)
