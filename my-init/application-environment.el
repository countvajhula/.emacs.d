;; vim-style local leader key for mode-specific
;; (e.g. python-specific) functionality
(defvar my-local-leader "\\")
(defvar my-leader-timeout 2.0)

;; convenient list- and functional-related macros
(use-package dash)

;; convenient dict-related macros
(use-package ht)

;; used in some (third-party) custom themes
;; (not sure if there's a better way to indicate this dependency)
(use-package autothemer)

;; intuitive "state machine" menus
(use-package hydra)

;; zoom entire frame including status bar (works by modifying font faces)
(use-package zoom-frm)

(use-package dictionary)

(use-package etymology-of-word
  :straight
  (etymology-of-word :local-repo "~/.emacs.d/my-packages/etymology-of-word" :type nil))

;; mark columns visually
(use-package column-marker
  :straight
  (column-marker :local-repo "~/.emacs.d/my-packages/column-marker" :type nil))

(use-package my-python
  :after general
  :straight
  (my-python :local-repo "~/.emacs.d/my-packages/my-python" :type nil)
  :config
  (add-hook 'python-mode-hook
            (lambda () (show-paren-mode 1)))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq python-indent-offset 4)))
  (add-hook 'python-mode-hook
            (lambda () (setq fill-column 79))))

(use-package my-elisp
  :after general

  :straight
  (my-elisp :local-repo "~/.emacs.d/my-packages/my-elisp" :type nil))

(use-package my-scheme
  :after general

  :straight
  (my-scheme :local-repo "~/.emacs.d/my-packages/my-scheme" :type nil))

(use-package my-racket
  :after general

  :straight
  (my-racket :local-repo "~/.emacs.d/my-packages/my-racket" :type nil))

(use-package cider
  :defer t)

(use-package slime
  :defer t
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  (setq slime-auto-start 'always))

(use-package arc
  :defer t
  :straight
  (arc
   :type git
   :host github
   :repo "countvajhula/arc.el")
  :config
  (setq arc-source-path "~/work/lisp/arc/arc3.2"))

(use-package symex
  :straight
  (symex
   :local-repo "~/.emacs.d/my-packages/symex"
   :type nil)
  :config
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook 'symex-mode)))
  (symex-hide-menu))

(use-package php-mode
  :defer t)

(use-package haskell-mode
  :defer t)

(use-package my-latex
  :after general

  :straight
  (my-latex :local-repo "~/.emacs.d/my-packages/my-latex" :type nil))

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
  :config
  (prescient-persist-mode t)
  (setq prescient-history-length 256))

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

;; not sure why this is necessary, but this initializes sunrise
;; commander, along with all of its extensions
(el-get-bundle sunrise-commander)

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

;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;


(defun my-count-lines-page ()
  "Modified from emacs's built-in count-lines-page to return a list of
   values corresponding to the position in the page."
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	    before (count-lines beg opoint)
	    after (count-lines opoint end))
      (list total before after))))

(defun my-buffer-info ()
  "get info on current buffer -- similar to Vim's C-g"
  (interactive)
  (-let [(total before after) (my-count-lines-page)]
    (if (= total 0)
	(setq bufinfo (list "-- No lines in buffer --"))
      (progn (setq percentage (floor (* (/ (float before)
					   total)
					100)))
	     (setq page-position (concat
				  "-- "
				  (number-to-string percentage)
				  "%"
				  " --"))
	     (setq total-lines (concat
				(number-to-string total)
				" lines"))
	     (setq bufinfo (list total-lines page-position))))
    (add-to-list 'bufinfo
		 (buffer-file-name))
    (message "%s" (string-join bufinfo " "))))

(cl-defun my-new-empty-buffer (&optional
                               buffer-name
                               major-mode-to-use
                               &key
                               switch-p)
  "Create a new empty buffer.

If BUFFER-NAME is not provided, the new buffer will be named
“untitled” or “untitled<2>”, “untitled<3>”, etc. The buffer will be
created in the currently active (at the time of command execution)
major mode.
If SWITCH-P is true, switch to the newly created buffer.

Modified from:
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let* ((buffer-name (or buffer-name "untitled"))
         (major-mode-to-use (or major-mode-to-use major-mode))
         ($buf (generate-new-buffer buffer-name)))
    (with-current-buffer $buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save t))
    (when switch-p
      (switch-to-buffer $buf))
    $buf))

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

  (defhydra hydra-leader (:timeout my-leader-timeout
                          :columns 2
                          :exit t)
    "Quick actions"
    ("a" org-agenda "Org agenda")
    ("d" dictionary-lookup-definition "lookup in dictionary")
    ("f" my-current-dir "dir")
    ("g" magit-status "Magit (git)")
    ("l" my-lisp-repl "Lisp REPL")
    ("m" my-switch-to-messages-buffer "Go to Messages buffer")
    ("s" eshell "Shell")
    ("t" dired-sidebar-toggle-sidebar "Nav Sidebar")
    ("u" undo-tree-visualize "Undo tree"))

  ;; define global vim-style "leader" key
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-leader/body))

(use-package my-familiar
  :after evil

  :straight
  (my-familiar :local-repo "~/.emacs.d/my-packages/my-familiar" :type nil))

(use-package my-general-behavior

  :straight
  (my-general-behavior :local-repo "~/.emacs.d/my-packages/my-general-behavior" :type nil))

(use-package my-look-and-feel

  :straight
  (my-look-and-feel :local-repo "~/.emacs.d/my-packages/my-look-and-feel" :type nil))


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
  'eshell)

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
