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

(use-package dictionary)

(use-package my-python
  :after general)

(use-package my-elisp
  :after general)

(use-package my-scheme
  :after general)

(use-package my-racket
  :after general)

(use-package php-mode
  :defer t)

(use-package haskell-mode
  :defer t)

(use-package tex
  :defer t
  :ensure auctex
  :config
  ;; make latexmk available via C-c C-c
  ;; (latexmk is a popular perl script to minimize number of
  ;; passes of compilation)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda ()
                              (setq TeX-command-default "latexmk")))
  ;; use Skim as pdf viewer
  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "open -a Skim.app %o")
          ("displayline" "displayline -g -b %n %o %b")
          ("open" "open %o"))
        TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "Skim")
          (output-html "open")))
  ;; Turn on RefTeX in AUCTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t))

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

(use-package company
  :config
  ;; enable company mode autocompletion in all buffers
  (setq company-idle-delay 0.2)
  (global-company-mode 1))

(use-package company-jedi)

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
  (setq ivy-wrap t))

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

;; looks like smex (smart command history in M-x) is used by counsel just
;; by being installed, and doesn't need to be explicitly invoked here
(use-package smex
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
  :after evil-epistemic-mode
  :config
  ;; define faces for epistemic modes
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

  (defface telephone-line-evil-mode
    '((t (:background "firebrick" :inherit telephone-line-evil)))
    "Face used in evil color-coded segments when in Mode state."
    :group 'telephone-line-evil)

  (telephone-line-mode t))

;; cozy time
(use-package fireplace)

;; virtual caps lock since actual one is remapped to Esc
(use-package caps-lock)


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
         (original-major-mode major-mode)
         ($buf (generate-new-buffer buffer-name)))
    (with-current-buffer $buf
      (funcall original-major-mode)
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
    ("s" eshell "Shell")
    ("t" sr-speedbar-toggle "Nav Sidebar")
    ("u" undo-tree-visualize "Undo tree"))

  ;; define global vim-style "leader" key
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-leader/body))

(use-package my-familiar
  :after evil)

(use-package my-general-behavior)

(use-package my-look-and-feel)


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
  ;; navigation sidebar
  (current-global-map)
  (kbd "C-c t")
  'sr-speedbar-toggle)

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
  (kbd "C-<escape>")
  'caps-lock-mode)

(define-key
  ;; calculator mode
  (current-global-map)
  (kbd "C-+")
  'calc)
