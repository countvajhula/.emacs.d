;;;;;;;;;;;;;;;;;;;
;; INITIAL SETUP ;;
;;;;;;;;;;;;;;;;;;;

;; bootstrap straight.el (package manager)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; --- Add the local "xelpa" recipe repository ---

;; 1. Tell straight.el to "install" the recipe repository package.
;;    The `:defer t` keyword prevents use-package from
;;    trying (and failing) to `require` a non-existent 'xelpa' feature.
(use-package xelpa
  :defer t
  :straight
  (xelpa
   :type nil
   :local-repo "~/.emacs.d/xelpa"
   :build nil))

;; 2. Manually add the path to the load-path and load the recipe protocol functions.
(add-to-list 'load-path "~/.emacs.d/xelpa")
(require 'xelpa)

;; 3. Now that the protocol is loaded, register "xelpa" as a recipe source.
(add-to-list 'straight-recipe-repositories 'xelpa)

;; 'require' looks in the load-path, so packages need to be
;; downloaded from melpa prior to this.
;; package.el, though, is bundled with emacs
(require 'package)

;; add some standard package repos with lots of non-bundled goodies
(setq my-package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")
                            ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
                            ("NonGNU ELPA"  . "https://elpa.nongnu.org/nongnu/")))
(setq package-archives (append package-archives
                               my-package-archives))

;; (setq use-package-always-defer t)

;; use Option key as Meta on mac
(setq mac-option-modifier 'meta)
;; map Mac's Command key to Emacs/Lisp's Super key
(setq mac-command-modifier 'super)
;; make Fn key do Hyper [coz, why not]
(setq mac-function-modifier 'hyper)
