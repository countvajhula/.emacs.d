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

(defun my-bootstrap-elacarte (recipes-file)
  "Directly read the recipe for elacarte and install it.

RECIPES is a file containing a list of recipes. Reading the recipes.eld
files directly is necessary because a recipe repository that could be
used by Straight doesn't yet exist."
  (unless (file-exists-p recipes-file)
    (user-error "Recipes file %s not found!" recipes-file))
  (let* ((recipes (with-temp-buffer
                    (insert-file-contents-literally recipes-file)
                    ;; Use `read-from-string` to handle empty files gracefully.
                    (car (read-from-string (buffer-string)))))
         (recipe (alist-get 'elacarte recipes)))
    (unless recipe
      (user-error "Elacarte recipe missing in %s!" recipes-file))
    (straight-use-package (cons 'elacarte recipe))))

(my-bootstrap-elacarte
 (expand-file-name "elacarte/recipes.eld"
                   user-emacs-directory))

;; --- Add the local "xelpa" recipe repository ---
(require 'elacarte)
(elacarte-build-recipe-repository)
(elacarte-register-recipe-repository)

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
