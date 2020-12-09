;;;;;;;;;;;;;;;;;;;
;; INITIAL SETUP ;;
;;;;;;;;;;;;;;;;;;;

;; bootstrap straight.el (package manager)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; 'require' looks in the load-path, so packages need to be
;; downloaded from melpa prior to this.
;; package.el, though, is bundled with emacs
(require 'package)

;; add some standard package repos with lots of non-bundled goodies
(setq my-package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")))
(setq package-archives (append package-archives
                               my-package-archives))

;; (setq use-package-always-defer t)

;; set up el-get, e.g. for packages not on ELPA/MELPA
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
