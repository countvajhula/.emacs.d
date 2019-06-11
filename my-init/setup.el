;;;;;;;;;;;;;;;;;;;
;; INITIAL SETUP ;;
;;;;;;;;;;;;;;;;;;;


;; 'require' looks in the load-path, so packages need to be
;; downloaded from melpa prior to this.
;; package.el, though, is bundled with emacs
(require 'package)

;; add some standard package repos with lots of non-bundled goodies
(setq my-package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")))
(setq package-archives (append package-archives
                               my-package-archives))

;; initialize all "installed" packages
(package-initialize)
;; avoid extra call to (package-initialize) after loading init.el
(setq package-enable-at-startup nil)

;; bootstrap use-package, a macro to keep configuration
;; (e.g. in this file) organized
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; set up el-get, e.g. for packages not on ELPA/MELPA
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
