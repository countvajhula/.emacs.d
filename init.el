(defun setup-load-path ()
  "Set up load path for the initialization process"
  (setq user-home-directory (getenv "HOME"))
  (setq user-customizations-directory (concat user-emacs-directory
                                              "my-init/"))
  (setq user-packages-directory (concat user-emacs-directory
                                        "my-packages/"))
  (add-to-list 'load-path user-customizations-directory)
  (add-to-list 'load-path user-packages-directory)
  ;; add all its subfolders too
  (let ((default-directory user-packages-directory))
    (normal-top-level-add-subdirs-to-load-path))
  ;; el-get -- an alternative package manager to ELPA/MELPA,
  ;; used for packages not on ELPA/MELPA
  (add-to-list 'load-path (concat user-emacs-directory
                                  "el-get/el-get")))

(setup-load-path)

;; initialize package managers and installed packages
(load "setup")
;; load any local general-purpose utilities
(load "utils")

;; load all configured packages
(load "task-environment")
(load "project-environment")
(load "application-environment")
(load "system-environment")
(load "network-environment")
(load "physical-environment")

(use-package evil-epistemic-mode
  :after (evil symex)  ;; TODO: should be independent of symex and arguably evil too
  :config
  ;; define towers
  (setq eem-complete-tower
        (ht ('name "complete")
            ('levels (list "insert"
                           "char"
                           "word"
                           "line"
                           "activity"
                           "normal"
                           "view"
                           "window"
                           "file"
                           "buffer"
                           "system"
                           "application"))))
  (setq eem-vim-tower
        (ht ('name "vim")
            ('levels (list "insert"
                           "normal"))))
  (setq eem-emacs-tower
        (ht ('name "emacs")
            ('levels (list "emacs"))))
  (setq eem-lisp-tower
        (ht ('name "lisp")
            ('levels (list "insert"
                           "symex"
                           "normal"))))
  (setq eem-towers
        (list eem-vim-tower
              eem-complete-tower
              eem-lisp-tower
              eem-emacs-tower))

  ;; evil interop keybindings
  (define-key evil-normal-state-map [escape] 'eem-enter-higher-level)
  (define-key evil-normal-state-map [return] 'eem-enter-lower-level)
  (define-key evil-insert-state-map [escape] 'eem-enter-higher-level)

  (setq epistemic-mode t)
  (global-set-key (kbd "s-m") 'hydra-tower/body)
  (global-set-key (kbd "s-<escape>") 'hydra-tower/body) ;; TODO: s-esc and s-ret should operate based on meta-towers
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook (lambda ()
                            (setq eem--current-tower-index 2)
                            (setq eem--current-level 2)))))
  ;; until arc is supported in symex mode
  (add-hook 'arc-mode-hook (lambda ()
                             (setq eem--current-tower-index 2)
                             (setq eem--current-level 2))))

;; load any customizations done via EMACS UI
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
