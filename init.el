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
  (setq epistemic-mode t)
  (global-set-key (kbd "s-m") 'hydra-mode/body)
  (global-set-key (kbd "s-<escape>") 'hydra-mode/body) ;; TODO: s-esc and s-ret should operate based on meta-towers
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name)
                                     "-hook"))))
      (add-hook mode-hook (lambda ()
                            (setq eem--current-tower-index 2)
                            (setq eem--current-level 2))))))

;; load any customizations done via EMACS UI
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
