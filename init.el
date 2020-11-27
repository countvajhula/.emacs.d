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

  ;; custom config
  (setq epistemic-show-menus nil)

  (global-set-key (kbd "s-n") 'evil-normal-state)
  (global-set-key (kbd "C-<escape>") 'my-enter-tower-mode)
  (global-set-key (kbd "M-<escape>") 'my-enter-mode-mode) ;; TODO: s-esc and s-ret should operate based on a structure, shouldn't be hardcoded
  (global-set-key (kbd "M-<return>")
                  (lambda ()
                    (interactive)
                    (eem-enter-selected-level)
                    (let ((ground (eem--get-ground-buffer)))
                      (my-exit-mode-mode)
                      (switch-to-buffer ground))))
  (global-set-key (kbd "C-<return>")
                  (lambda ()
                    (interactive)
                    (my-exit-tower-mode)
                    (my-enter-mode-mode)))
  ;; index entry to various modes
  (global-set-key (kbd "s-y")        ; symex mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "symex"))) ; since y looks like inverted lambda
  (global-set-key (kbd "s-;") (kbd "s-y")) ; since y is hard to reach
  ;; TODO: probably add the symex config under symex once hooks are added
  (global-set-key (kbd "s-w")        ; window mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "window")))
  (global-set-key (kbd "s-v")        ; view mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "view")))
  (global-set-key (kbd "s-x")        ; char mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "char")))
  (global-set-key (kbd "s-a")        ; activity mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "activity")))
  (global-set-key (kbd "s-z")        ; text mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "text")))
  (global-set-key (kbd "s-g")        ; history mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "history")))
  (global-set-key (kbd "s-i")        ; system mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "system")))
  (global-set-key (kbd "s-b")        ; buffer mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "buffer")))
  (global-set-key (kbd "s-f")        ; file mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "file")))
  (global-set-key (kbd "s-t")        ; tab mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "tab")))
  (global-set-key (kbd "s-l")        ; line mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "line")))
  (global-set-key (kbd "s-e")        ; application mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "application")))
  (global-set-key (kbd "s-r")        ; word mode
                  (lambda ()
                    (interactive)
                    (eem-enter-mode "word")))

  ;; until arc is supported in symex mode
  (add-hook 'arc-mode-hook (lambda ()
                             (setq eem--current-tower-index 2)
                             (setq eem--current-level 2))))

;; load any customizations done via EMACS UI
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
