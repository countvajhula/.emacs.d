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
    (normal-top-level-add-subdirs-to-load-path)))

(setup-load-path)

;; initialize package managers and installed packages
(load "setup")
;; load any local general-purpose utilities
(load "utils")

;; load core packages used in other packages
(load "core")

;; load all configured packages
(load "task-environment")
(load "project-environment")
(load "application-environment")
(load "system-environment")
(load "network-environment")
(load "physical-environment")

(use-package rigpa
  ;; TODO: should be independent of symex and arguably evil too
  ;; and, eventually, non-essential modes should be removed from
  ;; the core into separate packages

  :straight
  (rigpa
   :local-repo "~/.emacs.d/my-packages/rigpa"
   :type git)

  :config
  (rigpa-mode 1)

  (global-set-key (kbd "s-n") 'evil-normal-state)
  (global-unset-key (kbd "s-m"))
  (global-set-key (kbd "s-m s-m") 'rigpa-flashback-to-last-tower)
  (global-set-key (kbd "C-<escape>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-complex)
                      ;; ensure that we leave mode mode prior to entering
                      ;; tower mode - this is a temporary hack until
                      ;; we introduce metaspace coordinates
                      (rigpa-exit-mode-mode))
                    (rigpa-enter-tower-mode)))
  (global-set-key (kbd "M-<escape>") 'rigpa-enter-mode-mode) ;; TODO: s-esc and s-ret should operate based on a structure, shouldn't be hardcoded
  (global-set-key (kbd "s-<escape>") 'rigpa-enter-mode-mode)
  (global-set-key (kbd "M-<return>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-complex)
                      ;; only exit mode mode if we are actually in it
                      (rigpa-enter-selected-level)
                      (let ((ground (rigpa--get-ground-buffer)))
                        (rigpa-exit-mode-mode)
                        (switch-to-buffer ground)))))
  (global-set-key (kbd "s-<return>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-complex)
                      ;; only exit mode mode if we are actually in it
                      (rigpa-enter-selected-level)
                      (let ((ground (rigpa--get-ground-buffer)))
                        (rigpa-exit-mode-mode)
                        (switch-to-buffer ground)))))
  (global-set-key (kbd "C-<return>")
                  (lambda ()
                    (interactive)
                    (when (eq rigpa--complex rigpa-meta-tower-complex)
                      ;; only exit tower mode if we are actually in it
                      (rigpa-exit-tower-mode)
                      (rigpa-enter-mode-mode))))
  ;; index entry to various modes
  (global-set-key (kbd "s-w")        ; window mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "window")))
  (global-set-key (kbd "s-v")        ; view mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "view")))
  (global-set-key (kbd "s-x")        ; char mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "char")))
  (global-set-key (kbd "s-a")        ; activity mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "activity")))
  (global-set-key (kbd "s-z")        ; text mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "text")))
  (global-set-key (kbd "s-g")        ; history mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "history")))
  (global-set-key (kbd "s-i")        ; system mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "system")))
  (global-set-key (kbd "s-b")        ; buffer mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "buffer")))
  (global-set-key (kbd "s-k")        ; also buffer, to use the right hand more
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "buffer")))
  (global-set-key (kbd "s-f")        ; file mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "file")))
  (global-set-key (kbd "s-t")        ; tab mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "tab")))
  (global-set-key (kbd "s-l")        ; line mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "line")))
  (global-set-key (kbd "s-e")        ; application mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "application")))
  (global-set-key (kbd "s-r")        ; word mode
                  (lambda ()
                    (interactive)
                    (rigpa-enter-mode "word"))))

(setq native-comp-async-report-warnings-errors 'silent)

;; load any customizations done via EMACS UI
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
