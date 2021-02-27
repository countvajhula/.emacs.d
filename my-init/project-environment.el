;; navigation sidebar
(use-package sr-speedbar
  :disabled t)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package vscode-icon
  :disabled t)

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  ;(setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-theme 'icons)
  ;; (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-custom-font t))

(use-package ripgrep)

;; handy project-related functions like grep search, find file, etc.
(use-package projectile
  :after hydra
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-indexing-method 'native) ; native/alien/hybrid
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; for sublime emulation:
  ;; (define-key projectile-mode-map
  ;;             (kbd "s-p")
  ;;             'projectile-find-file)
  (defhydra hydra-projectile (:color teal
                              :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_f_: file            _s_: search (grep)     _i_: Ibuffer           _c_: cache clear
_e_: file dwim       _r_: replace           _b_: switch to buffer  _x_: remove known project
_l_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
_t_: recent file     _j_: jump to tag                            ^^^^_z_: cache current
_d_: dir             _g_: update gtags

"
    ("b"        projectile-switch-to-buffer)
    ("c"        projectile-invalidate-cache)
    ("d"        projectile-find-dir)
    ("e"        projectile-find-file-dwim)
    ("f"        projectile-find-file)
    ("s-f"      projectile-find-file)
    ("l"        projectile-find-file-in-directory)
    ("j"        projectile-find-tag)
    ("g"        ggtags-update-tags)
    ("s-g"      ggtags-update-tags)
    ("i"        projectile-ibuffer)
    ("K"        projectile-kill-buffers)
    ("s-k"      projectile-kill-buffers)
    ("m"        projectile-multi-occur)
    ("o"        projectile-multi-occur)
    ("s-p"      projectile-switch-project "switch project")
    ("p"        projectile-switch-project)
    ("r"        projectile-replace)
    ("s"        projectile-ripgrep)
    ("s-s"      projectile-ripgrep)
    ("t"        projectile-recentf)
    ("x"        projectile-remove-known-project)
    ("X"        projectile-cleanup-known-projects)
    ("z"        projectile-cache-current-file)
    ("`"        hydra-projectile-other-window/body "other window")
    ("q"        nil "cancel" :color blue)
    ("<escape>" nil :color blue))

  (define-key (current-global-map)
              (kbd "s-p")
              'hydra-projectile/body)
  (define-key (current-global-map)
              (kbd "s-F")
              'projectile-grep))

(use-package org-ibuffer
  :straight
  (org-ibuffer :local-repo "~/.emacs.d/my-packages/org-ibuffer" :type nil))

(use-package org
  :after (org-ibuffer hydra)
  :config
  ;; visually indent lower hierarchy levels
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/log/org/"))
  (setq org-default-notes-file "~/log/org/organizer.org")
  (setq org-todo-keywords '((sequence "[ ](t)"    ; todo
                                      "[\\](w)"   ; working / in-progress
                                      "[o](o)"    ; blocked / waiting
                                      "[-](-)"    ; won't do / invalid
                                      "[x](x)"))) ; done
  (setq org-tag-alist '(("@SF" . ?s)
                        ("@Oakland" . ?o)
                        ("raMP" . ?r)))

  ;; From: https://www.reddit.com/r/emacs/comments/7wsnoi/using_countdown_timers_for_alerts/
  (defun show-msg-after-timer ()
    "Show a message after timer expires. Based on run-at-time and can understand time like it can."
    (interactive)
    (let* ((msg-to-show (read-string "Enter msg to show: "))
           (time-duration (read-string "Time? ")))
      (message time-duration)
      (run-at-time time-duration nil #'message-box msg-to-show)))

  ;; interface with org-mode via a hydra menu
  (defhydra hydra-org (:columns 3
                       :exit t)
    "Org-mode Menu"
    ("l" org-store-link "store link")
    ("a" hydra-org-agenda/body "agenda")
    ("s" hydra-org-scheduling/body "scheduling")
    ("c" org-capture "capture")
    ("t" org-todo "todo")
    ("o" show-msg-after-timer "timer")
    ("g" org-set-tags "set tags")
    ("i" org-ibuffer "view all open org buffers")
    ("b" org-switchb "org switch buffer"))

  (defhydra hydra-org-agenda (:exit t)
    "Agenda"
    ("a" org-agenda "agenda")
    ("+" org-agenda-file-to-front "add file to agenda")
    ("-" org-remove-file "remove file from agenda")
    ("b" org-switchb "org switch buffer"))

  (defhydra hydra-org-scheduling (:exit t)
    "Scheduling"
    ("t" org-time-stamp "+ date")
    ("T" (lambda ()
           (interactive)
           (org-time-stamp '(4))) "+ date/time")
    ("d" org-deadline "+ deadline")
    ("s" org-schedule "schedule"))

  ;; access the org-mode menu via a "body" keybinding
  (global-set-key (kbd "s-o") 'hydra-org/body))

(use-package sicp
  :defer t)

(use-package buttercup
  :defer t)
