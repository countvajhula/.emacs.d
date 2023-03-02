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

(use-package wgrep)

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
  :after (org-ibuffer hydra buffer-ring rigpa)
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
  ;; use evil hierarchy bindings in insert state too
  (define-key org-mode-map
    (kbd "M-h") #'org-promote-subtree)
  (define-key org-mode-map
    (kbd "M-l") #'org-demote-subtree)
  (define-key org-mode-map
    (kbd "M-j") #'outline-move-subtree-down)
  (define-key org-mode-map
    (kbd "M-k") #'outline-move-subtree-up)

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
  (global-set-key (kbd "s-o") 'hydra-org/body)

  (defvar my--org-context-buffers (list "daisywheel.org"
                                        "continuations.org")) ; "plan.org"

  ;; keep these org buffers out of the rigpa primary
  ;; buffer ring, so that navigating them doesn't affect
  ;; buffer locality on the primary ring
  (dolist (buf-name my--org-context-buffers)
    (add-to-list 'rigpa-buffer-ignore-buffers buf-name))

  (defvar my--org-path-prefix "~/log/org/")

  (defun my-org-reference-buffer ()
    (get-buffer "daisywheel.org"))

  (defun my--set-plumb-line ()
    "Set the plumb line unconditionally."
    ;; This assumes it is being called in the org reference buffer
    (setq-local plumb-line-started (current-time)))

  (defun my-ensure-plumb-line ()
    "Ensure the plumb line has been set."
    (with-current-buffer (my-org-reference-buffer)
      (unless (and (boundp 'plumb-line-started) plumb-line-started)
        (my--set-plumb-line))))

  (defun my-start-daisy-timer ()
    "Start timer for daisy wheel."
    (let ((timer (show-msg-after-timer (* 5 60) "The wheel turns.")))
      (with-current-buffer (my-org-reference-buffer)
        (setq-local daisy-timer timer)
        (setq-local daisy-message nil)
        ;; set a plumb line if one isn't already set
        (my-ensure-plumb-line))))

  (defun my-cancel-daisy-timer ()
    "Cancel daisy wheel timer."
    (let ((timer (with-current-buffer (my-org-reference-buffer)
                   (and (boundp 'daisy-timer) daisy-timer))))
      (when timer
        ;; TODO: daisy timer isn't getting canceled
        ;; check whether it's correctly defined in the org buffers
        ;; and whehter it (at 3 sec) can be canceled manually
        ;; actually check a timer in isolation at the REPL
        (cancel-timer timer)
        (message "Canceled timer."))))

  (defun my-cancel-plumb-line ()
    "Cancel plumb line."
    (with-current-buffer (my-org-reference-buffer)
      (setq-local plumb-line-started nil)
      (message "Lost plumb line.")))

  ;; Modified from task-timer Emacs library:
  ;; https://www.emacswiki.org/emacs/TaskTimer
  (defun my-reset-plumb-line ()
    (interactive)
    (with-current-buffer (my-org-reference-buffer)
      (my--set-plumb-line)))

  (defun my-render-time (time)
    "Render a time value in a useful way at the right scale."
    (let* ((time (decode-time time
                              ;; set utc offset of zero? otherwise
                              ;; the hour reports as 16
                              ;; see: https://stackoverflow.com/q/41231682/323874
                              0))
           (seconds (decoded-time-second time))
           (minutes (decoded-time-minute time))
           (hours (decoded-time-hour time)))
      (if (> hours 0)
          (format "%d hr %d min" hours minutes)
        (if (> minutes 0)
            (format "%d min %d sec" minutes seconds)
          (format "%d sec" seconds)))))

  (defun my-daisy-timer-status ()
    "Check time remaining on daisy timer."
    (let ((timer (with-current-buffer (my-org-reference-buffer)
                   (and (boundp 'daisy-timer) daisy-timer))))
      (when timer
        (let ((seconds (- (timer-until timer
                                       (current-time)))))
          (when (> seconds 0)
            (my-render-time (seconds-to-time seconds)))))))

  (defun task-timer-status ()
    (interactive)
    (let ((time-started (with-current-buffer (my-org-reference-buffer)
                          (and (boundp 'plumb-line-started) plumb-line-started))))
      (when time-started
        (my-render-time
         (time-subtract (current-time)
                        time-started)))))

  ;; TODO: add ability to pause plumb line?
  ;; this would start a separate pause "timer"
  ;; then, the result would always be actual minus
  ;; the sum of all pause times collected in a list
  (defun my-timer-status ()
    "Show status of active timers."
    (interactive)
    (let ((daisy-status (my-daisy-timer-status))
          (plumb-status (task-timer-status)))
      (if daisy-status
          (message "%s to go [Plumb line: %s]"
                   daisy-status
                   plumb-status)
        (if plumb-status
            (message "Plumb line: %s"
                     plumb-status)
          (message "No plumb line set. Time to dive in?")))))

  (defun my-remember-work-buffer ()
    "Remember work context buffer as a property on the hydra."
    (let ((buffer-name (buffer-name (current-buffer))))
      (unless (member buffer-name my--org-context-buffers)
        (setq my-daisy-entry-buffer buffer-name))))

  (defun my-switch-to-work-context ()
    "Switch to work context."
    (interactive)
    (let ((entry-buffer my-daisy-entry-buffer))
      (switch-to-buffer entry-buffer)))

  (defun my-initialize-org-buffers ()
    "Ensure that org buffers are open - open them if they aren't already."
    (interactive)
    (let ((original-buffer (current-buffer)))
      (dolist (buf-name my--org-context-buffers)
        (unless (get-buffer buf-name)
          (find-file (concat my--org-path-prefix
                             buf-name))))
      (unless (eq original-buffer (current-buffer))
        ;; if we ended up creating any missing org buffers
        ;; we would have switched to them - let's return
        (switch-to-buffer original-buffer))))

  (defun my-org-create-buffer-ring ()
    "Create the buffer ring upon entry into org mode.

If the ring already exists, just switch to it."
    (interactive)
    ;; delete buffer ring and rebuild from scratch each time
    (let ((ring-name "my-org-buffer-ring"))
      (unless (buffer-ring-torus--find-ring ring-name) ; change semantics in buffer-ring
        (dolist (buf my--org-context-buffers)
          (buffer-ring-add ring-name buf)))
      (buffer-ring-torus-switch-to-ring ring-name)))

  ;; quick access to daisy wheel and continuations
  (defhydra hydra-daisy (:body-pre (progn (my-initialize-org-buffers)
                                          (my-remember-work-buffer)
                                          (my-org-create-buffer-ring))
                         :exit t)
    "Daisy wheel"
    ("h" buffer-ring-prev-buffer "previous" :exit nil)
    ("l" buffer-ring-next-buffer "next" :exit nil)
    ("d" (lambda ()
           (interactive)
           (switch-to-buffer "daisywheel.org"))
     "daisy wheel")
    ("c" (lambda ()
           (interactive)
           (switch-to-buffer "continuations.org"))
     "continuations")
    ("p" (lambda ()
           (interactive)
           (switch-to-buffer "plan.org"))
     "plan")
    ("'" rigpa-buffer-return-to-mark "return to buffer mark")
    ("s-o" (lambda ()
             (interactive)
             (my-cancel-daisy-timer)
             (my-start-daisy-timer)
             (my-switch-to-work-context))
     "start timer")
    ("o" (lambda ()
           (interactive)
           (my-cancel-daisy-timer)
           (my-start-daisy-timer)
           (my-switch-to-work-context))
     "start timer")
    ("O" (lambda ()
           (interactive)
           (my-cancel-daisy-timer)
           (my-switch-to-work-context))
     "start timer")
    ("s-i" (lambda ()
             (interactive)
             (my-reset-plumb-line)
             (my-switch-to-work-context))
     "start task timer")
    ("i" (lambda ()
           (interactive)
           (my-reset-plumb-line)
           (my-switch-to-work-context))
     "start task timer")
    ("I" (lambda ()
           (interactive)
           (my-cancel-plumb-line)
           (my-switch-to-work-context))
     "cancel plumb line")
    ("?" (lambda ()
           (interactive)
           (my-timer-status)
           (my-switch-to-work-context))
     "active timers status")
    ("s-k" (lambda ()
             (interactive)
             (my-timer-status)
             (my-switch-to-work-context))
     "active timers status")
    ("s-j" my-switch-to-work-context "return to work")
    ("q" my-switch-to-work-context "quit"))

  (global-set-key (kbd "s-j") 'hydra-daisy/body))

(use-package sicp
  :defer t)

(use-package buttercup
  :defer t)
