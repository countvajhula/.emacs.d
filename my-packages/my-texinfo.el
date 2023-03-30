(defun my-texinfo-open-output-file ()
  "Open Info output."
  (interactive)
  (let ((original-window (selected-window)))
    (find-file-other-window (string-join (list (file-name-base (buffer-name)) ".info")))
    (Info-on-current-buffer)
    (register-texinfo-leader)
    (select-window original-window)))

;; modified from texi2info
(defun my-texinfo-compile ()
  "Compile to Info output."
  (interactive)
  (save-buffer)
  (let ((original-window (selected-window)))
    (switch-to-buffer-other-window (buffer-name))
    (makeinfo-buffer)
    (sit-for 0.3) ; otherwise it selects the original window too soon ¯\_(ツ) _/¯
    (select-window original-window)))

(defhydra hydra-texinfo (:timeout my-leader-timeout
                         :columns 2
                         :exit t)
  "Texinfo menu"
  ("l" my-texinfo-open-output-file "Open output file")
  ("o" texinfo-show-structure "View in Info mode")
  ("p" Info-on-current-buffer "View in Info mode")
  ("i" TeX-doc "See documentation on this")
  ("?" TeX-doc "See documentation on this")
  ("x" my-texinfo-compile "Compile to an output format")
  ("\\" my-texinfo-compile "Compile to an output format"))

(defun register-texinfo-leader ()
  "Pull up Texinfo hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-texinfo/body))

(defvar texinfo-modes (list 'texinfo-mode
                            'Info-mode))

;; register texinfo leader in all texinfo modes
(dolist (mode-name texinfo-modes)
  (let ((mode-hook (intern (concat (symbol-name 'texinfo-mode)
                                   "-hook"))))
    ;; NOTE: although this is correctly added to the hook,
    ;; it does not get called upon entry to texinfo mode,
    ;; for reasons unknown.
    (add-hook mode-hook #'register-texinfo-leader)))

;; configure viewing .info files in info mode
;; simply using info mode isn't enough
;; NOTE: for some reason, this doesn't work. It works on any made-up
;; file extension, but does not work on .info.
;; UPDATE: it's because the info file contains a file-local
;; variable specification at the top which takes precedence
;; over auto-mode-alist. See https://www.emacswiki.org/emacs/SetAutoMode
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

(provide 'my-texinfo)
