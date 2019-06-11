;;;;;;;;;;;;;;;;;;;
;; LOOK AND FEEL ;;
;;;;;;;;;;;;;;;;;;;


;; appearance
(add-to-list 'custom-theme-load-path (concat user-emacs-directory
                                             "themes/"))
;; some themes have several variations (e.g. light and dark)
;; and share code between these variations in common elisp modules;
;; these modules need to be on the load path so that these themes work
(add-to-list 'load-path (concat user-emacs-directory
                                "themes/"))

;; set color scheme
;; (load-theme 'tango-dark t)
(use-package remember-last-theme
  :config
  (remember-last-theme-enable))

(set-frame-font "Menlo 12" nil t)

;; line numbers on by default
(global-display-line-numbers-mode)
;; show (line #, column #) in mode line
(setq column-number-mode t)

;; cool transparency [from emacswiki]
;; the alpha params are "active" and "inactive" frame
;; this is if you want the in-focus and not-in-focus
;; emacs frames to have different transparencies
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(provide 'my-look-and-feel)
