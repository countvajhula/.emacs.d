;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL BEHAVIOR ;;
;;;;;;;;;;;;;;;;;;;;;;


;; remove the toolbar at the top of the window
(tool-bar-mode -1)

;; refresh files from disk if there are changes
(global-auto-revert-mode t)

;; save autosave files in emacs folder instead of locally
;; in the folder containing the files being edited
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; "Civilized people want to version their backups and organize it so
;; that backups are not laced out through their harddrive, but rather
;; organized into specific backup directories."  From:
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
 ;; don't clobber symlinks
 backup-by-copying t
 ;; don't litter my fs tree
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; use versioned backups
 version-control t)

;; save any customizations done via Customize or set automatically by
;; various emacs modes to a designated file, instead of cluttering init.el
(setq custom-file (concat user-customizations-directory
                          "custom.el"))
(load custom-file 'noerror)

;; show diffs side-by-side
(setq ediff-split-window-function 'split-window-horizontally)

;; evil matchit mode loads automatically for html-mode and sgml-mode
;; but not the derivative mhtml-mode, which is actually the default
;; mode for HTML. manually enable it here for mhtml mode (probably figure
;; out a cleaner way to do this, e.g. by adding mhtml to the modes
;; that trigger autoloading, or via hooks for mhtml mode)
(plist-put evilmi-plugins 'mhtml-mode '((evilmi-simple-get-tag evilmi-simple-jump)
                                        (evilmi-html-get-tag evilmi-html-jump)))

;; tab width of 4
;; (setq tab-stop-list (number-sequence 4 120 4)) ;; don't think this is needed anymore, bases off of tab-width since Emacs 24
(setq-default tab-width 4)

;; scroll with 2 line margin for continuity
(setq scroll-margin 2)

;; keyboard scroll one line at a time instead of jumping
(setq scroll-step            1
      scroll-conservatively  10000)

;; wrap lines visually, preserving indentation (like vim's breakindent)
(use-package adaptive-wrap
  :config
  (global-visual-line-mode)
  ;; does this work globally?
  (adaptive-wrap-prefix-mode))

(provide 'my-general-behavior)
