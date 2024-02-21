;; Latex stuff
(use-package tex
  :defer t
  :straight auctex
  :config
  ;; make latexmk available via C-c C-c
  ;; (latexmk is a popular perl script to minimize number of
  ;; passes of compilation)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda ()
                              (setq TeX-command-default "latexmk")))
  ;; use Skim as pdf viewer
  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "open -a Skim.app %o")
          ("displayline" "displayline -g -b %n %o %b")
          ("open" "open %o"))
        TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "Skim")
          (output-html "open")))
  ;; Turn on RefTeX in AUCTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t))

(defun my-reftex-toc ()
  "TOC in other window."
  (interactive)
  (reftex-toc)
  (delete-window)
  (switch-to-buffer-other-window "*toc*"))

(defhydra hydra-tex (:timeout my-leader-timeout
                     :columns 2
                     :exit t)
  "LaTeX menu"
  ("a" TeX-command-run-all "Compile and view")
  ("c" TeX-command-master "Compile")
  ("o" my-reftex-toc "TOC")
  ("l" TeX-recenter-output-buffer "See Output")
  ("v" TeX-view "View"))

;; pull up LaTeX hydra with local leader
(add-hook 'LaTeX-mode-hook
          (lambda () (general-define-key
                      :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-tex/body)))

(provide 'my-latex)
