;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATION DONE VIA "UI" OR DONE AUTOMATICALLY BY EMACS MODES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(arc-source-path "~/work/lisp/arc/arc3.2")
 '(beacon-mode t)
 '(caps-lock-mode nil)
 '(centaur-tabs-mode nil nil (centaur-tabs))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(doom-dracula))
 '(custom-safe-themes
   '("8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "e05096f93a045adf557ad2eb79118d71607e48e5b26815e6f375ae245fb346fc" "2925ed246fb757da0e8784ecf03b9523bccd8b7996464e587b081037e0e98001" "9033f710b1ee48e0c36a4797d81311b5bc1fb7b0509a0121b21c059637a1c905" "4ba6aa8a2776688ef7fbf3eb2b5addfd86d6e8516a701e69720b705d0fbe7f08" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bb02c91b7e1c6c4238a0ef5a13db784815ea74ff8f13cef198315b4d3d7ae83e" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "2b7242a74c59605fbf9b8d35a3f49883a9fa44aabd0c6cb2455862d88b3867aa" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "ecc5d64c0a9114f49fafd5ae3ab6033d1d5a7d2a1af0cbcd5804c7b4833caea3" "23b76946d30991fdf3c221fb304d6d131c3d8bc79a071bc8b188dbe2908596ac" "88d31f762d52d5a2b3ab30c05eb98983933001092f0e23d05876802c27e56476" "4b9be03425adc2febcf5e377eeaeb43a61719786d8d5e25e41421cda1cf8930e" "296da7c17c698e963c79b985c6822db0b627f51474c161d82853d2cb1b90afb0" "6950cff91f52578d46e0c3c0b68d329a72157cca1e2380e2e8e7557b8257eb6d" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "1c7635fd451cc7834a4ec6ff92bf656d846cf75d9153ff01618f0d3e80323f04" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8fcbb734e7e8bb240299fb3d52628f13e1af6dda931ced3e1df7a0eb41309608" "84504dea3d3a35b93206101a50f4fbc2b4d869fd51b3b93d5b551895c54e9298" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "1a2a0286b90917fb70e4b578dc2a4fee3e768123e2ea6e0890f7a3e30505e119" "125744c0b04c7addbe25d238a4053740fad2ad5e18211662f637dbacb3802331" "06c178b80bedb9d4b6109e3b3989745ba21f1231f8b58fa4137c447854a7b020" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#383838")
 '(global-display-line-numbers-mode t)
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-background-colors
   '("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00"))
 '(hl-paren-colors '("#326B6B"))
 '(hl-sexp-background-color "#1c1f26")
 '(hl-todo-keyword-faces
   '(("TODO" . "#ba29eb")
     ("NEXT" . "#ba29eb")
     ("THEM" . "#08a7b3")
     ("PROG" . "#00bfa5")
     ("OKAY" . "#006fd7")
     ("DONT" . "#ff3d00")
     ("FAIL" . "#b0151a")
     ("DONE" . "#22a54e")
     ("NOTE" . "#ffb627")
     ("KLUDGE" . "#fb6107")
     ("HACK" . "#4d10a5")
     ("TEMP" . "#7a7b75")
     ("FIXME" . "#ba29eb")
     ("DEPRECATED" . "#ff3d00")
     ("REVIEW" . "#b27701")
     ("XXX" . "#4d10a5")
     ("XXXX" . "#4d10a5")
     ("\\?\\?\\?+" . "#4d10a5")))
 '(ibuffer-deletion-face 'diredp-deletion-file-name)
 '(ibuffer-marked-face 'diredp-flag-mark)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ivy-prescient-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#131033" "#1ea8fc"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131033" "#a7da1e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131033" "#546A90"))
 '(linum-format " %7i ")
 '(mac-option-modifier 'meta)
 '(magit-diff-use-overlays nil)
 '(next-error-recenter '(4))
 '(nrepl-message-colors
   '("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d"))
 '(objed-cursor-color "#e61f44")
 '(package-selected-packages
   '(magit ripgrep rg selectrum-prescient selectrum eimp fill-column-indicator company-fuzzy vscode-icon parsec modalka zoom-window centered-cursor-mode all-the-icons-dired git-timemachine ivy-prescient dired-sidebar all-the-icons centaur-tabs jedi scribble-mode proof-general python-black slime cider suggest zoom-frm flycheck-package package-lint flycheck dash-functional beacon evil-cleverparens chess lispy paredit geiser ht adaptive-wrap remember-last-theme haskell-mode highlight buttercup edebug-x evil-surround projectile auctex evil-indent-plus ace-window evil-goggles fireplace general org telephone-line smart-mode-line ibuffer-vc ibuffer-sidebar ivy-rich popwin evil-collection ace-jump-buffer avy ivy-hydra caps-lock smex counsel yasnippet-snippets yasnippet evil-mc multiple-cursors minimap evil-matchit evil-tabs tabbar php-mode ivy sicp company-jedi company sr-speedbar dictionary sublimity evil elpy))
 '(pdf-view-midnight-colors '("#FDF4C1" . "#282828"))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(python-check-command "/usr/local/bin/pyflakes")
 '(racket-program "racket")
 '(rustic-ansi-faces
   ["#0c0a20" "#e61f44" "#a7da1e" "#ffd400" "#1ea8fc" "#ff2afc" "#42c6ff" "#f2f3f7"])
 '(slime-auto-start 'always)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#f6f0e1")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#e43838")
     (40 . "#f71010")
     (60 . "#ab9c3a")
     (80 . "#9ca30b")
     (100 . "#ef8300")
     (120 . "#958323")
     (140 . "#1c9e28")
     (160 . "#3cb368")
     (180 . "#028902")
     (200 . "#008b45")
     (220 . "#077707")
     (240 . "#259ea2")
     (260 . "#358d8d")
     (280 . "#0eaeae")
     (300 . "#2c53ca")
     (320 . "#1111ff")
     (340 . "#2020cc")
     (360 . "#a020f0")))
 '(vc-annotate-very-old-color "#a020f0")
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-active-bar-face ((t nil)))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
