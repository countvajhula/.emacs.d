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
 '(arc-source-path "\"~/work/emacs/\"")
 '(caps-lock-mode nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (nimbus)))
 '(custom-safe-themes
   (quote
    ("ecc5d64c0a9114f49fafd5ae3ab6033d1d5a7d2a1af0cbcd5804c7b4833caea3" "23b76946d30991fdf3c221fb304d6d131c3d8bc79a071bc8b188dbe2908596ac" "88d31f762d52d5a2b3ab30c05eb98983933001092f0e23d05876802c27e56476" "4b9be03425adc2febcf5e377eeaeb43a61719786d8d5e25e41421cda1cf8930e" "296da7c17c698e963c79b985c6822db0b627f51474c161d82853d2cb1b90afb0" "6950cff91f52578d46e0c3c0b68d329a72157cca1e2380e2e8e7557b8257eb6d" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "1c7635fd451cc7834a4ec6ff92bf656d846cf75d9153ff01618f0d3e80323f04" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8fcbb734e7e8bb240299fb3d52628f13e1af6dda931ced3e1df7a0eb41309608" "84504dea3d3a35b93206101a50f4fbc2b4d869fd51b3b93d5b551895c54e9298" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "1a2a0286b90917fb70e4b578dc2a4fee3e768123e2ea6e0890f7a3e30505e119" "125744c0b04c7addbe25d238a4053740fad2ad5e18211662f637dbacb3802331" "06c178b80bedb9d4b6109e3b3989745ba21f1231f8b58fa4137c447854a7b020" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(default-input-method "TeX")
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#383838")
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (quote
    ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors
   (quote
    ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(hl-sexp-background-color "#1c1f26")
 '(ibuffer-deletion-face (quote diredp-deletion-file-name))
 '(ibuffer-marked-face (quote diredp-flag-mark))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(linum-format " %7i ")
 '(mac-option-modifier (quote meta))
 '(magit-diff-use-overlays nil)
 '(next-error-recenter (quote (4)))
 '(nrepl-message-colors
   (quote
    ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(package-selected-packages
   (quote
    (proof-general python-black slime cider suggest zoom-frm flycheck-package package-lint flycheck dash-functional beacon evil-cleverparens chess lispy paredit geiser ht adaptive-wrap remember-last-theme haskell-mode highlight buttercup edebug-x evil-surround projectile auctex evil-indent-plus ace-window evil-goggles fireplace general org telephone-line smart-mode-line ibuffer-vc ibuffer-sidebar ivy-rich popwin evil-collection ace-jump-buffer avy ivy-hydra caps-lock smex counsel yasnippet-snippets yasnippet evil-mc multiple-cursors minimap evil-matchit evil-tabs tabbar evil-magit php-mode ivy sicp company-jedi company sr-speedbar magit dictionary sublimity evil elpy)))
 '(pdf-view-midnight-colors (quote ("#FDF4C1" . "#282828")))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(python-check-command "/usr/local/bin/pyflakes")
 '(racket-program "racket")
 '(slime-auto-start (quote always))
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
   (quote
    ((20 . "#e43838")
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
     (360 . "#a020f0"))))
 '(vc-annotate-very-old-color "#a020f0")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
