;; email, web, networking, phone, chat, IRC, etc.

(use-package erc
  :config
  ;; See https://www.gnu.org/software/emacs/manual/html_node/erc/Sample-Configuration.html
  ;; for more configuration options
  ;; Also: https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
  (setq erc-nick "countvajhula")
  (setq erc-server "irc.libera.chat")
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#racket")))
  (setq erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#racket"))))
