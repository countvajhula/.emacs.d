;; email, web, networking, phone, chat, IRC, etc.

(use-package erc
  :config
  ;; See https://www.gnu.org/software/emacs/manual/html_node/erc/Sample-Configuration.html
  ;; for more configuration options
  (setq erc-nick "countvajhula")
  (setq erc-server "irc.libera.chat")
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))
