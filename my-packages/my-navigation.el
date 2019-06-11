;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM NAVIGATION FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my-jump-down ()
  (interactive)
  (dotimes (i 9)
    (evil-next-line)))

(defun my-jump-up ()
  (interactive)
  (dotimes (i 9)
    (evil-previous-line)))

(defun my-scroll-down ()
  (interactive)
  (evil-scroll-line-down 3))

(defun my-scroll-up ()
  (interactive)
  (evil-scroll-line-up 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM NAVIGATION KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-key
  ;; alternative to Vim's C-u (since emacs reserves C-u)
  evil-motion-state-map
  (kbd "C-S-d")
  'evil-scroll-up)

(define-key
  ;; alternative to Vim's C-b (for symmetry with C-S-d necessitated above)
  evil-motion-state-map
  (kbd "C-S-f")
  'evil-scroll-page-up)

(define-key
  ;; handy navigation to jump down the file
  evil-motion-state-map
  (kbd "C-s-j")
  'my-jump-down)

(define-key
  ;; handy navigation to jump up the file
  evil-motion-state-map
  (kbd "C-s-k")
  'my-jump-up)

(define-key
  ;; handy navigation to jump down the file (more convenient alternative,
  ;; which may be overridden in some modes (like org, and magit)
  evil-motion-state-map
  (kbd "C-j")
  'my-jump-down)

(define-key
  ;; handy navigation to jump up the file (more convenient alternative,
  ;; which may be overridden in some modes (like org, and magit)
  evil-motion-state-map
  (kbd "C-k")
  'my-jump-up)

(define-key
  ;; handy navigation to jump up the file
  evil-motion-state-map
  (kbd "<backspace>")
  'my-jump-up)

(define-key
  ;; scroll down the file a little faster than usual
  evil-motion-state-map
  (kbd "C-e")
  'my-scroll-down)

(define-key
  ;; scroll up the file a little faster than usual
  evil-motion-state-map
  (kbd "C-y")
  'my-scroll-up)

(define-key
  ;; remap original vim scroll bindings as "fine tuning"
  ;; rather than default scroll behavior
  evil-motion-state-map
  (kbd "C-S-e")
  'evil-scroll-line-down)

(define-key
  ;; remap original vim scroll bindings as "fine tuning"
  ;; rather than default scroll behavior
  evil-motion-state-map
  (kbd "C-S-y")
  'evil-scroll-line-up)

(define-key
  ;; go all the way to the beginning of the buffer
  evil-motion-state-map
  (kbd "C-S-h")
  'evil-goto-first-line)

(define-key
  ;; go all the way to the end of the buffer
  evil-motion-state-map
  (kbd "C-S-l")
  'evil-goto-line)


(provide 'my-navigation)
