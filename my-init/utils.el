(defun my-noop (&rest args)
  "A function that does nothing."
  (interactive))

(defun my-lisp-repl ()
  "Enter elisp REPL, context-aware.

If there is only one window, open REPL in a new window. Otherwise
open in current window."
  (interactive)
  (when (= (length (window-list))
           1)
    (progn (evil-window-vsplit)
           (evil-window-right 1)))
  (ielm))

(defun my-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))  ; TODO: create in lisp interaction mode if missing

(defun my-switch-to-messages-buffer ()
  "Switch to messages buffer while retaining focus in original window."
  (interactive)
  (switch-to-buffer-other-window "*Messages*")
  (evil-window-mru))

(defun my-current-dir ()
  "View current dir in dired."
  (interactive)
  (dired nil))

(defun my-recenter-view-advice (orig-fn &rest args)
  "Depending on context, recenter screen on cursor.

Recenter if new jump location is not visible from any part of the
initial screen (when centered) -- same behavior as Vim."
  (save-excursion
    (evil-window-top)
    (setq initial-screen-top-line (line-number-at-pos))
    (evil-window-bottom)
    (setq initial-screen-bottom-line (line-number-at-pos)))
  (let ((res (apply orig-fn args)))
    (let* ((current-line-position (line-number-at-pos))
           (distance-from-screen-top (abs (- current-line-position
                                             initial-screen-top-line)))
           (distance-from-screen-bottom (abs (- current-line-position
                                                initial-screen-bottom-line)))
           (min-distance (min distance-from-screen-top
                              distance-from-screen-bottom)))
      (when (> min-distance
               (/ (window-text-height)
                  2))
        (recenter))
      res)))

(defun current-line-empty-p ()
  "From: https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace "
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;;; `with-undo-collapse` macro, to treat a sequence of operations
;;; as a single entry in the undo list.
;;; From: https://emacs.stackexchange.com/questions/7558/collapsing-undo-history/7560#7560
(defun undo-collapse-begin (marker)
  "Mark the beginning of a collapsible undo block.
This must be followed with a call to undo-collapse-end with a marker
eq to this one."
  (push marker buffer-undo-list))

(defun undo-collapse-end (marker)
  "Collapse undo history until a matching marker."
  (cond
   ((eq (car buffer-undo-list) marker)
    (setq buffer-undo-list (cdr buffer-undo-list)))
   (t
    (let ((l buffer-undo-list))
      (while (not (eq (cadr l) marker))
        (cond
         ((null (cdr l))
          (error "undo-collapse-end with no matching marker"))
         ((eq (cadr l) nil)
          (setf (cdr l) (cddr l)))
         (t (setq l (cdr l)))))
      ;; remove the marker
      (setf (cdr l) (cddr l))))))

(defmacro with-undo-collapse (&rest body)
  "Execute body, then collapse any resulting undo boundaries."
  (declare (indent 0))
  (let ((marker (list 'apply 'identity nil)) ; build a fresh list
        (buffer-var (make-symbol "buffer")))
    `(let ((,buffer-var (current-buffer)))
       (unwind-protect
           (progn
             (undo-collapse-begin ',marker)
             ,@body)
         (with-current-buffer ,buffer-var
           (undo-collapse-end ',marker))))))

(defun point-at-indentation-p ()
  "Check if point is at the point of indentation, i.e. if it's
the first non-whitespace character.

From: https://stackoverflow.com/a/13313091"
  (= (save-excursion (back-to-indentation)
                         (point))
     (point)))
