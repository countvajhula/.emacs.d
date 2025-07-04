(defun my-lisp-repl ()
  "Enter elisp REPL, context-aware.

If the REPL is already visible, switch to that window.  Otherwise,
if there is only one window, open REPL in a new window.  Otherwise
open in most recently used other window."
  (interactive)
  (let ((window (get-buffer-window "*ielm*")))
    (cond (window (select-window window))
          ((= 1 (length (window-list)))
           (evil-window-vsplit)
           (evil-window-right 1)
           (ielm))
          (t (evil-window-mru)  ; better LRU
             (ielm)))
    (goto-char (point-max))))

(defun my-shell ()
  "Enter eshell.

If there is only one window, open REPL in a new window.  Otherwise
open in most recently used other window."
  (interactive)
  (let ((window (get-buffer-window "*eshell*")))
    (cond (window (select-window window))
          ((= 1 (length (window-list)))
           (evil-window-vsplit)
           (evil-window-right 1)
           (eshell))
          (t (evil-window-mru)  ; better LRU
             (eshell)))
    (goto-char (point-max))))

(defun my-switch-to-scratch-buffer (&optional other)
  "Switch to scratch buffer."
  (interactive)
  ;; TODO: create in lisp interaction mode if missing
  (if other
      (switch-to-buffer-other-window "*scratch*")
    (switch-to-buffer "*scratch*")))

(defun my-switch-to-messages-buffer ()
  "Switch to messages buffer while retaining focus in original window."
  (interactive)
  (switch-to-buffer-other-window "*Messages*")
  (goto-char (point-max))
  (recenter)
  (evil-window-mru))

(defun my-current-dir ()
  "View current dir in dired."
  (interactive)
  (dired nil))

(defun my-recenter-view-advice (orig-fn &rest args)
  "Depending on context, recenter screen on cursor.

Recenter if new jump location is not visible from any part of the
initial screen (when centered) -- same behavior as Vim."
  (condition-case err
      (progn (save-excursion
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
    (error (message "Buried error: %S" err))))

(defun my-disable-line-numbers ()
  "Disable line numbers in current buffer."
  (display-line-numbers-mode -1))

(defun my-enable-cursor ()
  "Enable the cursor."
  (internal-show-cursor nil t))

(defun my-disable-cursor ()
  "Disable the cursor."
  (internal-show-cursor nil nil))

(defun my-enable-blink-cursor ()
  "Enable blinking of the cursor."
  (blink-cursor-mode 1))

(defun my-disable-blink-cursor ()
  "Disable blinking of the cursor."
  (blink-cursor-mode -1))

(defun my-enable-show-paren-mode ()
  "Enable show-paren-mode."
  (show-paren-mode 1))

(defun my-disable-show-paren-mode ()
  "Disable show-paren-mode."
  (show-paren-mode -1))

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

(defun my-camel-case-to-snake-case ()
  "Change the word at point from camelCase to snake_case."
  (interactive)
  (let ((result (s-snake-case (thing-at-point 'word))))
    (apply #'evil-delete (evil-inner-word))
    (insert result)))

(defun my-count-lines-page ()
  "Modified from emacs's built-in count-lines-page to return a list of
   values corresponding to the position in the page."
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
	      total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	      (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	        before (count-lines beg opoint)
	        after (count-lines opoint end))
      (list total before after))))

(defun my-buffer-info ()
  "get info on current buffer -- similar to Vim's C-g"
  (interactive)
  (-let [(total before after) (my-count-lines-page)]
    (if (= total 0)
	    (setq bufinfo (list "-- No lines in buffer --"))
      (progn (setq percentage (floor (* (/ (float before)
					                       total)
					                    100)))
	         (setq page-position (concat
				                  "-- "
				                  (number-to-string percentage)
				                  "%"
				                  " --"))
	         (setq total-lines (concat
				                (number-to-string total)
				                " lines"))
	         (setq bufinfo (list total-lines page-position))))
    (add-to-list 'bufinfo
		         (buffer-file-name))
    (message "%s" (string-join bufinfo " "))))

(cl-defun my-new-empty-buffer (&optional
                               buffer-name
                               major-mode-to-use
                               &key
                               switch-p)
  "Create a new empty buffer.

If BUFFER-NAME is not provided, the new buffer will be named
“untitled” or “untitled<2>”, “untitled<3>”, etc. The buffer will be
created in the currently active (at the time of command execution)
major mode.
If SWITCH-P is true, switch to the newly created buffer.

Modified from:
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let* ((buffer-name (or buffer-name "untitled"))
         (major-mode-to-use (or major-mode-to-use major-mode))
         ($buf (generate-new-buffer buffer-name)))
    (with-current-buffer $buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save t))
    (when switch-p
      (switch-to-buffer $buf))
    $buf))

;; Modified from:
;; https://www.reddit.com/r/emacs/comments/7wsnoi/using_countdown_timers_for_alerts/
(defun show-msg-after-timer (&optional time-duration msg-to-show)
  "Show a message after timer expires. Based on run-at-time and can understand time like it can."
  (interactive "nTime (sec)? ")
  (let* ((msg-to-show (or msg-to-show (read-string "Enter msg to show: ")))
         (hrs (/ time-duration 3600))
         (mins (/ (% time-duration 3600) 60))
         (secs (% time-duration 60))
         (time-string
          (cond ((> hrs 0)
                 (cond ((> secs 0) (format "%d hrs, %d mins, %d secs" hrs mins secs))
                       ((> mins 0) (format "%d hours %d minutes" hrs mins))
                       (t (format "%d hours" hrs))))
                ((> mins 0)
                 (cond ((> secs 0) (format "%d mins %d secs" mins secs))
                       (t (format "%d minutes" mins))))
                (t (format "%d seconds" secs)))))
    (message "Started timer for %s..." time-string)
    ;; TODO: separate the timer from the message
    ;; so this should be about starting a timer for the duration
    ;; and pretty printing it. it should accept a lambda as input arg
    ;; and then call that with the timer here, rather than
    ;; message-box directly (the lambda would call it and also
    ;; have a default message if none is set).
    (run-at-time time-duration nil #'message-box msg-to-show)))
