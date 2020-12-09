;;; Create and display an `ibuffer' showing only `org-mode' files.
;;;
;;; I have bound this to "\C-cb" using:
;;;    (global-set-key "\C-cb" #'org-ibuffer)
;;;
;;; Now "\C-x\C-b" creates a generic `ibuffer' and "\C-cb"
;;; creates an `org-mode' `ibuffer'.

(require 'ibuffer)

(defun org-ibuffer ()
  "Open an `ibuffer' window showing only `org-mode' buffers."
  (interactive)
  (ibuffer nil "*Org Buffers*" '((used-mode . org-mode))))

;;; This work created by Neil Smithline is licensed under the Creative
;;; Commons Attribution-ShareAlike 3.0 Unported License.
;;;
;;; To view a copy of this license, visit
;;; http://creativecommons.org/licenses/by-sa/3.0/ or send a letter to
;;; Creative Commons, 444 Castro Street, Suite 900, Mountain View,
;;; California, 94041, USA.
;;;
;;; Inquiries about further rights can be made at
;;; http://www.neilsmithline.com/

(provide 'org-ibuffer)
