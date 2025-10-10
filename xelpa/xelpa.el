;; xelpa.el --- Implements the "xelpa" local recipe repository protocol.
;; -*- lexical-binding: t -*-

(defun straight-recipes-xelpa-retrieve (package)
  "Look up a PACKAGE recipe in the local xelpa repository.
PACKAGE should be a symbol. If a recipe file for the package
exists, return its contents; otherwise return nil."
  (let ((recipe-file (expand-file-name (symbol-name package) "recipes/")))
    (when (file-exists-p recipe-file)
      (with-temp-buffer
        (insert-file-contents-literally recipe-file)
        (read (current-buffer))))))

(defun straight-recipes-xelpa-list ()
  "Return a list of all available recipes in xelpa, as strings."
  ;; Assumes default-directory is the root of the xelpa repo.
  (cl-remove-if #'file-directory-p
                (directory-files "recipes/")))

(defun straight-recipes-xelpa-version ()
  "Return the current version of the xelpa retriever logic."
  1)

(provide 'xelpa)
