;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Change the style of name in c++ program.

;;;Code:

(require 'cc-mode)

(defun util-cc-change-name-style ()
  "Change the style of name in c++ program."
  (interactive)
  (let ((name (thing-at-point 'symbol t)) new)
    (cond
     ((null name) (message "No name found at point."))
     ;; action: file_name -> FileName
     ((string-match-p "^[0-9a-z_]+$" name)
      (setq new (mapconcat #'capitalize (split-string name "_") "")))
     ;; action: FileName -> FILE_NAME
     ((string-match-p "^\\([A-Z][0-9a-z]*\\)*$" name)
      (let ((elems (personal-search-all "[A-Z][0-9a-z]*" name)))
        (setq new (mapconcat #'upcase elems "_"))))
     ;; action: FILE_NAME -> file_name
     ((string-match-p "^[0-9A-Z_]+$" name)
      (setq new (downcase name)))
     ;; default: print an error message
     (t (message "Invalid name at point.")))
    (when new
      (delete-region (beginning-of-thing 'symbol) (end-of-thing 'symbol))
      (save-excursion (insert new)))))

(provide 'util-cc-change-name-style)

;;; util-cc-change-name-style.el ends here
