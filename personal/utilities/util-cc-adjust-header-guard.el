;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Adjust guard of c++ header files.

;;;Code:

(require 'cc-mode)
(require 'personal-util)

(defun util-cc-adjust-header-guard ()
  "Adjust guard of c++ header files."
  (interactive)
  (when (buffer-file-name)
    (dolist (name (directory-files (file-name-directory (buffer-file-name))))
      (cond ((string-suffix-p ".pb.h" name) nil)
            ((string-suffix-p ".h" name)
             (with-temp-buffer
               (find-file name)
               (goto-char (point-min))
               (personal-pattern-replace)
               (save-buffer)))))))

(provide 'util-cc-adjust-header-guard)

;;; util-cc-adjust-header-guard.el ends here
