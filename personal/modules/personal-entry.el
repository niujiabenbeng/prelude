;;; -*- lexical-binding: t; -*-
;;; personal-entry.el --- Configuration when starting emacs.

;;; Commentary:

;; Configuration when starting emacs.

;;;Code:

(require 'neotree)
(require 'personal-neotree)

(defun emacs-client-toggle-neotree ()
  "Toggle neotree after initialization."
  (let ((window (selected-window)))
    ;; do not toggle neotree when frame is too small
    (when (> (frame-outer-width) 120)
      (neotree-toggle)
      (select-window window))))

(defun emacs-client-init-find-file (file-name read-only)
  "Visit file by emacs client."
  (when-let ((name (or file-name (car recentf-list))))
    (find-file name)
    (read-only-mode read-only))
  (when personal-start-neotree-after-init
    (emacs-client-toggle-neotree)
    ;; when neotree is activated immediately after emacs is started,
    ;; a display problem may occur, so we redraw display 0.1s later.
    (run-with-timer 0.1 nil 'redraw-display)))

(provide 'personal-entry)

;;; personal-entry.el ends here
