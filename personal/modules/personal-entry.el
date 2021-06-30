;;; -*- lexical-binding: t; -*-
;;; personal-entry.el --- Configuration when starting emacs.

;;; Commentary:

;; Configuration when starting emacs.

;;;Code:

(prelude-require-package 'psession)
(require 'neotree)
(require 'psession)
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
  (find-file file-name)
  (read-only-mode read-only)
  (when personal-start-neotree-after-init
    (emacs-client-toggle-neotree)
    ;; when neotree is activated immediately after emacs is started,
    ;; a display problem may occur, so we redraw display 0.5s later.
    (run-with-timer 0.5 nil 'redraw-display)))

(defun emacs-client-save-session ()
  "Save emacs client session."
  (unless (file-directory-p psession-elisp-objects-default-directory)
    (make-directory psession-elisp-objects-default-directory t))
  (psession-save-last-winconf)
  (psession--dump-some-buffers-to-list)
  (psession--dump-object-to-file-save-alist))

(defun emacs-client-load-session ()
  "Restore last emacs client session."
  (psession-restore-last-winconf)
  (psession--restore-some-buffers)
  (psession--restore-objects-from-directory))

;; save current session before exiting emacs
(global-set-key
 (kbd "C-x C-c")
 (lambda () (interactive)
   (when (file-directory-p psession-elisp-objects-default-directory)
     (delete-directory psession-elisp-objects-default-directory t))
   (emacs-client-save-session)
   (save-buffers-kill-terminal 1)))

(provide 'personal-entry)

;;; personal-entry.el ends here
