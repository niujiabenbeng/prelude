;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Show all minor mode in Helm buffer.

;;;Code:

(defun personal-show-all-minor-mode ()
  "Show all minor modes in Helm buffer."
  (interactive)
  (declare (obsolete nil "2021.02.26"))
  ;; borrowed from source code: `describe-mode'
  (let (minor-modes)
    ;; Older packages do not register in minor-mode-list but only in
    ;; minor-mode-alist.
    (dolist (x minor-mode-alist)
      (setq x (car x))
      (unless (memq x minor-mode-list)
        (push x minor-mode-list)))
    ;; Document a minor mode if it is listed in minor-mode-alist,
    ;; non-nil, and has a function definition.
    (dolist (mode minor-mode-list)
      (let ((fmode (or (get mode :minor-mode-function) mode)))
        (and (boundp mode) (symbol-value mode) (fboundp fmode)
             (push mode minor-modes))))
    (helm :sources
          (helm-build-sync-source "Minor mode: "
            :candidates minor-modes
            :action (lambda (m) (describe-minor-mode (intern m)))))))

(provide 'modeline-show-all-minor-mode)

;;; modeline-show-all-minor-mode.el ends here
