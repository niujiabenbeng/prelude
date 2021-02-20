;;; personal-modeline.el --- Additional modeline configuration.

;;; Commentary:

;; Additional modeline configuration.

;;; Code:

(prelude-require-package 'smart-mode-line)
(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)
(setq sml/name-width '(44 . 60))
(add-hook 'after-init-hook #'sml/setup)

(defun personal-diminish-all ()
  "Hide all minor modes in mode line."
  (dolist (mode minor-mode-alist)
    (diminish (car mode))))

(add-hook 'after-init-hook  #'personal-diminish-all t)
(add-hook 'emacs-lisp-mode  #'personal-diminish-all t)
(add-hook 'python-mode-hook #'personal-diminish-all t)
(add-hook 'c++-mode-hook    #'personal-diminish-all t)
(add-hook 'prog-mode-hook   #'personal-diminish-all t)

(defun personal-show-all-minor-mode ()
  "Show all minor modes in Helm buffer."
  (interactive)
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

(provide 'personal-modeline)

;;; personal-modeline.el ends here
