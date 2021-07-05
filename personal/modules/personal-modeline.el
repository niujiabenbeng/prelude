;;; -*- lexical-binding: t; -*-
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
    (diminish (car mode)))
  (diminish-undo 'company-mode))

(add-hook 'after-init-hook  #'personal-diminish-all t)
(add-hook 'emacs-lisp-mode  #'personal-diminish-all t)
(add-hook 'python-mode-hook #'personal-diminish-all t)
(add-hook 'c++-mode-hook    #'personal-diminish-all t)
(add-hook 'prog-mode-hook   #'personal-diminish-all t)

(provide 'personal-modeline)

;;; personal-modeline.el ends here
