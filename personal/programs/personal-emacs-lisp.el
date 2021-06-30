;;; -*- lexical-binding: t; -*-
;;; personal-emacs-lisp.el --- Personal configuration for emacs lisp.

;;; Commentary:

;; Personal configuration for emacs lisp.

;;; Code:

(require 'prelude-emacs-lisp)

;; make flycheck find the right packages
(setq-default flycheck-emacs-lisp-load-path load-path)

;; disable flycheck in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)) t)

(let ((map emacs-lisp-mode-map))
  (define-key map (kbd "C-c C-z") nil) ;; previous run: prelude-visit-ielm
  (define-key map (kbd "C-c C-c") nil) ;; previous run: eval-defun
  (define-key map (kbd "C-c C-b") nil) ;; previous run: eval-buffer
  )

(provide 'personal-emacs-lisp)

;;; personal-emacs-lisp.el ends here
