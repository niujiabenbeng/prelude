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

(defun personal-elisp-find-thing-at-point (sym-name)
  "Find the elisp thing at point, and show in other window."
  (interactive (list (elisp-slime-nav--read-symbol-at-point)))
  (personal-display-result-other-window
   (elisp-slime-nav-find-elisp-thing-at-point sym-name)))

(defun personal-elisp-pop-marker-stack ()
  "Show previous marker position in other window."
  (interactive)
  (personal-display-result-other-window
   (xref-pop-marker-stack)))

(let ((map emacs-lisp-mode-map))
  (define-key map (kbd "C-c C-z") nil) ;; previous run: prelude-visit-ielm
  (define-key map (kbd "C-c C-c") nil) ;; previous run: eval-defun
  (define-key map (kbd "C-c C-b") nil) ;; previous run: eval-buffer
  (define-key map (kbd "C-M-.") 'personal-elisp-find-thing-at-point)
  (define-key map (kbd "C-M-,") 'personal-elisp-pop-marker-stack))

(defvar personal--elisp-company-backends
  '((company-abbrev company-dabbrev company-dabbrev-code)
    company-capf company-files)
  "Company backends for emacs lisp.")

(add-hook
 'emacs-lisp-mode-hook
 (lambda () (setq-local company-backends personal--elisp-company-backends)))

(provide 'personal-emacs-lisp)

;;; personal-emacs-lisp.el ends here
