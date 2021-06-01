;;; -*- lexical-binding: t; -*-
;;; personal-company.el --- Custom company settings.

;;; Commentary:

;; Enhanced company settings for personal use. This configuration
;; combines prelude emacs settings and prucell emacs settings.

;; tip: `company-diag' to show backend currently used.

;;;Code:

(prelude-require-packages '(company company-quickhelp))

(require 'company)
(require 'company-quickhelp)

(setq company-idle-delay 0.2)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above nil)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-code-everywhere t)

(global-company-mode 1)
(company-quickhelp-mode 1)

;; borrowed from prucell emacs `lisp/init-company.el'
(define-key company-mode-map [remap completion-at-point] 'company-complete)
(define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
(define-key company-mode-map (kbd "M-/") 'company-other-backend)

(provide 'personal-company)

;;; personal-company.el ends here
