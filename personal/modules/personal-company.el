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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backend settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local personal-company-backends nil "All backends")

(defvar-local personal-company-backend nil "Backend in use")

(defun personal-get-company-backends (major-backend)
  `((,major-backend company-files company-keywords company-yasnippet)
    (company-abbrev company-dabbrev company-dabbrev-code)))

(defun personal-set-company-backends (hook backends)
  "Set company backends to file local variables."
  (add-hook hook
            (lambda ()
              (setq-local personal-company-backends backends)
              (setq-local personal-company-backend (car backends))
              (setq-local company-backends (list personal-company-backend)))))

(defun personal-company-other-backend ()
  "Set company backend to next one and regenerate candidates."
  (interactive)
  (unless (looking-back "[-_/.0-9a-zA-Z]")
    (user-error "Cannot complete without prefix."))
  (when (looking-at-p "[-_/.0-9a-zA-Z]")
    (user-error "Cannot complete in the middle of a word."))
  (if-let* ((all  personal-company-backends)
            (curr personal-company-backend)
            (next (cadr (member curr all))))
      (setq-local personal-company-backend next)
    (setq-local personal-company-backend (car all)))
  (setq-local company-backends (list personal-company-backend))
  ;; the message is not shown in minibuffer, i don't know why
  (message "current company backend: %s" personal-company-backend)
  ;; copy from: `company-other-backend'
  (company-cancel)
  (company-begin-backend personal-company-backend))

;; set default values
(setq-default personal-company-backends (personal-get-company-backends 'company-capf))
(setq-default personal-company-backend (car personal-company-backends))
(setq-default company-backends (list personal-company-backend))

;; borrowed from prucell emacs `lisp/init-company.el'
(define-key company-mode-map [remap completion-at-point] 'company-complete)
(define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
(define-key company-mode-map (kbd "M-/") 'personal-company-other-backend)

(provide 'personal-company)

;;; personal-company.el ends here
