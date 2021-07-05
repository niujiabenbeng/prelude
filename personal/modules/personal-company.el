;;; -*- lexical-binding: t; -*-
;;; personal-company.el --- Custom company settings.

;;; Commentary:

;; Enhanced company settings for personal use. This configuration
;; combines prelude emacs settings and prucell emacs settings.

;; tip:
;; * `company-diag' to show backend currently used.
;; * `M-<number>' to complete using the line with that number.

;; TODO: more useful company-ispell, which only contains the cet-4 and cet-6
;; english words.

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
(setq company-abort-manual-when-too-short t)

(global-company-mode 1)
(company-quickhelp-mode 1)

;; borrowed from prucell emacs `lisp/init-company.el'
(define-key company-mode-map [remap completion-at-point] 'company-complete)
(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-or-complete-common)
(define-key company-mode-map (kbd "M-/") 'company-other-backend)

;; It seems that it is a bad idea to group some backends together, unless these
;; backends do similar jobs. Because different backends may generate different
;; types of candidates, and we cannot differentiate the source of candidates
;; from one to another. This may make `company-transformers' hard to implement.
(defun personal-company-get-backends (major-backend)
  `(,major-backend
    company-files
    (company-abbrev company-dabbrev company-dabbrev-code)
    company-semantic
    company-ispell
    company-yasnippet))

(defun personal-company-set-backends (hook backends)
  "Set `company-backends' to BACKENDS in HOOK."
  (add-hook hook (lambda () (setq company-backends backends))))

(defmacro personal-company-add-transformer (mode &rest body)
  "Add mode specific transformer to company mode."
  `(add-to-list
    'company-transformers
    (lambda (candidates)
      (if (and candidates (eq major-mode ,mode))
          (progn ,@body) candidates)) t))

(defun personal-company-allow-completion-p (&rest args)
  "Prevent completion in some certain situations."
  (let ((regex (rx (or (char alnum) "::" "-" "_" "@" "/" ".")))
        (limit (save-excursion (backward-char 2) (point))))
    (when (not (looking-back regex limit))
      (error "text before point does not meet requirement.")))
  (let ((regex (rx (or (char alnum)))))
    (when (looking-at-p regex)
      (error "text after point does not meet requirement."))))

(add-hook 'company-transformers #'personal-company-allow-completion-p)
(add-hook 'company-completion-started-hook #'personal-company-allow-completion-p)
(setq-default company-backends (personal-company-get-backends 'company-capf))

(provide 'personal-company)

;;; personal-company.el ends here
