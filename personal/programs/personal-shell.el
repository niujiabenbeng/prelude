;;; -*- lexical-binding: t; -*-
;;; personal-shell.el --- Personal configuration for shell script.

;;; Commentary:

;; Personal configuration for shell script.

;;; Code:

(require 'comint)
(require 'prelude-shell)

;; prevent shell echoing
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)
(add-hook 'shell-mode-hook #'turn-off-auto-fill)

;; always indent 4 spaces after continuation
(setq-default sh-basic-offset 4)
(setq sh-indent-after-continuation 'always)

;; sh-mode imenu支持变量
(defun my-shell-mode-setup-imenu ()
  (setq imenu-generic-expression
        (append '(("Variable" "^\\([[:alnum:]_]+\\)=.*" 1))
                (nthcdr 1 (car sh-imenu-generic-expression)))))
(add-hook 'sh-mode-hook 'my-shell-mode-setup-imenu)

;; set company backends
(personal-company-set-backends
 'sh-mode-hook
 '((company-abbrev company-dabbrev company-dabbrev-code)
   (company-files company-capf company-yasnippet)))

(provide 'personal-shell)

;;; personal-shell.el ends here
