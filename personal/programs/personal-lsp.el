;;; -*- lexical-binding: t; -*-
;;; personal-lsp.el --- Personal configuration of lsp.

;;; Commentary:

;; Personal configuration of lsp for various languages.
;; Emacs client for the Language Server Protocol:
;;   https://github.com/emacs-lsp/lsp-mode#supported-languages

;;; Code:

(setq lsp-keymap-prefix "C-c l"
      lsp-keep-workspace-alive nil
      lsp-signature-auto-activate nil
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-modeline-workspace-status-enable nil

      lsp-auto-guess-root t
      lsp-idle-delay 0.1
      lsp-response-timeout 5
      lsp-ui-doc-enable nil
      lsp-enable-snippet t

      lsp-enable-file-watchers nil
      lsp-enable-folding nil
      lsp-enable-semantic-highlighting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-text-document-color nil
      lsp-enable-links nil
      lsp-headerline-breadcrumb-enable nil

      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil)

(setq lsp-ui-doc-max-height 8
      lsp-ui-doc-max-width 35
      lsp-ui-sideline-ignore-duplicate t
      lsp-ui-peek-enable t
      lsp-ui-imenu-enable t
      lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-doc-show-with-mouse nil
      lsp-ui-doc-position 'at-point
      lsp-ui-sideline-show-hover nil)

(prelude-require-packages '(lsp-mode lsp-ui))
(require 'lsp-mode)
(require 'lsp-ui)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(defun personal-lsp-find-thing-at-point ()
  "Find file or function definition at point."
  (interactive)
  (let ((files (personal--fcf-get-files-at-point))
        (symbol (symbol-at-point)) finish)
    (when files
      (xref-push-marker-stack)
      (find-file (car files))
      (setq finish t))
    (when (and (not finish) (not symbol))
      ;; use `error' to prevent jumping to other window in
      ;; `personal-lsp-find-thing-at-point-other-window'
      (error "Not found symbol at point.")
      (setq finish t))
    (when (and (not finish) (eq major-mode 'emacs-lisp-mode))
      (elisp-slime-nav-find-elisp-thing-at-point (symbol-name symbol))
      (setq finish t))
    (when (not finish)
      (lsp-ui-peek-find-definitions))))

(defun personal-lsp-find-thing-at-point-other-window ()
  "Find file or function definition at point and show other window."
  (interactive)
  (personal-display-result-other-window (personal-lsp-find-thing-at-point)))

(defun personal-lsp-pop-marker-stack-other-window ()
  "Show previous marker position in other window."
  (interactive)
  (personal-display-result-other-window (xref-pop-marker-stack)))

(let ((map personal-mode-map))
  (define-key map (kbd "M-.")   #'personal-lsp-find-thing-at-point)
  (define-key map (kbd "C-M-.") #'personal-lsp-find-thing-at-point-other-window)
  (define-key map (kbd "C-M-,") #'personal-lsp-pop-marker-stack-other-window))

;; set backends for all programming languages.
;; note `lsp-completion' modifies variable `company-backends'.
;; we should set `company-backends' after lsp is initialized.
(personal-company-set-backends
 'lsp-after-open-hook
 (personal-company-get-backends 'company-capf))

(provide 'personal-lsp)

;;; personal-lsp.el ends here
