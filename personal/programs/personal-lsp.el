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
      lsp-ui-doc-enable nil
      lsp-ui-doc-show-with-mouse nil
      lsp-ui-doc-position 'at-point
      lsp-ui-sideline-show-hover nil)

(prelude-require-packages '(lsp-mode lsp-ui))
(require 'lsp-mode)
(require 'lsp-ui)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(let ((map lsp-ui-mode-map))
  (define-key map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key map [remap xref-find-references] #'lsp-ui-peek-find-references))

(provide 'personal-lsp)

;;; personal-lsp.el ends here
