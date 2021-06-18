;;; -*- lexical-binding: t; -*-
;;; personal-python.el --- Personal configuration for Python.

;;; Commentary:

;; Personal configuration for Python.
;;
;; install python language server:
;;   `pip install 'python-lsp-server[all]'`
;;
;; we choose the following tools:
;;   1. jedi for completion & jumping
;;   2. yapf for auto formatting
;;   3. pylint for linting

;;; Code:

(require 'python)
(require 'personal-lsp)

(setq lsp-pylsp-plugins-mccabe-enabled nil)
(setq lsp-pylsp-plugins-pycodestyle-enabled nil)
(setq lsp-pylsp-plugins-pydocstyle-enabled nil)
(setq lsp-pylsp-plugins-pyflakes-enabled nil)
(setq lsp-pylsp-plugins-autopep8-enabled nil)
(setq lsp-pylsp-plugins-flake8-enabled nil)
(setq lsp-pylsp-plugins-yapf-enabled t)
(setq lsp-pylsp-plugins-pylint-enabled t)
(add-hook 'python-mode-hook #'lsp-deferred)

(provide 'personal-python)

;;; personal-python.el ends here
