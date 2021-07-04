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

(defun personal-python-calc-candidate-score (candidate prefix)
  (let ((score 0))
    (when (s-starts-with-p "_" candidate)
      (setq score (- score 100)))
    (when (s-starts-with-p "__" candidate)
      (setq score (- score 100)))
    (when (s-starts-with-p prefix candidate)
      (setq score (+ score 1000)))
    (when (s-starts-with-p prefix candidate t)
      (setq score (+ score 1000)))
    score))

(personal-company-add-transformer
 'python-mode
 (let ((prefix (company-grab-symbol)))
   (setq prefix (or prefix ""))
   (cl-stable-sort
    candidates '> :key
    (lambda (x)
      (personal-python-calc-candidate-score
       (substring-no-properties x) prefix)))))

(provide 'personal-python)

;;; personal-python.el ends here
