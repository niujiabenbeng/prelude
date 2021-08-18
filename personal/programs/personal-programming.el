;;; -*- lexical-binding: t; -*-
;;; personal-programming.el --- general settings of programming languages.

;;; Commentary:

;; Personal configuration for programming languages.

;;; Code:

(require 'prelude-programming)

(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

(add-hook 'prog-mode-hook 'subword-mode)

;; set backends for cmake mode
(personal-company-set-backends
 'cmake-mode-hook
 (personal-company-get-backends 'company-cmake))

;; indent json file with 4 whitespaces
(setq json-encoding-default-indentation "    ")

(provide 'personal-programming)

;;; personal-programming.el ends here
