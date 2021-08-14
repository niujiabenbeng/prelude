;;; -*- lexical-binding: t; -*-
;;; personal-cc.el --- Personal configuration for C/C++.

;;; Commentary:

;; Personal configuration for C/C++.

;; Use clangd as c++ language server. The binary files can be found in
;;          https://releases.llvm.org/download.html

;;; Code:

(prelude-require-package 'google-c-style)
(require 'cc-mode)
(require 'google-c-style)
(require 'personal-lsp)

;; add header files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; change the mode-name as normal, original: "C++//1", see:
;; https://www.reddit.com/r/emacs/comments/8v8de4/change_modename_for_major_mode/
(with-eval-after-load "cc-cmds" (defalias 'c-update-modeline #'ignore))

;; enable other modes in c++-mode
(add-hook 'c-mode-common-hook #'subword-mode)

;; google-c-style make [tab] behave properly. note that we should append
;; the fucntions at the end of hook to override prelude settings.
(add-hook 'c-mode-common-hook 'google-set-c-style t)
(add-hook 'c-mode-common-hook 'google-make-newline-indent t)

;; lsp mode configuration, see `clangd --help' for detailed information
(setq lsp-clients-clangd-args
      '("--all-scopes-completion"
        "--background-index"
        "--completion-style=detailed"
        "--cross-file-rename"
        "--fallback-style=Google"
        "--header-insertion=never"
        "--pch-storage=memory"  ; 机器内存较大的时候使用
        "--log=error"))
(add-hook 'c-mode-common-hook #'lsp-deferred)

;; disable flycheck-mode since we have an improved version of clang-tidy.
(add-hook 'lsp-configure-hook
          (lambda () (if (eq major-mode 'c++-mode) (flycheck-mode -1))) t)

(provide 'personal-cc)

;;; personal-cc.el ends here
