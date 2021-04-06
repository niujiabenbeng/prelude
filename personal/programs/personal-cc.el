;;; -*- lexical-binding: t; -*-
;;; personal-cc.el --- Personal configuration for C/C++.

;;; Commentary:

;; Personal configuration for C/C++.

;;; Code:

(prelude-require-package 'google-c-style)
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

(defun personal-cc-lsp-setup ()
  "Set C++ environment based on `CLNAGDEXEC'."
  (setq lsp-clients-clangd-executable
        (or (getenv "CLANGDEXEC")
            (executable-find "clangd")
            (error "clangd not found.")))
  ;; clangd提供了自动插入头文件的选项, 但是我习惯将经常使用的头文件放入一
  ;; 个文件common.h中, 其他文件只需要include "common.h"即可. 所以这里设置
  ;; `-header-insertion=never'
  (setq lsp-clients-clangd-args
        '("-background-index" "-log=error" "-clang-tidy"
          "-fallback-style=Google" "-header-insertion=never"
          "-completion-style=detailed"))
  ;; 通过检查有没有clangd要求的配置文件来确定是否开启lsp
  (when-let ((file (buffer-file-name)))
    (when (or (locate-dominating-file file "compile_commands.json")
              (locate-dominating-file file "compile_flags.txt"))
      (lsp-deferred))))

(add-hook 'c-mode-common-hook #'personal-cc-lsp-setup)
(add-hook 'c-mode-common-hook #'personal-yasnippet-all)

(provide 'personal-cc)

;;; personal-cc.el ends here
