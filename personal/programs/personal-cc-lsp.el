;;; personal-cc-lsp.el --- c++ configuration based on lsp.

;;; Commentary:
;;; c++ configuration based on lsp.

;;; Code:

(require 'personal-lsp)

(defun personal-c++-lsp-setup ()
  "Set C++ environment based on `CLNAGDEXEC'."
  (setq lsp-clients-clangd-executable
        (or (getenv "CLANGDEXEC")
            (executable-find "clangd")
            (error "clangd not found.")))
  ;; clangd提供了自动插入头文件的选项, 但是我习惯将经常使用的头文件放入一个文件
  ;; common.h中, 其他文件只需要include "common.h"即可. 所以这里设置
  ;; `-header-insertion=never'
  (setq lsp-clients-clangd-args
        '("-background-index" "-log=error" "-clang-tidy"
          "-fallback-style=Google" "-header-insertion=never"
          "-completion-style=detailed"))
  (lsp-deferred))
(add-hook 'c-mode-hook #'personal-c++-lsp-setup)
(add-hook 'c++-mode-hook #'personal-c++-lsp-setup)

(provide 'personal-cc-lsp)

;;; personal-cc-lsp.el ends here
