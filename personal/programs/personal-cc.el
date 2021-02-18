;;; personal-cc.el --- Personal configuration for C/C++.

;;; Commentary:
;;; Personal configuration for C/C++.

;;; Code:

(prelude-require-package 'google-c-style)
(prelude-require-package 'clang-format)
(require 'prelude-c)
(require 'google-c-style)
(require 'clang-format)

;;; add header files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; change the mode-name as normal, original: "C++//1", see:
;;; https://www.reddit.com/r/emacs/comments/8v8de4/change_modename_for_major_mode/
(with-eval-after-load "cc-cmds" (defalias 'c-update-modeline #'ignore))

;;; enable other modes in c++-mode
(add-hook 'c++-mode-hook #'subword-mode)

;;; google-c-style make [tab] behave properly. note that we should append
;;; the fucntions at the end of hook to override prelude settings.
(add-hook 'c-mode-common-hook 'google-set-c-style t)
(add-hook 'c-mode-common-hook 'google-make-newline-indent t)

(cond ((eq personal-c++-conf 'dumb)
       (require 'personal-cc-dumb))
      ((eq personal-c++-conf 'native)
       (require 'personal-irony))
      ((eq personal-c++-conf 'lsp)
       (require 'personal-lsp)
       (require 'personal-cc-lsp))
      (t (error "Wrong value of `personal-c++-conf'")))

(provide 'personal-cc)

;;; personal-cc.el ends here
