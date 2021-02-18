;;; personal-irony.el --- c++ code completion based on irony.

;;; Commentary:
;;; c++ code completion based on irony.

;;; Code:

(prelude-require-package 'irony)
(require 'irony)

(add-hook 'c++-mode-hook   'irony-mode)
(add-hook 'c-mode-hook     'irony-mode)
(add-hook 'objc-mode-hook  'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;; company-irony & company-irony-c-headers
(prelude-require-package 'company-irony)
(prelude-require-package 'company-irony-c-headers)
(require 'company-irony)
(require 'company-irony-c-headers)
(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-irony-c-headers company-irony)))

;;; flycheck-irony
(prelude-require-package 'flycheck-irony)
(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'personal-irony)

;;; personal-irony.el ends here
