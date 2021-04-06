;;; -*- lexical-binding: t; -*-
;;; personal-programming.el --- general settings of programming languages.

;;; Commentary:

;; Personal configuration for programming languages.

;;; Code:

(require 'prelude-programming)

(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;; https://github.com/syl20bnr/spacemacs/pull/179
(defun personal-yasnippet-all ()
  "Enable yasnippet for all backends."
  (setq
   company-backends
   (mapcar
    (lambda (backend)
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (cl-remove-duplicates company-backends))))

(add-hook 'prog-mode-hook 'subword-mode)

(provide 'personal-programming)

;;; personal-programming.el ends here
