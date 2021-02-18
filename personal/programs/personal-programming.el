;;; personal-programming.el --- Personal configuration for programming
;;; languages.

;;; Commentary:
;;; Personal configuration for programming languages.

;;; Code:


(require 'prelude-programming)

(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.prototxt\\'" . protobuf-mode))

(provide 'personal-programming)

;;; personal-programming.el ends here
