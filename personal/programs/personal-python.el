;;; personal-python.el --- Personal configuration for Python.

;;; Commentary:
;;; Personal configuration for Python.

;;; Code:

(require 'python)
(require 'prelude-python)

(defun personal-set-python-interpreter ()
  "Set python interpreter based on `PYTHONENV'."
  (let ((env (getenv "PYTHONENV")))
    (when (and env (file-executable-p env))
      (setq pythonic-interpreter env)
      (setq python-shell-interpreter env)
      (pythonic-activate pythonic-interpreter))))
(add-hook 'python-mode-hook 'personal-set-python-interpreter)

(cond ((eq personal-python-conf 'native)
       (require 'personal-python-native))
      ((eq personal-python-conf 'lsp)
       (require 'personal-lsp)
       (require 'personal-python-lsp))
      (t (error "Wrong value of `personal-python-conf'")))

(provide 'personal-python)

;;; personal-python.el ends here
