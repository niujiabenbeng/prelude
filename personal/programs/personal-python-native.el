;;; personal-python-native.el --- Personal configuration for Python.

;;; Commentary:
;;; Personal configuration for Python.

;;; Code:

(require 'python)
(require 'prelude-python)

(defun personal-python-pop-marker-stack ()
  "Show previous marker position in other window."
  (interactive)
  (personal-display-result-other-window
   (xref-pop-marker-stack)))

(defun personal-setup-python-env ()
  ;; do not indent when using yasnippet.
  ;; see https://github.com/joaotavora/yasnippet/issues/485
  (set (make-local-variable 'yas-indent-line) 'fixed)
  ;; fix error: "global name 'FileNotFoundError' is not defined"
  ;; see: https://github.com/pythonic-emacs/anaconda-mode
  (setq anaconda-mode-localhost-address "localhost")
  ;; show function parameters in the echo area
  (anaconda-eldoc-mode +1)
  ;; local key bindings
  (let ((map anaconda-mode-map))
    (define-key map (kbd "C-M-.") 'anaconda-mode-find-definitions-other-window)
    (define-key map (kbd "C-M-=") 'anaconda-mode-find-assignments-other-window)
    (define-key map (kbd "C-M-r") 'anaconda-mode-find-references-other-window)
    (define-key map (kbd "C-M-,") 'personal-python-pop-marker-stack)))
(add-hook 'python-mode-hook 'personal-setup-python-env t)

;;; use yapf as python formatter.
(prelude-require-package 'yapfify)
(require 'yapfify)

(defun personal-set-python-yapf ()
  "Set yapf based on `YAPFEXEC'."
  (let ((env (getenv "YAPFEXEC")))
    (when (and env (file-executable-p env))
      (setq yapfify-executable env)
      (yapf-mode +1))))
;; (add-hook 'python-mode-hook 'personal-set-python-yapf)

(provide 'personal-python-native)

;;; personal-python-native.el ends here
