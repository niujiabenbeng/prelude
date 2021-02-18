;;; personal-helm-swoop.el --- A replacement of helm-occur.

;;; Commentary:
;;; A replacement of helm-occur for better experience.

;;; Code:

(prelude-require-package 'helm-swoop)
(require 'helm-config)
(require 'helm-swoop)

;; Rebind helm-occur to helm-swoop
(global-set-key (kbd "C-o") 'helm-swoop)
(define-key helm-command-map (kbd "o") 'helm-swoop)
(define-key isearch-mode-map (kbd "C-o") 'helm-swoop-from-isearch)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)
;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)
;; Optional face for line numbers. Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)
;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

(provide 'personal-helm-swoop)

;;; personal-helm-swoop.el ends here
