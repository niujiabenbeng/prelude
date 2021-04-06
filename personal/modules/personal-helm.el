;;; -*- lexical-binding: t; -*-
;;; personal-helm.el --- Additional helm settings.

;;; Commentary:

;; Additional helm settings after prelude-helm and prelude-helm-averywhere.

;;; Code:

(require 'helm)
(require 'helm-config)
(require 'helm-buffers)

;; retrieve the helm previous behavior, see link below for more info
;;   https://github.com/emacs-helm/helm/issues/2175
(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)
(customize-set-variable 'helm-ff-lynx-style-map t)
(customize-set-variable 'helm-imenu-lynx-style-map t)
(customize-set-variable 'helm-semantic-lynx-style-map t)
(customize-set-variable 'helm-occur-use-ioccur-style-keys t)
(customize-set-variable 'helm-window-prefer-horizontal-split t)

;; make tab completion available with helm, see:
;;   https://tuhdo.github.io/helm-intro.html
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; echo input in header line, see:
;;   https://emacs-china.org/t/helm-minibuffer/8884/5
(setq helm-echo-input-in-header-line t)
(add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

;; Use <C-o> for easy use of helm-occur
(global-set-key (kbd "C-o") 'helm-occur)
(global-set-key (kbd "C-h b") 'helm-descbinds)

;; add bookmark to `helm-mini-default-sources'
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks))

(defun personal-helm-recenter-window (&optional arg)
  "Recenter helm window."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window
      (recenter-top-bottom arg))))
(define-key helm-map (kbd "C-l") 'personal-helm-recenter-window)

;; avoid tab-completion pinging random websites, see:
;;   https://www.reddit.com/r/emacs/comments/fn85bk/tabcompletion_pinging_random_websites/
(setq ffap-machine-p-known 'reject)

(provide 'personal-helm)

;;; personal-helm.el ends here
