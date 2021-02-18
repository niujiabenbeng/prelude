;;; personal-helm.el --- Additional helm settings.

;;; Commentary:
;;; Additional helm settings after prelude-helm and prelude-helm-averywhere.

;;; Code:

(require 'helm)
(require 'helm-config)
(require 'helm-buffers)

;;; retrieve the helm previous behavior, see link below for more info
;;;   https://github.com/emacs-helm/helm/issues/2175
(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)
(customize-set-variable 'helm-ff-lynx-style-map t)
(customize-set-variable 'helm-imenu-lynx-style-map t)
(customize-set-variable 'helm-semantic-lynx-style-map t)
(customize-set-variable 'helm-occur-use-ioccur-style-keys t)
(customize-set-variable 'helm-window-prefer-horizontal-split t)

;;; make tab completion available with helm, see:
;;;   https://tuhdo.github.io/helm-intro.html
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;;; echo input in header line, see:
;;;   https://emacs-china.org/t/helm-minibuffer/8884/5
(setq helm-echo-input-in-header-line t)
(add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

;;; Use <C-o> for easy use of helm-occur
(global-set-key (kbd "C-o") 'helm-occur)
(global-set-key (kbd "C-h b") 'helm-descbinds)

;;; add bookmark to `helm-mini-default-sources'
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

;;; disable some keybindings in helm-map which are used rarely.
(let ((map helm-command-map))
  ;; <prefix C-c SPC> run: helm-all-mark-rings, alternative: <prefix SPC>
  ;; <prefix C-c f>   run: helm-recentf, alternative: <C-c c f>
  ;; <prefix C-c g>   run: helm-google-suggest
  ;; <prefix C-c w>   run: helm-wikipedia-suggest
  ;; <prefix C-c C-x> run: helm-run-external-command
  (define-key map (kbd "C-c") nil)
  ;; <prefix C-x C-b> run: helm-buffers-list, alternative: <C-x C-b>
  ;; <prefix C-x C-f> run: helm-find-files, alternative: <C-x C-f>
  ;; <prefix C-x r b> run: helm-filtered-bookmarks
  ;; <prefix C-x r i> run: helm-register
  (define-key map (kbd "C-x") nil)
  ;; <prefix M-g a>   run: helm-do-grep-ag
  ;; <prefix M-g i>   run: helm-gid
  (define-key map (kbd "M-g") nil)
  ;; <prefix M-s o>   run: helm-occur, alternative: <C-o>, <prefix o>
  (define-key map (kbd "M-s") nil)
  ;; <ESC> is the prefix of Ctrl/Meta key
  (define-key map (kbd "ESC") nil)
  ;; <prefix h g>     run: helm-info-gnus
  ;; <prefix h h>     run: helm-documentation
  ;; <prefix h i>     run: helm-info-at-point
  ;; <prefix h r>     run: helm-info-emacs
  (define-key map (kbd "h") nil)
  ;; below is some ordinary keybindings
  (define-key map (kbd "8")   nil) ;; run: helm-ucs (unicode support)
  (define-key map (kbd "r")   nil) ;; run: helm-regexp, too slow
  (define-key map (kbd "/")   nil) ;; run: helm-find, alternative: <C-c p f>
  (define-key map (kbd "F")   nil) ;; run: helm-select-xfont
  (define-key map (kbd "a")   nil) ;; run: helm-apropos, alternative: <C-h f>
  (define-key map (kbd "m")   nil) ;; run: helm-man-woman
  (define-key map (kbd "p")   nil) ;; run: helm-list-emacs-process
  (define-key map (kbd "s")   nil) ;; run: helm-surfraw
  (define-key map (kbd "t")   nil) ;; run: helm-top
  (define-key map (kbd "C-:") nil) ;; run: helm-eval-expression-with-eldoc
  ;; rebind some keys for consistency
  (define-key map (kbd "C-,") nil) ;; run: helm-calcul-expression
  (define-key map (kbd ",") 'helm-calcul-expression))

(provide 'personal-helm)

;;; personal-helm.el ends here
