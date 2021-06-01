;;; -*- lexical-binding: t; -*-
;;; personal-editor.el --- Enhancement of prelude editor.

;;; Commentary:

;; Enhance some espects of prelude editor to meet personal needs.

;;; Code:

;; set max length of a line.
(setq whitespace-line-column 80)

;; globally enable auto-fill-mode.
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column whitespace-line-column)

;; start emacs with full screen.
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; immediately echo the key sequence.
(setq echo-keystrokes 0.01)

;; set default tab width.
(setq-default tab-width 4)

;; ediff settings.
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; disable scratch message for emacs client.
(setq initial-scratch-message "\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq debug-on-error t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
")

;; keep cursor 2 lines away from the edge of screen.
;; (setq scroll-margin 2)

;; set yasnippet, do not use yasnippet-snippets.
(require 'yasnippet)
(setq yas-snippet-dirs
      (list (expand-file-name "snippets" prelude-personal-dir)))
(yas-global-mode 1)

;; enable hs-minor-mode for all programming modes.
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; whitespace-mode has some bugs on tab and indentation. but it is not to be
;; disabled because it provides some other useful features. here we make the
;; background color of whitespace-mode the same as the background of
;; color-theme, so that it is not visiable.
(let ((color (face-attribute 'default :background)))
  ;; (set-face-background 'whitespace-trailing color)
  ;; (set-face-background 'whitespace-space color)
  ;; (set-face-background 'whitespace-indentation color)
  ;; (set-face-background 'whitespace-space-after-tab color)
  (set-face-background 'whitespace-empty color))

;; bring back some colors previously disabled by zenburn
(set-face-background 'hl-line "#505050")
(set-face-background 'vhl/default-face "black")
(set-face-foreground 'anzu-replace-to "red")
(set-face-background 'rectangle-preview "black")

;; projectile uses `alien' as its default indexing method, which uses gitignore
;; to ignore files and directories in a project. However, `projectile-grep'
;; does not respect gitignore unless `projectile-use-git-grep' is set to t.
(setq projectile-use-git-grep t)

;; solve the issue: `helm-M-x' produces duplicates items.
;; see: https://github.com/emacs-helm/helm/issues/2291
(setq history-delete-duplicates t)

;; do not kill old compile processes
(setq compilation-always-kill nil)

;; prelude advised `yank' and `yank-pop' to automatically indent yanked text,
;; but it is not properly behaved in some major modes such as `makefile-mode'.
;; here we remove the advice.
(ad-deactivate #'yank)
(ad-deactivate #'yank-pop)

;; move window in a loop manner
(setq windmove-wrap-around t)

(provide 'personal-editor)

;;; personal-editor.el ends here
