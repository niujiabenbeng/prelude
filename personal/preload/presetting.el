;;; -*- lexical-binding: t; -*-
;;; presetting.el --- Entry point of personal settings before prelude.

;;; Commentary:

;; Entry point of personal settings before prelude.

;;; Local Variables:
;;; eval: (flycheck-mode -1)
;;; End:

;; do not display the menu-bar and line numbers
(setq prelude-minimalistic-ui t)

;; do not use the Super key in keybindings
(setq prelude-super-keybindings nil)

;; do not use auto save
(setq prelude-auto-save nil)

;; disable warnings on arrow navigation.
(setq prelude-guru nil)

;; do not check the spelling on the fly.
(setq prelude-flyspell nil)

;; disable warnings on environment variable settings
(setq exec-path-from-shell-check-startup-files nil)

(defvar personal-start-neotree-after-init t
  "Start neotree after initialization.")

;;; presetting.el ends here
