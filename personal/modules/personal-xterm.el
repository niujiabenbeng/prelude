;;; personal-xterm.el --- xterm configuration.

;;; Commentary:
;;; personal xterm configuration.

;;; Code:

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")
(xterm-mouse-mode 1)
(mwheel-install)

(provide 'personal-xterm)

;;; personal-xterm.el ends here
