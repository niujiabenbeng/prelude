;;; setting.el --- Entry point of personal settings.

;;; Commentary:
;;; Entry point of personal settings.

;;; Code:

(setenv "USE_EMACS" "1")

(add-to-list 'load-path (expand-file-name "modules"  prelude-personal-dir))
(add-to-list 'load-path (expand-file-name "packages" prelude-personal-dir))
(add-to-list 'load-path (expand-file-name "programs" prelude-personal-dir))

(prelude-require-packages
 '(neotree))

;;; Load prelude modules
(require 'prelude-helm)
(require 'prelude-helm-everywhere)
(require 'prelude-company)
(require 'prelude-key-chord)
(require 'prelude-org)
(require 'prelude-emacs-lisp)
(require 'prelude-xml)
(require 'prelude-yaml)

;;; Load personal configurations.
(require 'personal-putty-keys)
(require 'personal-keys)
(require 'personal-editor)
(require 'personal-dired)
(require 'personal-neotree)
(require 'personal-isearch)
(require 'personal-helm)
(require 'personal-helm-swoop)
(require 'personal-modeline)

;;; Programming languages support
(require 'personal-programming)
(require 'personal-cc)
(require 'personal-python)
(require 'personal-shell)
(require 'personal-emacs-lisp)

;;; setting.el ends here
