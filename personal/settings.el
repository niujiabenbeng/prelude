;;; setting.el --- Entry point of personal settings.

;;; Commentary:
;;; Entry point of personal settings.

;;; Code:

(setenv "USE_EMACS" "1")

(add-to-list 'load-path (expand-file-name "lisp"     prelude-personal-dir))
(add-to-list 'load-path (expand-file-name "modules"  prelude-personal-dir))
(add-to-list 'load-path (expand-file-name "packages" prelude-personal-dir))
(add-to-list 'load-path (expand-file-name "programs" prelude-personal-dir))

(prelude-require-packages '(s neotree psession dash yasnippet))

(defun personal-get-package-version (package)
  (when-let (info (alist-get package package-alist))
    (package-version-join (package-desc-version (car info)))))

(defun personal-check-package-version (package version-required)
  (if-let ((version (personal-get-package-version package)))
      (when (string-version-lessp version version-required)
        (error "package '%s' is too old" package))
    (error "do not find package '%s'" package)))

(personal-check-package-version 's "20210616.619")
(personal-check-package-version 'dash "20210704.1302")
(personal-check-package-version 'neotree "20200324.1946")
(personal-check-package-version 'psession "20210203.828")

;;; Load prelude modules
(require 'prelude-helm)
(require 'prelude-helm-everywhere)
(require 'prelude-key-chord)
(require 'prelude-org)
(require 'prelude-xml)
(require 'prelude-yaml)

;;; Load personal configurations.
(require 'personal-putty-keys)
(require 'personal-company)
(require 'personal-mode)
(require 'personal-keys)
(require 'personal-editor)
(require 'personal-neotree)
(require 'personal-isearch)
(require 'personal-helm)
(require 'personal-helm-swoop)
(require 'personal-dired)
(require 'personal-modeline)
(require 'personal-entry)

;;; Programming languages support
(require 'personal-programming)
(require 'personal-cc)
(require 'personal-python)
(require 'personal-shell)
(require 'personal-emacs-lisp)

;;; Other functionalities
(require 'cc-adjust-header-guard)
(require 'cc-change-name-style)
(require 'modeline-show-all-minor-mode)

;;; setting.el ends here
