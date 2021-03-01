;;; personal-python.el --- Personal configuration for Python.

;;; Commentary:

;; Personal configuration for Python.

;;; Code:

(require 'python)
(require 'personal-lsp)

(defun personal-python-lsp-get-pylint-init-hook (project-root init-file)
  "Return a piece of python snippet as pylint init-hook."
  (let ((init-hook ""))
    ;; 如果project-root存在, 则将project-root放到sys.path中
    (when (and project-root (file-exists-p project-root))
      (setq init-hook (concat init-hook (format "\
import sys
sys.path.append(\"%s\")
" (expand-file-name project-root)))))
    ;; 如果init-file存在, 则加载init-file的内容
    (when (and init-file (file-exists-p init-file))
      (setq init-hook (concat init-hook (format "\
import importlib.util
spec = importlib.util.spec_from_file_location(\"init\", \"%s\")
foo = importlib.util.module_from_spec(spec)
spec.loader.exec_module(foo)
" (expand-file-name init-file)))))
    init-hook))

(defun personal-python-lsp-enable-pylint ()
  "Enable pylint in lsp."
  (setq lsp-pyls-plugins-pylint-enabled t)
  ; make pylint respect subdirs and dynamic import
  (when-let* ((curr (buffer-file-name))
              (root (locate-dominating-file curr ".pylintrc"))
              (conf (expand-file-name ".pylintrc" root))
              (args (list (format "--rcfile=%s" conf))))
    (setq curr (file-name-directory curr))
    (push (format
           "--init-hook='%s'"
           (personal-python-lsp-get-pylint-init-hook
            root (expand-file-name "init.py" curr))) args)
    (setq lsp-pyls-plugins-pylint-args (vconcat args))))

(defun personal-python-lsp-setup ()
  "Set python language server based on `PYLSEXEC'."
  (let ((env (getenv "PYLSEXEC")))
    (if (and env (file-executable-p env))
        (setq lsp-pyls-server-command env)
      (error "python-language-server is not installed.")))
  (setq lsp-pyls-plugins-mccabe-enabled nil)
  (setq lsp-pyls-plugins-pylint-enabled nil)
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq lsp-pyls-plugins-pydocstyle-enabled nil)
  (setq lsp-pyls-plugins-pyflakes-enabled nil)
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled t)
  (setq lsp-pyls-plugins-flake8-enabled nil)
  (personal-python-lsp-enable-pylint)
  (lsp-deferred))

(add-hook 'python-mode-hook #'personal-python-lsp-setup)
(add-hook 'python-mode-hook #'personal-yasnippet-all)

(provide 'personal-python)

;;; personal-python.el ends here
