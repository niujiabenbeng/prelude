;;; -*- lexical-binding: t; -*-
;;; personal-neotree.el --- neotree configuration.

;;; Commentary:

;; personal neotree configuration.

;;; Code:

(require 'neotree)
(require 'projectile)
(require 'personal-windows)

(defvar personal-neotree-hidden-regexp-list
  '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "__pycache__" "\\.d$")
  "The regexp list matching hidden files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-neotree-get-ignore-file ()
  "If `.neoignore' is found, return the full path, else return nil."
  (let ((filename (buffer-file-name)) dirname)
    ;; if we are in neotree buffer, use the root directory of neotree window
    (and (eq (selected-window) (neo-global--get-window))
         (file-directory-p neo-buffer--start-node)
         (setq dirname neo-buffer--start-node))
    ;; if dirname is nil, use `protjectile-project-root'
    (when (not dirname) (setq dirname (projectile-project-root)))
    ;; if dirname is nil, search `.neoignore' through the path hierarchy
    (when (and filename (not dirname))
      (setq dirname (locate-dominating-file filename ".neoignore")))
    ;; if dirname is nil, use the value of `default-directory'
    (setq filename (expand-file-name ".neoignore" dirname))
    (if (file-exists-p filename) filename nil)))

(defun personal-neotree-set-hidden-regexp-list ()
  "Set `neo-hidden-regexp-list' based on ignore file."
  (setq neo-hidden-regexp-list personal-neotree-hidden-regexp-list)
  (when-let ((ignore-file (personal-neotree-get-ignore-file)))
    (setq neo-hidden-regexp-list
          (with-temp-buffer
            (insert-file-contents ignore-file)
            (split-string (buffer-string))))))

(defun personal-neotree-project-dir ()
  "Open NeoTree using the projectile root."
  (interactive)
  (let ((window (selected-window))
        (path (buffer-file-name)) name)
    (neotree-toggle)
    (personal-neotree-set-hidden-regexp-list)
    (when (and path (neo-global--window-exists-p))
      (setq name (file-name-nondirectory path))
      ;; show hidden file when current file is hidden.
      (neo-buffer--set-show-hidden-file-p
       (seq-filter (lambda (x) (string-match-p x name))
                   neo-hidden-regexp-list))
      (neotree-find (file-truename path)))
    ;; often when we open neotree, we only want to see directory tree.
    ;; here we jump back to the original window.
    (select-window window)))

(defun personal-neotree-parent-root ()
  "Set parent directory as neotree root."
  (interactive)
  (goto-char (point-min))
  (neotree-select-up-node)
  (personal-neotree-set-hidden-regexp-list)
  (neo-buffer--set-show-hidden-file-p nil)
  (neotree-next-line)
  (neo-point-auto-indent))

(defun personal-neotree-goto-other (arg)
  "Move to previous/next file/directory other than current one."
  (setq arg (if (< arg 0) -1 1))
  (let ((curr (neo-buffer--get-filename-current-line)) type)
    (if (null curr)
        (forward-line arg)
      (setq type (file-regular-p curr))
      (while (and curr
                  (eq type (file-regular-p curr))
                  (= (forward-line arg) 0))
        (setq curr (neo-buffer--get-filename-current-line)))))
  (neo-buffer--post-move)
  (neo-point-auto-indent))

(defun personal-neotree-previous-other ()
  "Goto the previous file/directory other than current one."
  (interactive)
  (personal-neotree-goto-other -1))

(defun personal-neotree-next-other ()
  "Goto the next file/directory other than current one."
  (interactive)
  (personal-neotree-goto-other 1))

(defun personal-neotree-open-file-left-window (full-path &optional arg)
  "Open file in window on the left side."
  (windmove-right)
  (find-file full-path))

(defun personal-neotree-open-file-right-window (full-path &optional arg)
  "Open file in window on the right side."
  (select-window (personal-neotree-right-window))
  (find-file full-path))

(defun personal-neotree-view-file-left-window (full-path &optional arg)
  "View file in window on the left side."
  (personal-neotree-open-file-left-window full-path)
  (neo-global--select-window))

(defun personal-neotree-view-file-right-window (full-path &optional arg)
  "View file in window on the right side."
  (personal-neotree-open-file-right-window full-path)
  (neo-global--select-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq neo-theme 'ascii)
(setq neo-smart-open t)
(setq neo-window-width 24)
(setq neo-window-fixed-size nil)
(setq neo-show-updir-line nil)
(setq neo-auto-indent-point t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(global-set-key [f4] 'personal-neotree-project-dir)

;; make convinient key-bindings. here lists all commonly used commands.
(let ((map neotree-mode-map))
  (define-key map (kbd "{")       'shrink-window-horizontally)
  (define-key map (kbd "}")       'enlarge-window-horizontally)
  (define-key map (kbd "g")       'neotree-refresh)
  (define-key map (kbd "a")       'neotree-collapse-all)
  (define-key map (kbd "h")       'neotree-hidden-file-toggle)
  (define-key map [(ctrl up)]     'personal-neotree-previous-other)
  (define-key map [(ctrl down)]   'personal-neotree-next-other)
  (define-key map (kbd "C-c d +") 'neotree-create-node)
  (define-key map (kbd "C-c d C") 'neotree-copy-node)
  (define-key map (kbd "C-c d D") 'neotree-delete-node)
  (define-key map (kbd "C-c d R") 'neotree-rename-node)
  (define-key map (kbd "C-c d d") 'neotree-dir))

(let ((map neotree-mode-map))
  (define-key map (kbd "TAB")
    (neotree-make-executor
     :file-fn 'personal-neotree-view-file-left-window
     :dir-fn  'neo-open-dir))
  (define-key map (kbd "RET")
    (neotree-make-executor
     :file-fn 'personal-neotree-open-file-left-window
     :dir-fn  'neo-open-dir))
  (define-key map [(ctrl tab)]
    (neotree-make-executor
     :file-fn 'personal-neotree-view-file-right-window
     :dir-fn  'neo-open-dir))
  (define-key map [(ctrl return)]
    (neotree-make-executor
     :file-fn 'personal-neotree-open-file-right-window
     :dir-fn  'neo-open-dir)))

(provide 'personal-neotree)

;;; personal-neotree.el ends here
