;;; personal-neotree.el --- neotree configuration.

;;; Commentary:

;; personal neotree configurations.

;;; Code:

(prelude-require-package 'neotree)
(require 'neotree)
(require 'projectile)

(defvar personal-neotree-hidden-regexp-list
  '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "__pycache__" "\\.d$")
  "The regexp list matching hidden files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-neotree-default-windows ()
  "Return default window configuration.

Default configuration: [neotree|left|right], where neotree is optional.
No horizontal window exists."
  (interactive)
  (let ((neo (neo-global--get-window))
        (cur (selected-window))
        left right)
    ;; make current window at the top side
    (while (not (window-at-side-p cur 'top))
      (delete-window (windmove-find-other-window 'up)))
    ;; delete all live windows not at the top side
    (let (win-to-del)
      (walk-window-tree
       (lambda (win)
         (unless (window-at-side-p win 'top)
           (setq win-to-del (cons win win-to-del)))))
      (dolist (win win-to-del) (delete-window win)))
    ;; traverse neotree, left & right window
    (select-window (frame-first-window))
    (when (and neo (not (eq neo (selected-window))))
      (error "Neotree window should be at the left side."))
    ;; neotree assures that there is a window on the right
    (when neo (windmove-right))
    (setq left (selected-window))
    (select-window
     (or (windmove-find-other-window 'right)
         (split-window-right)))
    (setq right (selected-window))
    ;; remove redundant horizontal windows
    (let ((rest (windmove-find-other-window 'right)))
      (while rest
        (if (eq cur right)
            (delete-window rest)
          (delete-window right)
          (select-window rest)
          (setq right rest))
        (setq rest (windmove-find-other-window 'right))))
    (balance-windows)
    (select-window cur)
    (list :cur cur :neo neo :left left :right right)))

(defun personal-neotree-other-window (&optional windows)
  "Return window other than the selected from default window configuration."
  (setq windows (or windows (personal-neotree-default-windows)))
  (let ((left    (plist-get windows :left))
        (right   (plist-get windows :right))
        (current (plist-get windows :cur)))
    (if (eq left current) right left)))

(defun personal-neotree-left-window (&optional windows)
  "Return window on the left side from default window configuration."
  (setq windows (or windows (personal-neotree-default-windows)))
  (plist-get windows :left))

(defun personal-neotree-right-window (&optional windows)
  "Return window on the right side from default window configuration."
  (setq windows (or windows (personal-neotree-default-windows)))
  (plist-get windows :right))

(defmacro personal-display-result (winpos &rest body)
  "Execute BODY and display result in window specified by WINPOS."
  `(let (buf pos)
     (save-window-excursion
       (save-excursion
         (progn ,@body)
         (setq buf (current-buffer) pos (point))))
     (cond
      ((eq ,winpos 'left)
       (select-window (personal-neotree-left-window)))
      ((eq ,winpos 'right)
       (select-window (personal-neotree-right-window)))
      ((eq ,winpos 'other)
       (select-window (personal-neotree-other-window)))
      (t (error "Unknown window position: %s" winpos)))
     (switch-to-buffer buf)
     (goto-char pos)))

(defmacro personal-display-result-left-window (&rest body)
  "Execute BODY and display result in the left side window."
  `(personal-display-result 'left ,@body))

(defmacro personal-display-result-right-window (&rest body)
  "Execute BODY and display result in the right side window."
  `(personal-display-result 'right ,@body))

(defmacro personal-display-result-other-window (&rest body)
  "Execute BODY and display result in window other than the selected one."
  `(personal-display-result 'other ,@body))

(defun personal-display-path ()
  "Display path in echo area."
  (interactive)
  (cond ((eq (current-buffer) (neo-global--get-buffer))
         (message (neo-buffer--get-filename-current-line)))
        (t (message (buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; local functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-neotree-get-ignore-file ()
  "If `.neoignore' is found, return the full path, else return nil."
  (let ((filename (buffer-file-name)) dirname)
    ;; if we are in neotree buffer, use the root directory of neotree window
    (and (eq (select-window) (neo-global--get-window))
         (file-directory-p neo-buffer--start-node)
         (setq dirname neo-buffer--start-node))
    ;; if dirname is nil, use `protjectile-project-root'
    (when (not dirname) (setq dirname (projectile-project-root)))
    ;; if dirname is nil, search `.neoignore' through the directory
    (when (and filename (not dirname))
      (setq dirname (locate-dominating-file filename ".neoignore")))
    ;; if dirname is nil, use the value of `default-directory'
    (setq filename (expand-file-name ".neoignore" dirname))
    (if (file-exists-p filename) filename nil)))

(defun personal-neotree-set-hidden-regexp-list ()
  "Set `neo-hidden-regexp-list' based on ignore file."
  (setq neo-hidden-regexp-list personal-neotree-hidden-regexp-list)
  (let* ((ignore-file (personal-neotree-get-ignore-file)))
    (setq neo-hidden-regexp-list
          (with-temp-buffer
            (insert-file-contents file)
            (split-string (buffer-string))))))

(defun personal-neotree-project-dir ()
  "Open NeoTree using the projectile root."
  (interactive)
  (let ((path (buffer-file-name)) name)
    (neotree-toggle)
    (personal-neotree-set-hidden-regexp-list)
    (when (and path (neo-global--window-exists-p))
      (setq name (file-name-nondirectory path))
      ;; show hidden file when current file is hidden.
      (neo-buffer--set-show-hidden-file-p
       (seq-filter (lambda (x) (string-match-p x name))
                   neo-hidden-regexp-list))
      (neotree-find (file-truename path)))))

(defun personal-neotree-parent-root ()
  "Set parent directory as neotree root."
  (interactive)
  (goto-char (point-min))
  (neotree-select-up-node)
  (personal-neotree-set-hidden-regexp-list)
  (neo-buffer--set-show-hidden-file-p nil)
  (neotree-next-line)
  (neo-point-auto-indent))

(defun personal-neotree-goto-dir (arg)
  "Move to next dir if ARG >= 0, otherwise move to previous dir."
  (setq arg (if (< arg 0) -1 1))
  (while (and (= (forward-line arg) 0)
              (not (file-directory-p
                    (neo-buffer--get-filename-current-line)))))
  (neo-buffer--post-move)
  (neo-point-auto-indent))

(defun personal-neotree-previous-dir ()
  "Goto the previous directory."
  (interactive)
  (personal-neotree-goto-dir -1))

(defun personal-neotree-next-dir ()
  "Goto the next directory."
  (interactive)
  (personal-neotree-goto-dir 1))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq neo-theme 'ascii)
(setq neo-smart-open t)
(setq neo-window-width 24)
(setq neo-window-fixed-size nil)
(setq neo-show-updir-line nil)
(setq neo-auto-indent-point t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(global-set-key [f4] 'personal-neotree-project-dir)
;; (define-key personal-mode-map (kbd "C-c c p") 'personal-display-path)
;; (define-key personal-mode-map (kbd "C-c c w") 'personal-neotree-default-windows)

;; make convinient key-bindings, we do not modify the official key bindings,
;; instead we make alternative key bindings for personal use.
(let ((map neotree-mode-map))
  (define-key map (kbd "{")       'shrink-window-horizontally)
  (define-key map (kbd "}")       'enlarge-window-horizontally)
  (define-key map (kbd "g")       'neotree-refresh)
  (define-key map (kbd "a")       'neotree-collapse-all)
  (define-key map (kbd "h")       'neotree-hidden-file-toggle)
  (define-key map [(ctrl up)]     'personal-neotree-previous-dir)
  (define-key map [(ctrl down)]   'personal-neotree-next-dir)
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
