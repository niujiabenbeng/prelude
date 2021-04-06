;;; -*- lexical-binding: t; -*-
;;; personal-mode.el --- personal configuration: minor mode

;;; Commentary:

;; An alternative minor mode of prelude-mode.

;;; Code

(require 'crux)
(require 'neotree)
(require 'windmove)
(require 'personal-util)
(require 'personal-windows)

(defun personal-display-path ()
  "Display path in echo area."
  (interactive)
  (cond ((eq (current-buffer) (neo-global--get-buffer))
         (message (neo-buffer--get-filename-current-line)))
        (t (message (buffer-file-name)))))

(defun personal-swap-windows ()
  "Swap windows occurding to current window position."
  (interactive)
  (if (windmove-find-other-window 'right)
      (crux-transpose-windows 1)
    (crux-transpose-windows -1)))

(defun personal-kill-other-buffers ()
  "Like `crux-kill-other-buffers', but do not ask y-or-n."
  (interactive)
  (seq-each
   #'kill-buffer
   (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list)))))

(defun personal-dumb-command ()
  "Do nothing but display a message."
  (interactive)
  (message "'%s' is not allowed." (key-description (this-command-keys))))

(defvar personal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a")     'crux-move-beginning-of-line)
    (define-key map (kbd "C-M-z")   'crux-indent-defun)
    (define-key map (kbd "C-c c n") 'crux-cleanup-buffer-or-region)
    (define-key map (kbd "C-c c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c c s") 'personal-swap-windows)
    (define-key map (kbd "C-c c d") 'crux-delete-file-and-buffer)
    (define-key map (kbd "C-c c o") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c c O") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c c k") 'personal-kill-other-buffers)
    (define-key map (kbd "C-c c u") 'personal-find-confiuration-file)
    (define-key map (kbd "C-c c j") 'personal-jump-to-file-at-point)
    (define-key map (kbd "C-c 4 j") 'personal-jump-to-file-at-point-other-window)
    (define-key map (kbd "C-c c i") 'helm-imenu)
    (define-key map (kbd "C-c p")   'projectile-command-map)
    (define-key map (kbd "C-c c p") 'personal-display-path)
    (define-key map (kbd "C-c c w") 'personal-neotree-default-windows)
    ;; disable the following keys
    (define-key map (kbd "C-c c c") 'personal-dumb-command)
    (define-key map (kbd "C-c C-c") 'personal-dumb-command)
    map)
  "Keymap for Personal mode.")

;; define minor mode
(define-minor-mode personal-mode
  "Minor mode to consolidate personal extensions.

\\{personal-mode-map}"
  :lighter " Personal"
  :keymap personal-mode-map
  :global t)

(provide 'personal-mode)

;;; personal-mode.el ends here
