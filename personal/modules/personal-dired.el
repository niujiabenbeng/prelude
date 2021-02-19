;;; personal-dired.el --- dired configuration.

;;; Commentary:

;; personal dired configuration.

;;; Code:

(require 'dired)

;;; use single buffer in dired-mode
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(defun personal-dired-create-dir-or-file (dir-or-file)
  "Mimic `neotree-copy-node'."
  (interactive
   (list (read-file-name "Create dir-or-file: " (dired-current-directory))))
  (if (file-exists-p dir-or-file)
      (message "file already exists: %s" dir-or-file)
    (if ((string-suffix-p "/" dir-or-file))
        (dired-create-directory dir-or-file)
      (let ((dirname (file-name-directory dir-or-file)))
        (unless (file-exists-p dirname)
          (dired-create-directory dirname))
        (write-region "" nil dir-or-file))
      (revert-buffer))))

;; make convinient key-bindings, we do not modify the official key bindings,
;; instead we make alternative key bindings for personal use.
(let ((map dired-mode-map))
  (define-key map (kbd "g")        'revert-buffer)
  (define-key map (kbd "l")        'dired-do-redisplay)
  (define-key map (kbd "^")        'dired-up-directory)
  (define-key map (kbd "v")        'dired-view-file)
  (define-key map (kbd "w")        'dired-copy-filename-as-kill)
  (define-key map (kbd "+")        'personal-dired-create-dir-or-file)
  (define-key map (kbd "TAB")      'dired-display-file)
  (define-key map (kbd "C-<up>")   'dired-prev-dirline)
  (define-key map (kbd "C-<down>") 'dired-next-dirline)
  (define-key map (kbd "C-c d +")  'personal-dired-create-dir-or-file)
  (define-key map (kbd "C-c d C")  'dired-do-copy)
  (define-key map (kbd "C-c d D")  'dired-do-delete)
  (define-key map (kbd "C-c d R")  'dired-do-rename))

(provide 'personal-dired)

;;; personal-dired.el ends here
