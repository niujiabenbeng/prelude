;;; personal-windows.el --- Utility for window configuration.

;;; Commentary:

;; This module defines some useful functions of window configuration. Current
;; implementation is based on neotree.

;;; Code:

(require 'neotree)

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

(provide 'personal-windows)

;;; personal-windows.el ends here
