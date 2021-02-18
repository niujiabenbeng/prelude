;;; personal-movetext.el --- Move text functionality for Prelude Emacs.
;;; Added by Chen Li@2020.04.02

;;; Commentary:
;;; Move text left, right, up, and down.

;;; Code:

(defvar personal-horizonal-location 0 "horizonal cursor location.")

(defun personal-move-char-horizontally (n)
  "Move character under current cursor left or right by N characters."
  (let ((char (char-after)))
    (delete-char 1)
    (goto-char (+ (point) n))
    (insert char)
    (backward-char)))

(defun personal-move-line-vertically (n)
  "Move line at current cursor up or down by N lines."
  (when (or (and (> n 0) (save-excursion (search-forward "\n" nil t)))
            (and (< n 0) (save-excursion (search-backward "\n" nil t))))
    (let* ((offset (current-column))
           (beg (line-beginning-position))
           (end (line-end-position))
           (text (delete-and-extract-region beg end)))
      (when (< (point) (point-max))
        (delete-char 1))
      (forward-line n)
      (if (and (= (point) (point-max))
               (> (point) (line-beginning-position)))
          (progn (insert "\n" text)
                 (move-to-column offset))
        (insert text "\n")
        (backward-char)
        (move-to-column offset)))))

(defun personal-move-region-horizontally (n)
  "Move region left or right by N characters."
  (let* ((text (delete-and-extract-region (point) (mark)))
         (to (max (point-min) (min (point-max) (+ (point) n))))
         (offset (- to (point))))
    (forward-char offset)
    (setq personal-horizonal-location (current-column))
    (set-mark (point))
    (insert text)
    ;; make auto-scroll take effect
    (if (< n 0) (exchange-point-and-mark))
    (setq deactivate-mark nil)))

(defun personal-move-region-vertically (n)
  "Move region up or down by N lines."
  (let* ((text (delete-and-extract-region (point) (mark)))
         line-length)
    (unless (memq last-command
                  '(personal-move-text-up
                    personal-move-text-down))
      (setq personal-horizonal-location (current-column)))
    (forward-line n) ;; cursor is at the start of line
    (setq line-length (- (line-end-position) (point)))
    (if (> line-length personal-horizonal-location)
        (forward-char personal-horizonal-location)
      (forward-char line-length))
    (set-mark (point))
    (insert text)
    ;; make auto-scroll take effect
    (if (< n 0) (exchange-point-and-mark))
    (setq deactivate-mark nil)))

(defun personal-move-text-left ()
  (interactive)
  (if (region-active-p)
      (personal-move-region-horizontally -1)
    (personal-move-char-horizontally -1)))

(defun personal-move-text-right ()
  (interactive)
  (if (region-active-p)
      (personal-move-region-horizontally 1)
    (personal-move-char-horizontally 1)))

(defun personal-move-text-up ()
  (interactive)
  (if (region-active-p)
      (personal-move-region-vertically -1)
    (personal-move-line-vertically -1)))

(defun personal-move-text-down ()
  (interactive)
  (if (region-active-p)
      (personal-move-region-vertically 1)
    (personal-move-line-vertically 1)))

(provide 'personal-movetext)

;;; personal-movetext.el ends here
