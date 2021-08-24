;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Sort lines and delete duplicates.

;;;Code:

(defun util-normalize-lines ()
  "Sort lines in region and delete duplicates."
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (let* ((beg (personal-region-beginning))
             (end (personal-region-end))
             (text (buffer-substring-no-properties beg end)) result)
        ;; 这里buffer的内容改变之后, point的位置就会变化, 所以用temp-buffer
        (with-temp-buffer
          (insert text)
          (delete-trailing-whitespace (point-min) (point-max))
          (flush-lines "^$" (point-min) (point-max))
          (sort-lines nil (point-min) (point-max))
          (delete-duplicate-lines (point-min) (point-max))
          (setq result (buffer-string)))
        (if (string= result text)
            (message "The region is properly normalized.")
          (goto-char beg)
          (delete-region beg end)
          (insert result)))
    (message "No region provided.")))

(provide 'util-normalize-lines)

;;; util-normalize-lines.el ends here
