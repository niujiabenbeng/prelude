;;; -*- lexical-binding: t; -*-
;;; personal-isearch.el --- Personal isearch settings.

;;; Commentary:

;; Enhancement of isearch functionality.

;;; Code:

(require 's)

(defun personal-isearch-del-word ()
  "Delete word from end of search string and search again."
  (interactive)
  (let ((start (save-excursion (backward-word) (point))))
    (while (and (> (point) start)
                (> (length isearch-string) 0))
      (isearch-del-char))))

(let ((map isearch-mode-map))
  (define-key map (kbd "DEL") 'isearch-del-char)
  (define-key map (kbd "SPC") 'isearch-edit-string)
  (define-key map (kbd "C-<left>") 'isearch-del-char)
  (define-key map (kbd "C-<right>") 'isearch-yank-char)
  (define-key map (kbd "C-S-<left>") 'personal-isearch-del-word)
  (define-key map (kbd "C-S-<right>") 'isearch-yank-word))

;; adopt prucell isearch & lazy-highlight color
(set-face-attribute 'isearch nil        :background "#D7D75F")
(set-face-attribute 'isearch nil        :foreground "#000000")
(set-face-attribute 'lazy-highlight nil :background "#5FAFAF")
(set-face-attribute 'lazy-highlight nil :foreground "#000000")

;; make search respect cases
(setq-default case-fold-search nil)

;; borrowerd from following snippet and modified a bit:
;;   https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs
(defun personal-isearch-with-region ()
  "Isearch on selected text."
  (when (region-active-p)
    (let ((region (funcall region-extract-function nil)))
      (when (s-contains-p "\n" region)
        ;; isearch-cancel signals a quit error which overrides our own
        ;; message. so we capture it and signal our own error.
        (condition-case nil (isearch-cancel)
          (quit (error "Search text should be in one line."))))
      (goto-char (region-beginning))
      (deactivate-mark)
      (isearch-update)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'personal-isearch-with-region)

(provide 'personal-isearch)

;;; personal-isearch.el ends here
