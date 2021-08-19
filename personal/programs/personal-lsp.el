;;; -*- lexical-binding: t; -*-
;;; personal-lsp.el --- Personal configuration of lsp.

;;; Commentary:

;; Personal configuration of lsp for various languages.
;; Emacs client for the Language Server Protocol:
;;   https://github.com/emacs-lsp/lsp-mode#supported-languages

;;; Code:

(setq lsp-keymap-prefix "C-c l"
      lsp-keep-workspace-alive nil

      lsp-auto-guess-root t
      lsp-idle-delay 0.1
      lsp-response-timeout 5

      ;; prevent underscore below symbols
      lsp-enable-symbol-highlighting nil
      ;; prevent underscore below c++ include
      lsp-enable-links nil
      ;; prevent a line above the working window
      lsp-headerline-breadcrumb-enable nil)

(setq lsp-ui-doc-max-height 8
      lsp-ui-doc-max-width 35
      lsp-ui-sideline-ignore-duplicate t
      lsp-ui-peek-enable t
      lsp-ui-imenu-enable t
      lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-doc-show-with-mouse nil
      lsp-ui-doc-position 'at-point
      lsp-ui-sideline-show-hover nil)

(prelude-require-packages '(lsp-mode lsp-ui))
(require 'lsp-mode)
(require 'lsp-ui)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(advice-add #'lsp-completion--regex-fuz :override #'identity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jumping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-lsp-find-thing-at-point ()
  "Find file or function definition at point."
  (interactive)
  (let ((files (personal--fcf-get-files-at-point))
        (symbol (symbol-at-point)) finish)
    (when files
      (xref-push-marker-stack)
      (find-file (car files))
      (setq finish t))
    (when (and (not finish) (not symbol))
      ;; use `error' to prevent jumping to other window in
      ;; `personal-lsp-find-thing-at-point-other-window'
      (error "Not found symbol at point.")
      (setq finish t))
    (when (and (not finish) (eq major-mode 'emacs-lisp-mode))
      (elisp-slime-nav-find-elisp-thing-at-point (symbol-name symbol))
      (setq finish t))
    (when (not finish)
      (lsp-ui-peek-find-definitions))))

(defun personal-lsp-find-thing-at-point-other-window ()
  "Find file or function definition at point and show other window."
  (interactive)
  (personal-display-result-other-window (personal-lsp-find-thing-at-point)))

(defun personal-lsp-pop-marker-stack-other-window ()
  "Show previous marker position in other window."
  (interactive)
  (personal-display-result-other-window (xref-pop-marker-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal-lsp-noformat-marker
  "NOFORMAT(\\([-0-9:]+\\))"
  "Regexp of marker to disable auto formatting.")

(defun personal-lsp-parse-range-string (content)
  "Parse range string, such as `:2', `-2:', `-1:1'"
  (let ((beg 0) (end 0) (ok t))
    (cond ((string-match "^:\\(-?[0-9]+\\)$" content)
           (setq end (string-to-number (match-string 1 content))))
          ((string-match "^\\(-?[0-9]+\\):$" content)
           (setq beg (string-to-number (match-string 1 content))))
          ((string-match "^\\(-?[0-9]+\\):\\(-?[0-9]+\\)$" content)
           (setq beg (string-to-number (match-string 1 content)))
           (setq end (string-to-number (match-string 2 content))))
          ((string-match "^-?[0-9]+$" content)
           (setq beg (string-to-number content) end beg))
          (t (setq ok nil)))
    (when (and ok (<= beg end))
      (setq beg (save-excursion (forward-line beg) (line-beginning-position)))
      (setq end (save-excursion (forward-line end) (line-end-position)))
      (cons beg end))))

(defun personal-lsp-collect-ranges (marker)
  "Collect ranges specified by MARKER in current buffer."
  (let (text range ranges)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward marker nil t)
        (setq text (match-string-no-properties 1))
        (setq range (personal-lsp-parse-range-string text))
        (when range (setq ranges (cons range ranges)))))
    ranges))

(defun personal-lsp-flip-ranges (ranges &optional beg end)
  "Flip ranges limited by BEG and END."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let ((loc beg) range flipped)
    (dolist (item (cl-sort ranges #'< :key #'car))
      ;; 这里range为前闭后闭区间
      (when (< (1+ loc) (car item))
        (setq range (cons loc (1- (car item))))
        (setq flipped (cons range flipped)))
      (setq loc (1+ (cdr item))))
    (when (< loc (point-max))
      (setq range (cons loc (point-max)))
        (setq flipped (cons range flipped)))
    (reverse flipped)))

(defun personal-lsp-format-buffer ()
  "Format buffer while paying respect to NOFORMAT marker."
  (interactive "*")
  (let* ((marker personal-lsp-noformat-marker)
         (ranges (personal-lsp-collect-ranges marker)))
  (dolist (item (personal-lsp-flip-ranges ranges))
    (lsp-format-region (car item) (cdr item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; linting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal-lsp-nolint-line-marker
  "NOLINTLINE(\\([-0-9:]+\\))"
  "Regexp of marker to disable linting among specific lines.")

(defvar personal-lsp-nolint-field-marker
  "NOLINTFIELD(\\([-_[:alnum:]]+\\))"
  "Regexp of marker to discard specific errors among entire source file.")

(defvar personal-lsp-nolint-ranges nil
  "Nolint ranges specified by `personal-lsp-nolint-line-marker'.")

(defvar personal-lsp-nolint-errors nil
  "discarded erros specified by `personal-lsp-nolint-field-marker'")

(defun personal-lsp-collect-nolint-ranges ()
  "Collect nolint ranges in current buffer."
  (setq personal-lsp-nolint-ranges
        (personal-lsp-collect-ranges personal-lsp-nolint-line-marker)))

(defun personal-lsp-collect-nolint-fields ()
  "Collect nolint fields in current buffer."
  (setq personal-lsp-nolint-errors nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward personal-lsp-nolint-field-marker nil t)
      (setq personal-lsp-nolint-errors
            (append personal-lsp-nolint-errors
                    (split-string (match-string-no-properties 1) "," t))))))

(defun personal-lsp-ignore-lint-errors (err)
  (let* ((id (flycheck-error-id err))
         (pos (flycheck-error-pos err))
         (ranges personal-lsp-nolint-ranges)
         (discard (or (member id personal-lsp-nolint-errors)
                      (member "all" personal-lsp-nolint-errors))))
    (while (and ranges (not discard))
      (setq discard (or (< pos (caar ranges)) (> pos (cadr ranges))))
      (setq ranges (cdr ranges)))
    discard))

(add-hook 'flycheck-before-syntax-check-hook #'personal-lsp-collect-nolint-ranges)
(add-hook 'flycheck-before-syntax-check-hook #'personal-lsp-collect-nolint-fields)
(add-hook 'flycheck-process-error-functions  #'personal-lsp-ignore-lint-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keybinding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((map personal-mode-map))
  (define-key map (kbd "M-.")   #'personal-lsp-find-thing-at-point)
  (define-key map (kbd "C-M-.") #'personal-lsp-find-thing-at-point-other-window)
  (define-key map (kbd "C-M-,") #'personal-lsp-pop-marker-stack-other-window)
  (define-key map [remap lsp-format-buffer] #'personal-lsp-format-buffer))

(provide 'personal-lsp)

;;; personal-lsp.el ends here
