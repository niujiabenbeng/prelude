;;; -*- lexical-binding: t; -*-
;;; personal-pair.el --- Pair manipulation.

;;; Commentary:

;; Additional pair manipulation along with `smartparens`.

;;; Code:

(require 's)
(require 'smartparens)

(defun personal-get-enclosing-sexp ()
  "like `sp-get-enclosing-sexp', but also deal with boundaries."
  (cond
   ;; first choice: use the result of `show-smartparens-mode'
   ((and sp-show-pair-previous-point
         sp-show-pair-previous-match-positions
         (= sp-show-pair-previous-point (point)))
    (-interleave '(:beg :end :op-l :cl-l) sp-show-pair-previous-match-positions))
   ;; second choice: look backward and search for closing delimiter
   ((sp--looking-back-p (sp--get-closing-regexp sp-pair-list)) (sp-get-sexp t))
   ;; third choice: look forward and search for opening delimiter
   ((sp--looking-at-p (sp--get-opening-regexp sp-pair-list)) (sp-get-sexp))
   ;; last choice: return enclosing expression
   (t (sp-get-enclosing-sexp))))

(defun personal-goto-pair ()
  "Goto matched pair."
  (interactive)
  (if-let* ((enc (personal-get-enclosing-sexp))
            (beg (sp-get enc :beg))
            (end (sp-get enc :end)))
      (if (= (point) beg) (goto-char end) (goto-char beg))
    (error "No pair is found around point.")))

(defun personal-mark-pair ()
  "Mark matched pair."
  (interactive)
  (if-let ((enc (personal-get-enclosing-sexp)))
      (progn (goto-char (sp-get enc :beg))
             (push-mark (point) t t)
             (goto-char (sp-get enc :end))
             (setq deactivate-mark nil))
    (error "No pair is found around point.")))

(defun personal-copy-pair ()
  "Copy matched pair."
  (interactive)
  (if-let ((enc (personal-get-enclosing-sexp)))
      (copy-region-as-kill (sp-get enc :beg) (sp-get enc :end))
    (error "No pair is found around point.")))

(defun personal-kill-pair ()
  "Kill matched pair."
  (interactive)
  (if-let ((enc (personal-get-enclosing-sexp)))
      (kill-region (sp-get enc :beg) (sp-get enc :end))
    (error "No pair is found around point.")))

(defmacro personal-restrict-to-pairs (function)
  "Restrict FUNCTION to perform on navigation pairs."
  (let (name)
    ;; we define a named function for better documentation.
    (setq name (s-replace-regexp "^sp-" "personal-" (symbol-name function)))
    (setq name (s-replace-regexp "sexp$" "pair" name))
    (setq name (s-replace "-sexp-" "-pair-" name))
    `(defun ,(intern name) (&optional _arg)
       ,(format "Balanced pair restricted version of `%s'." function)
       (interactive "P")
       ;; the last argument:
       ;;   first evaluate the symbol function, then quote the result.
       ;; here `sp-restrict-to-pairs' does not work, I don't know why.
       (sp-restrict-to-object 'sp-prefix-pair-object ',function))))

(provide 'personal-pair)

;;; personal-pair.el ends here
