;;; personal-pair.el --- Pair manipulation.

;;; Commentary:

;; Additional pair manipulation along with `smartparens`.

;; NOTE: The code of this module is deprecated. It should be reviewed (and
;; rewrite if necessary) before applying to emacs environment.

;;; Code:

(require 's)
(require 'subr-x)
(require 'smartparens)

(defun personal-get-enclosing-sexp ()
  "like `sp-get-enclosing-sexp', but also deal with boundaries."
  (let (enc (pair-list (sp--get-pair-list-context 'navigate)))
    ;; check if point is at the end of an enclosing sexp
    (when (and (not enc) (sp--looking-back (sp--get-closing-regexp pair-list)))
      (save-excursion
        (goto-char (match-beginning 0))
        (setq enc (sp-get-enclosing-sexp)))
      (unless (and enc (= (point) (sp-get enc :end)))
        (setq enc nil)))
    ;; check if point is at the beginning of an enclosing sexp
    (when (and (not enc) (sp--looking-at (sp--get-opening-regexp pair-list)))
      (save-excursion
        (goto-char (match-end 0))
        (setq enc (sp-get-enclosing-sexp)))
      (unless (and enc (= (point) (sp-get enc :beg)))
        (setq enc nil)))
    ;; point is in the middle of an enclosing sexp
    (or enc (sp-get-enclosing-sexp))))

(defmacro personal-forward-whitespace (&rest body)
  "Forward to the first non whitespace character and excute BODY."
  `(progn
     (let ((pos (point))
           (pair-list (sp--get-pair-list-context 'navigate)))
       (unless (sp--looking-back-p (sp--get-closing-regexp pair-list))
         (when (re-search-forward "[^ \t]" nil t)
           (goto-char (match-beginning 0))
           (unless (sp--looking-at-p (sp--get-opening-regexp pair-list))
             (goto-char pos)))))
     (progn ,@body)))

(defun personal-goto-pair ()
  "Goto matched pair."
  (interactive)
  (personal-forward-whitespace
   (if-let* ((enc (personal-get-enclosing-sexp))
             (beg (sp-get enc :beg))
             (end (sp-get enc :end)))
       (if (= (point) beg)
         (goto-char end)
           (goto-char beg))
     (error "No pair is found around point."))))

(defun personal-mark-pair ()
  "Mark matched pair."
  (interactive)
  (personal-forward-whitespace
   (if-let ((enc (personal-get-enclosing-sexp)))
       (progn (goto-char (sp-get enc :beg))
              (push-mark (point) t t)
              (goto-char (sp-get enc :end))
              (setq deactivate-mark nil))
     (error "No pair is found around point."))))

(defun personal-copy-pair ()
  "Copy matched pair."
  (interactive)
  (personal-forward-whitespace
   (if-let ((enc (personal-get-enclosing-sexp)))
       (copy-region-as-kill (sp-get enc :beg) (sp-get enc :end))
     (error "No pair is found around point."))))

(defun personal-kill-pair ()
  "Kill matched pair."
  (interactive)
  (personal-forward-whitespace
   (if-let ((enc (personal-get-enclosing-sexp)))
       (kill-region (sp-get enc :beg) (sp-get enc :end))
     (error "No pair is found around point."))))

(defmacro personal-restrict-to-pairs (function)
  "Restrict FUNCTION to perform on navigation pairs."
  (let (name)
    ;; we define a named function for better documentation.
    (setq name (s-replace-regexp "^sp-" "personal-" (symbol-name function)))
    (setq name (s-replace-regexp "sexp$" "pair" name))
    (setq name (s-replace "-sexp-" "-pair-" name))
    `(defun ,(intern name) ()
       ,(format "Balanced pair restricted version of `%s'" function)
       (interactive)
       (let* ((pairs (sp--get-pair-list-context 'navigate))
              (opens (sp--get-opening-regexp pairs)))
         (sp-restrict-to-object
          'sp-prefix-pair-object
          ;; the last argument: first evaluate the symbol function,
          ;; then quote the result
          (sp-restrict-to-pairs-interactive opens ',function))))))

(provide 'personal-pair)

;;; personal-pair.el ends here
