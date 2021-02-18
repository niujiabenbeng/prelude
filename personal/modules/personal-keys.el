;;; personal-keys.el --- Personal key bindings.

;;; Commentary:
;;; Modify some key bindings of perlude emacs for personal needs.

;;; Code:

(require 'personal-pair)
(require 'personal-util)
(require 'personal-movetext)

;;; disable some key maps
(setf (cdr prelude-mode-map) nil)
(setf (cdr smartparens-mode-map) nil)

;;; disable default windmove key bindings. S-{left, right, up, down} are
;;; reserved for shift selection.
(global-set-key [S-up]    nil)
(global-set-key [S-down]  nil)
(global-set-key [S-left]  nil)
(global-set-key [S-right] nil)

;;; bind C-M-{left right up down} to windmove.
(global-set-key [C-M-up]    'windmove-up)
(global-set-key [C-M-down]  'windmove-down)
(global-set-key [C-M-left]  'windmove-left)
(global-set-key [C-M-right] 'windmove-right)

;;; key bindings for text moving
(global-set-key [M-up]    'personal-move-text-up)
(global-set-key [M-down]  'personal-move-text-down)
(global-set-key [M-left]  'personal-move-text-left)
(global-set-key [M-right] 'personal-move-text-right)

;;; key bindings for text editing.
(global-set-key [M-delete] 'kill-word)
(global-set-key [C-delete] 'kill-word)
(global-set-key [M-backspace] 'backward-kill-word)
(global-set-key [C-backspace] 'backward-kill-word)

;;; alter default emacs keybindings
(global-set-key (kbd "C-x r w") 'copy-rectangle-as-kill)

;;; key bindings for some functionalities
(global-set-key (kbd "C-;")     'personal-comment-line)
(global-set-key (kbd "M-;")     'personal-comment-line)
(global-set-key (kbd "<f5>")    'personal-run-current-script)
(global-set-key (kbd "C-<f5>")  'kill-compilation)
(global-set-key (kbd "C-<tab>") 'personal-pattern-replace)

;;; ctrl-return is also used in neotree-mode
(global-set-key [(ctrl return)] 'crux-smart-open-line)

;;; "C-=" is for expand-region, "C--" is for shrink-region.
;;; I don't know why "-" vs "=" is a pair on standard keyboard layout.
(global-set-key (kbd "C--") (lambda () (interactive) (er/expand-region -1)))

;;; prelude binds "C-+" to text-scale-increase, "C--" to text-scale-decrease.
;;; but "C--" is used for shrink-region. So we discard the binding of "C-+".
(global-set-key (kbd "C-+") nil)   ;; previous run: text-scale-increase

;;; disable some global key bindings
(global-set-key (kbd "C-c j") nil) ;; previous run: avy-goto-word-or-subword-1
(global-set-key (kbd "C-\\") nil)  ;; previous run: toggle-input-method
(global-set-key (kbd "C-t") nil)   ;; previous run: transpose-chars
(global-set-key (kbd "M-t") nil)   ;; previous run: transpose-words
(global-set-key (kbd "C-x f") nil) ;; previous run: set-fill-column
(global-set-key (kbd "M-o") nil)   ;; previous run: facemenu-keymap

;;; previous-bufer/next-buffer is not that useful, since there are lots of
;;; non-editing buffers. to switch to other buffer, use 'C-x b'
(global-set-key (kbd "C-x <left>") nil)    ;; previous run: previous-buffer
(global-set-key (kbd "C-x <right>") nil)   ;; previous run: next-buffer
(global-set-key (kbd "C-x C-<left>") nil)  ;; previous run: previous-buffer
(global-set-key (kbd "C-x C-<right>") nil) ;; previous run: next-buffer

;;; key-bindings for winner. Prefer "C-x C-<left>" to "C-x <left>", since
;;; the former does not need to release ctrl key.
(setf (cdr winner-mode-map) nil)
(let ((map winner-mode-map))
  (define-key map (kbd "C-x C-<left>") 'winner-undo)
  (define-key map (kbd "C-x C-<right>") 'winner-redo))

;;; key-chords settings
(key-chord-define-global "xx" nil)
(key-chord-define-global "vv" 'helm-all-mark-rings)

(defun personal-transpose-windows ()
  "Transpose windows occurding to current window position."
  (interactive)
  (if (windmove-find-other-window 'right)
      (crux-transpose-windows 1)
    (crux-transpose-windows -1)))

;;; copy from crux-kill-other-buffers, but do not ask y-or-n
(defun personal-kill-other-buffers ()
  "Kill all buffers but the current one."
  (interactive)
  (seq-each
   #'kill-buffer
   (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list)))))

(defun personal-dumb-command ()
  "Do nothing but display a message."
  (interactive)
  (message "'%s' is not allowed." (key-description (this-command-keys))))

;;; previous prelude keybindings
(let ((map prelude-mode-map))
  (define-key map (kbd "C-a")       'crux-move-beginning-of-line)
  (define-key map (kbd "C-M-z")     'crux-indent-defun)
  (define-key map (kbd "C-c c n")   'crux-cleanup-buffer-or-region)
  (define-key map (kbd "C-c c e")   'crux-eval-and-replace)
  (define-key map (kbd "C-c c s")   'personal-transpose-windows)
  (define-key map (kbd "C-c c d")   'crux-delete-file-and-buffer)
  (define-key map (kbd "C-c c o")   'crux-duplicate-current-line-or-region)
  (define-key map (kbd "C-c c r")   'crux-rename-buffer-and-file)
  (define-key map (kbd "C-c c k")   'personal-kill-other-buffers)
  (define-key map (kbd "C-c c u")   'personal-find-confiuration-file)
  (define-key map (kbd "C-c c j")   'personal-jump-to-file-at-point)
  (define-key map (kbd "C-c 4 j")   'personal-jump-to-file-at-point-other-window)
  (define-key map (kbd "C-c c i")   'helm-imenu)
  (define-key map (kbd "C-c p")     'projectile-command-map))

;;; personal keybindings, also use prelude mode map
(let ((map prelude-mode-map))
  (define-key map (kbd "C-c c c")   'personal-dumb-command)
  (define-key map (kbd "C-c C-c")   'personal-dumb-command))

;;; personal keybindings for pair manipulation
(let ((map smartparens-mode-map))
  (define-key map (kbd "C-M-f") (personal-restrict-to-pairs sp-forward-sexp))
  (define-key map (kbd "C-M-b") (personal-restrict-to-pairs sp-backward-sexp))
  (define-key map (kbd "C-M-p") 'personal-goto-pair)
  (define-key map (kbd "C-M-@") 'personal-mark-pair)
  (define-key map (kbd "C-M-w") 'personal-copy-pair)
  (define-key map (kbd "C-M-k") 'personal-kill-pair)
  (define-key map (kbd "C-M-u") 'sp-splice-sexp)
  (define-key map (kbd "C-M--") 'sp-forward-slurp-sexp)
  (define-key map (kbd "C-M-=") 'sp-forward-barf-sexp))

;;; do not show some keys in which-key popup
(push '(("C-c C-c" . nil) . 0) which-key-replacement-alist)
(push '(("C-c c c" . nil) . 0) which-key-replacement-alist)

;;; add key replacement in which-key-mode
(which-key-add-key-based-replacements "C-c c" "prelude-mode-prefix")
(which-key-add-key-based-replacements "C-c d" "dired-mode-prefix")
(which-key-add-key-based-replacements "C-c @" "hs-mode-prefix")

;;; reset prelude-tips
(setq
 prelude-tips
 '(;; global-mode-map key bindings
   "Press <M-z> to zap up to char."
   "Press <C-RET> to open a line beneath the current one."
   "Press <C-=>/<C--> to expand/shrink the selected region."
   "Press <C-^>/<M-^> to join lines below/above."
   "Press <C-S-Backspace> to kill the whole line."
   "Press <M-%%> to replace string in current buffer."
   "Press <C-x n n>/<C-x n w> to narrow-to/widen-from region."
   "Press <C-x \\> to align region according to regexp."
   ;; prelude-mode-map key bindings
   "Press <C-c c n> to cleanup buffer or region."
   "Press <C-c c e> to evaluate and replace sexp with its value."
   "Press <C-c c s> to swap two windows."
   "Press <C-c c d> to delete the current file and buffer."
   "Press <C-c c o> to duplicate current line or region."
   "Press <C-c c r> to rename the current buffer and file."
   "Press <C-c c k> to kill all the buffers, but the active one."
   "Press <C-c c u> to show all confiuration files."
   "Press <C-c c i> to list all the symbol in current buffer."
   "Press <C-c c j> to jump to the start of a word in any visible window."
   "Press <C-c c m> to show all minor modes current enabled."
   "Press <C-c c p> to display path under cursor."
   "Press <C-c c w> to get default window configuration."
   ;; projectile-mode-map key bindings
   "Press <C-c p f> to navigate a project's files with helm."
   "Press <C-c p s g> to run grep on a project."
   "Press <C-c p p> to switch between projects."
   ;; tips of helm-mode
   "In Helm buffer, press <C-SPC> to mark current candidates."
   "In Helm buffer, press <C-c C-i> to insert marked candidates into current buffer."
   "In Helm buffer, press <C-t> to switch back the location of helm buffer."
   "In Helm buffer, press <Tab> to show the content/documentation of current item."
   "Press <C-o> or <C-c h o> to display the occurrances of a string."
   "Press <C-c h SPC> or <vv> to display the mark ring."
   "Press <C-c p b> to bring previous Helm session back."
   "Press <C-c p h> to navigate a project in Helm."
   ;; commands & key bindings of any other keymaps
   "Press <M-(>/<M-\"> to insert parenthesis or double quotation marks."
   "Command <untabify> to convert all tabs to spaces in region or buffer."
   "Command <indent-region> to indent region or buffer."
   "Use <re-builder> to interactively search for regexp."))

(provide 'personal-keys)

;;; personal-keys.el ends here
