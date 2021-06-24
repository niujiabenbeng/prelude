;;; -*- lexical-binding: t; -*-
;;; personal-util.el --- Utilities for personal needs.

;;; Commentary:

;; Utilities for personal needs.

;;; Code:

(require 's)
(require 'rx)
(require 'helm)
(require 'subr-x)
(require 'compile)

(defun personal-search-all (regexp string)
  "Collect all pieces of text in STRING which match REGEXP."
  (let (parts)
    (while (string-match regexp string)
      (setq parts (cons (match-string 0 string) parts))
      (setq string (substring string (match-end 0))))
    (reverse parts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; personal-pattern-replace ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-cutting-line-p ()
  "Return true if current line is cutting line."
  (string-match-p
   "^\\([;#/]\\).+\\1$"
   (string-trim (or (thing-at-point 'line t) ""))))

(defun personal-format-cutting-line ()
  "Format the cutting line (assume the current line is cutting line)."
  (let* ((line (string-trim (thing-at-point 'line t)))
         (marker (substring line 0 1))
         (pattern "[;#/ \t\n\r]+")
         (target-length 80)
         line-length head tail)
    ;; 去掉line两端的注释符号和空白字符
    (setq line (string-trim line pattern pattern))
    ;; 在line两端各加上一个注释符号和一个空白字符
    (if (> (length line) 0)
        (setq line (concat " " line " ")))
    (setq line (concat marker line marker))

    (setq line-length (string-width line))
    (if (> line-length target-length)
        (message "Error: Current line is too long.")
      (setq head (/ (- target-length line-length) 2))
      (setq tail (- target-length line-length head))
      (setq head (apply 'concat (make-list head marker)))
      (setq tail (apply 'concat (make-list tail marker)))
      (setq line (concat head line tail "\n"))
      (if (string-equal (thing-at-point 'line t) line)
          (message "Current line has already been formatted.")
        (delete-region
         (progn (forward-visible-line 0) (point))
         (progn (forward-visible-line 1) (point)))
        (insert line)))))

(defun personal-shebang-p ()
  "Return true if current point is at the beginning of buffer."
  (= (point) (point-min)))

(defun personal-add-shebang-python ()
  "Add shebang for python file."
  (let ((line (or (thing-at-point 'line t) "")))
    (if (string-prefix-p "#!" line)
        (message "header is already configured.")
      (insert "#! /usr/bin/env python\n" )
      (insert "# coding: utf-8\n"))))

(defun personal-add-shebang-shell ()
  "Add shebang for shell file."
  (let ((line (or (thing-at-point 'line t) "")))
    (if (string-prefix-p "#!" line)
        (message "header is already configured.")
      (insert "#! /bin/bash\n"))))

(defun personal-add-shebang-c-header ()
  "Add shebang for c header file."
  (let* ((path (buffer-file-name))
         (head (file-name-nondirectory path))
         (posi (string-match "[^/]+/\\(include\\|src\\)/" path))
         (line (or (thing-at-point 'line t) "")))
    (when posi
      (setq head (substring path posi))
      (setq head (replace-regexp-in-string "/\\(include\\|src\\)/" "_" head)))
    (setq head (replace-regexp-in-string "[^[:alnum:]]" "_" head))
    (setq head (upcase (concat head "_")))

    (if (string= (format "#ifndef %s\n" head) line)
        (message "header is already configured.")
      (if (string-prefix-p "#ifndef" line)
          ;; 如果之前已经存在但名称不同, 则替换名称
          (let ((text (string-trim (substring line (length "#ifndef")))))
            (while (re-search-forward text nil t) (replace-match head)))
        ;; 如果之前不存在, 则插入新的
        (insert (format "#ifndef %s\n" head))
        (insert (format "#define %s\n\n" head))
        (goto-char (point-max))
        (insert (format "\n#endif  // %s\n" head)))
      ;; 跳转到第三行
      (goto-line 3))))

(defun personal-add-shebang-elisp ()
  "Add shebang for emacs lisp file."
  (let ((name (file-name-nondirectory (buffer-file-name)))
        (line (or (thing-at-point 'line t) ""))
        (head ";;; -*- lexical-binding: t; -*-\n"))
    (if (string= line head)
        (message "header is already configured.")
      (insert (format "%s;;; %s --- \n\n" head name))
      (insert ";;; Commentary:\n\n;;\n\n;;;Code:\n\n")
      (goto-char (point-max))
      (insert (format "\n(provide '%s)\n" (file-name-base name)))
      (insert (format "\n;;; %s ends here\n" name))
      (goto-line 2)
      (end-of-line))))

(defun personal-add-shebang ()
  "Add shebang according to file extension."
  (let ((ext (file-name-extension (or (buffer-file-name) ""))))
    (cond ((null ext) (message "file has no extension."))
          ((string= ext "py")  (personal-add-shebang-python))
          ((string= ext "h")   (personal-add-shebang-c-header))
          ((string= ext "hpp") (personal-add-shebang-c-header))
          ((string= ext "sh")  (personal-add-shebang-shell))
          ((string= ext "el")  (personal-add-shebang-elisp))
          (t (message ".%s is not supported yet." ext)))))

(defun personal-pattern-replace ()
  "Replace predefined pattern at current point."
  (interactive)
  (cond ((personal-cutting-line-p) (personal-format-cutting-line))
        ((personal-shebang-p) (personal-add-shebang))
        (t (message "No pattern found"))))

;;;;;;;;;;;;;;;;;;;;;;;;; personal-run-current-script ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-extract-shell-command ()
  "Extract all shell snippets on markdown format."
  (declare (obsolete nil "2021.02.22"))
  (let ((pattern "```[[:space:]]?\\([sS]hell\\)?\\([^`]+\\)```$")
        command commands prefix)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (setq command (match-string-no-properties 2))
        (save-excursion
          (goto-char (match-beginning 0))
          (setq prefix (string-trim-right
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))))
        (unless (string-empty-p prefix)
          (setq command (s-replace (concat "\n" prefix) "\n" command)))
        (setq commands (cons (string-trim command) commands))))
    (reverse commands)))

(defun personal-run-current-script ()
  "Run current script using Shebang."
  (interactive)
  (if (null (buffer-file-name))
      (message "Current buffer is not attached to any file.")
    (when (buffer-modified-p) (save-buffer))
    (if (string= (buffer-substring-no-properties 1 3) "#!")
        (if compilation-in-progress
            (message "The compilation process is running.")
          (compile (buffer-file-name) t))
      (message "Current file does not have a shebang line."))))

(defun personal-kill-compilation ()
  "Kill current compilation."
  (interactive)
  (if compilation-in-progress
      (with-current-buffer "*compilation*" (kill-compilation))
    (message "No compilation process found.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; personal-comment-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-comment-line ()
  "A more useful `comment-line'."
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (comment-or-uncomment-region
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (if (= (point) (line-beginning-position))
             (line-end-position 0)
           (line-end-position))))
    (comment-line 1)))

;;;;;;;;;;;;;;;;;;;;;;; personal-find-confiuration-file ;;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal--fcf-context
  '((names   . nil)   ;; names of sources
    (actives . nil))  ;; names of current active sources
  "The context of function `personal-find-configuration-file'.
It is initialized in the beginning of that function.")

(defun personal--fcf-context-get (key)
  (alist-get key personal--fcf-context))

(defun personal--fcf-context-put (key value)
  (setf (alist-get key personal--fcf-context) value))

(defun personal--fcf-wrap-nonexist (show)
  (let ((user-input-indicator "[?] "))
    (setq show (propertize show 'font-lock-face '(:foreground "#888888")))
    (concat user-input-indicator show)))

(defun personal--fcf-get-candidate (path &optional name root show-full-path)
  (if (null path) (setq path (expand-file-name name root)))
  (if (null name) (setq name (file-relative-name path root)))
  (let ((show (if show-full-path path name)))
    (when (not (file-exists-p path))
      (setq show (personal--fcf-wrap-nonexist show)))
    (cons show (expand-file-name path))))

(defun personal--fcf-get-prefix (paths)
  "Return common directory of all paths."
  (setq paths (mapcar #'expand-file-name paths))
  (file-name-directory (seq-reduce #'s-shared-start paths (car paths))))

(defun personal--fcf-match-name (candidate root)
  "Return t if name of CANDIDATE relative to ROOT matches `helm-pattern'."
  (let ((name (file-relative-name (cdr candidate) root))
        (patterns (split-string helm-pattern)) (match t))
    (while (and match patterns)
      (setq match (string-match-p (car patterns) name))
      (setq patterns (cdr patterns)))
    match))

(defun personal--fcf-match-name-filter (candidates root)
  "Return all CANDIDATES whose names relative to ROOT match `helm-pattern'."
  (seq-filter (lambda (x) (personal--fcf-match-name x root)) candidates))

(defun personal--fcf-match-name-only-filter (candidates)
  "Return all CANDIDATES whose names match `helm-pattern'."
  (seq-filter
   (lambda (x)
     (personal--fcf-match-name x (file-name-directory (cdr x)))) candidates))

(defun personal--fcf-get-compare-key (candidate &optional fields)
  "Get key of CANDIDATE for comparing.
FIELDS should be a symbol or a list of symbols, and can be values below:
   - ext:     sort by extension
   - name:    sort by name
   - length:  sort by length of name
   - existp:  sort by existence
   - recent:  sort by recently visited time."
  (and fields (symbolp fields) (setq fields (list fields)))
  (let ((name (cdr candidate)) (key ""))
    (dolist (field fields)
      (cond ((eq field 'ext)
             (setq key (concat key (file-name-extension name))))
            ((eq field 'name)
             (setq key (concat key name)))
            ((eq field 'length)
             (setq key (concat key (format "%03d" (length name)))))
            ((eq field 'existp)
             (setq key (concat key (if (file-exists-p name) "0" "1"))))
            ((eq field 'recent)
             (let ((pos (-elem-index name recentf-list)))
               (setq key (concat key (format "%04d" (or pos 9999))))))
            (t (error "unknown field: %s" field))))
    key))

(defun personal--fcf-sort (candidates &optional fields)
  ;; 这里我们先计算每一个candidate的key, 并将它放到car的位置. 然后再排序, 最后将
  ;; key丢掉, 获得排序后的结果. 这样做的目的是, 避免重复的key的计算, 并且, 如果
  ;; 直接在cl-sort中动态生成key的话, 程序运行的结果不对, 目前不知道是为什么.
  (let ((fun (lambda (x) (cons (personal--fcf-get-compare-key x fields) x))))
    (mapcar #'cdr (cl-sort (mapcar fun candidates) #'string-lessp :key #'car))))

(defmacro personal--fcf-create-candidate-transformer (&rest body)
  "The outer part of a candidate transformer.
If current source is active, evaluate BODY and return the value,
otherwise return nil."
  `(let ((name (alist-get 'name sources))
         (actives (personal--fcf-context-get 'actives)))
     (if (member name actives) (progn ,@body) nil)))

(defun personal--fcf-move-source (step)
  "Change current active source in the direction given by STEP."
  (let* ((names (personal--fcf-context-get 'names))
         (actives (personal--fcf-context-get 'actives))
         (size (- (length actives) step)))
    (setq size (max 1 (min (length names) size)))
    (personal--fcf-context-put 'actives (last names size)))
  (helm-update))

(defvar personal--fcf-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map [left]
      (lambda () (interactive) (personal--fcf-move-source -1)))
    (define-key map [right]
      (lambda () (interactive) (personal--fcf-move-source 1)))
    (define-key map [(ctrl return)] #'helm-ff-run-switch-other-window)
    map)
  "Keymap for `personal-find-confiuration-file'.")

;;;;;;;;;;;;;;;;;;;;;;;;; soruce: temporary test file ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal--fcf-testfile-dir
  '(("cpp" . "~/.testplace/temp-cpp/tools")
    ("py"  . "~/.testplace/temp-python/tools")
    ("sh"  . "~/.testplace/temp-shell")
    ("el"  . "~/.testplace/temp-elisp")
    ("txt" . "~/.testplace/temp-text"))
  "Directories of test files.")

(defvar personal--fcf-testfile-root
  (personal--fcf-get-prefix (mapcar #'cdr personal--fcf-testfile-dir))
  "Root directory of all test files.")

(defvar personal--fcf-testfile-exclude
  '("main.cpp" "init.py" "main.py" "main_unittest.py")
  "Names of file which should be excluded.")

(defvar personal--fcf-testfile-show-type 'spmn
  "Type of message to show. The value can be the following:
   -- snmn:  # show name match name
   -- spmp:  # show path match path
   -- spmn:  # show path match name ")

(defun personal--fcf-testfile-candidates ()
  "Collect test files for helm candidates."
  (let ((sp (not (eq personal--fcf-testfile-show-type 'snmn)))
        (root personal--fcf-testfile-root) ext dir pattern candidates)
    (dolist (pair personal--fcf-testfile-dir)
      (setq ext (car pair) dir (cdr pair))
      (setq pattern (format "\\.%s\\'" ext))
      (when (file-exists-p dir)
        (dolist (name (directory-files dir nil pattern t))
          (when (not (member name personal--fcf-testfile-exclude))
            (push (personal--fcf-get-candidate
                   (expand-file-name name dir) nil root sp) candidates))))
      ;; add a default file to avoid no file found
      (push (personal--fcf-get-candidate
             (expand-file-name (format "test.%s" ext) dir) nil root sp)
            candidates))
    candidates))

(defun personal--fcf-testfile-candidate-transformer (candidates sources)
  (personal--fcf-create-candidate-transformer
   (when (eq personal--fcf-testfile-show-type 'spmn)
     (setq candidates (personal--fcf-match-name-filter
                       candidates personal--fcf-testfile-root)))
   (let ((sp (not (eq personal--fcf-testfile-show-type 'snmn)))
         (root personal--fcf-testfile-root) extends name path)
     ;; 将`helm-pattern'中的内容分离为文件名和扩展名
     (dolist (elem (split-string helm-pattern))
       (let ((pair (assoc elem personal--fcf-testfile-dir)))
         (if pair (push pair extends) (push elem name))))
     (when name
       (setq name (s-join "_" (reverse name)))
       (dolist (elem (or extends personal--fcf-testfile-dir))
         (setq path (format "%s.%s" name (car elem)))
         (setq path (expand-file-name path (cdr elem)))
         (push (personal--fcf-get-candidate path nil root sp) candidates))))
   (personal--fcf-sort
    (cl-remove-duplicates candidates :test #'string-equal :key #'cdr)
    '(existp recent ext name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; soruce: project file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal--fcf-project-show-type 'spmn
  "Type of message to show. The value can be the following:
   -- snmn:  # show name match name
   -- spmp:  # show path match path
   -- spmn:  # show path match name ")

(defun personal--fcf-project-candidates ()
  "Collect project configuration files for helm candidates."
  ;; when-let return nil if root is nil
  (when-let ((root (projectile-project-root)))
    (let ((sp (not (eq personal--fcf-project-show-type 'snmn))))
      (cl-loop for name in (projectile-project-files root)
            collect (personal--fcf-get-candidate nil name root sp)))))

(defun personal--fcf-project-candidate-transformer (candidates sources)
  (personal--fcf-create-candidate-transformer
   (when (eq personal--fcf-project-show-type 'spmn)
     (setq candidates (personal--fcf-match-name-filter
                       candidates (projectile-project-root))))
   (personal--fcf-sort candidates '(recent name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; soruce: recentf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal--fcf-recentf-candidates ()
  (cl-loop for path in (seq-filter #'file-exists-p (-take 20 recentf-list))
        collect (personal--fcf-get-candidate path path nil)))

(defun personal--fcf-recentf-candidate-transformer (candidates sources)
  (personal--fcf-create-candidate-transformer candidates))

;;;;;;;;;;;;;;;;;;;;;;; soruce: emacs configuration file ;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal--fcf-emacs-configurations
  '("settings.el"
    "preload/presetting.el"
    "programs/personal-cc.el"
    "programs/personal-emacs-lisp.el"
    "programs/personal-lsp.el"
    "programs/personal-programming.el"
    "programs/personal-python.el"
    "programs/personal-shell.el"
    "programs/personal-shell.el"
    "modules/personal-editor.el"
    "modules/personal-entry.el"
    "modules/personal-helm-swoop.el"
    "modules/personal-helm.el"
    "modules/personal-isearch.el"
    "modules/personal-keys.el"
    "modules/personal-mode.el"
    "modules/personal-modeline.el"
    "modules/personal-neotree.el"
    "packages/personal-movetext.el"
    "packages/personal-pair.el"
    "packages/personal-util.el"
    "packages/personal-windows.el"
    )
  "Configuration files for emacs environment.")

(defvar personal--fcf-emacs-show-type 'spmn
  "Type of message to show. The value can be the following:
   -- snmn:  # show name match name
   -- spmp:  # show path match path
   -- spmn:  # show path match name ")

(defun personal--fcf-emacs-candidates ()
  "Collect emacs configuration files for helm candidates."
  (let ((sp (not (eq personal--fcf-emacs-show-type 'snmn))))
    (cl-loop for name in personal--fcf-emacs-configurations
          collect (personal--fcf-get-candidate
                   nil name prelude-personal-dir sp))))

(defun personal--fcf-emacs-candidate-transformer (candidates sources)
  (personal--fcf-create-candidate-transformer
   (when (eq personal--fcf-emacs-show-type 'spmn)
     (setq candidates (personal--fcf-match-name-filter
                       candidates prelude-personal-dir)))
   (personal--fcf-sort candidates '(existp recent length))))

;;;;;;;;;;;;;;;;;;;;;;; soruce: shell configuration file ;;;;;;;;;;;;;;;;;;;;;;;

(defvar personal--fcf-shell-configurations
  '("~/.oh-my-zsh/custom/settings.zsh"
    "~/.oh-my-zsh/custom/functions.zsh"
    "~/.oh-my-zsh/custom/inputrc.zsh"
    "~/.zshrc"
    "~/.bashrc"
    "~/.bash_profile")
  "Configuration files for shell environment.")

(defvar personal--fcf-shell-show-type 'spmn
  "Type of message to show. The value can be the following:
   -- snmn:  # show name match name
   -- spmp:  # show path match path
   -- spmn:  # show path match name ")

(defun personal--fcf-shell-candidates ()
  "Collect shell configuration files for helm candidates."
  (let ((sp (not (eq personal--fcf-shell-show-type 'snmn))))
    (cl-loop for path in personal--fcf-shell-configurations
          collect (personal--fcf-get-candidate
                   path (file-name-nondirectory path) nil sp))))

(defun personal--fcf-shell-candidate-transformer (candidates sources)
  (personal--fcf-create-candidate-transformer
   (when (eq personal--fcf-shell-show-type 'spmn)
     (setq candidates (personal--fcf-match-name-only-filter candidates)))
   (personal--fcf-sort candidates '(existp recent length))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main entry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal--fcf-build-helm-source (name candidates transformer)
  (declare (indent 1))
  (helm-build-sync-source name
    :candidates candidates
    :action #'helm-type-file-actions
    :persistent-action #'helm-ff-kill-or-find-buffer-fname
    :keymap personal--fcf-keymap
    :filtered-candidate-transformer transformer
    :fuzzy-match nil
    :allow-dups nil))

(defun personal-find-confiuration-file ()
  "Find configuration files from various sources."
  (interactive)
  (let ((root (projectile-project-root))
        (names '("recentf" "test" "emacs" "shell")))
    ;; `project' is not added into the sources if we already in emacs.d.
    (and root
         (not (string= root prelude-dir))
         (push "project" names))
    (personal--fcf-context-put 'names names)
    (personal--fcf-context-put 'actives names))

  (helm :sources
        (list (personal--fcf-build-helm-source "project"
                #'personal--fcf-project-candidates
                #'personal--fcf-project-candidate-transformer)
              (personal--fcf-build-helm-source "recentf"
                #'personal--fcf-recentf-candidates
                #'personal--fcf-recentf-candidate-transformer)
              (personal--fcf-build-helm-source "test"
                #'personal--fcf-testfile-candidates
                #'personal--fcf-testfile-candidate-transformer)
              (personal--fcf-build-helm-source "emacs"
                #'personal--fcf-emacs-candidates
                #'personal--fcf-emacs-candidate-transformer)
              (personal--fcf-build-helm-source "shell"
                #'personal--fcf-shell-candidates
                #'personal--fcf-shell-candidate-transformer))
        :buffer "*helm-find-configuration-file*"))

;;;;;;;;;;;;;;;;;;;;;;; personal-jump-to-thing-at-point ;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal--jtf-find-file-by-name (name root)
  "Find file by NAME in ROOT."
  (split-string
   (shell-command-to-string
    (format "find %s -type f -name '%s'" (expand-file-name root) name))))

(defun personal--fcf-get-files-at-point ()
  "Get filenames at current point in current project."
  (let ((path (thing-at-point 'filename t)) name root files keyf)
    (when path
      (setq path (expand-file-name path))
      (setq name (file-name-nondirectory path))
      (setq root (projectile-project-root))
      (setq keyf (lambda (x) (length (s-shared-end path x))))
      (when (file-exists-p path) (setq files (list path)))
      (when (and root (not files))
        (setq files (personal--jtf-find-file-by-name name root))
        (setq files (cl-sort files #'> :key keyf))))
    files))

(defun personal--fcf-get-files-match-buffer-name ()
  "Get filenames whose names match the current buffer file name."
  (when-let* ((path (buffer-file-name))
              (name (file-name-base path))
              (root (projectile-project-root))
              (files (projectile-project-files root)))
    (setq name (string-remove-prefix "test_" name))
    (setq name (string-remove-prefix "unittest_" name))
    (unless (string-empty-p name)
      (seq-filter
       (lambda (x) (s-contains-p name (file-name-nondirectory x)))
       (delete path (mapcar (lambda (x) (expand-file-name x root)) files))))))

(defun personal--jtf-get-files ()
  "Get filename from current point or buffer file name."
  (or (personal--fcf-get-files-at-point)
      (personal--fcf-get-files-match-buffer-name)))

(defun personal-jump-to-file-at-point (&optional other-window)
  "Jump to file specified by name under cursor."
  (interactive)
  (let* ((files (personal--jtf-get-files))
         (curr-wfun #'find-file)
         (curr-ofun #'find-file-other-window)
         (helm-wfun #'helm-type-file-actions)
         (helm-ofun #'helm-find-files-other-window)
         (file-fn (if other-window curr-ofun curr-wfun))
         (helm-fn (if other-window helm-ofun helm-wfun)))
    (cond
     ((= (length files) 1)
      (apply file-fn (car files) nil))
     ((> (length files) 1)
      (helm :sources
            (helm-build-sync-source "files"
              :candidates files
              :action helm-fn
              :persistent-action #'helm-ff-kill-or-find-buffer-fname)))
     (t (message "Not find anything to jump to.")))))

(defun personal-jump-to-file-at-point-other-window ()
  (interactive)
  (personal-jump-to-file-at-point t))

;;;;;;;;;;;;;;;;;;;;;;;; word navigation & manipulation ;;;;;;;;;;;;;;;;;;;;;;;;

(defun personal-forward-word ()
  "Move forward one word but not cross multiple lines."
  (interactive "^")
  (let ((word-end (point-max))
        (sign-end (point-max))
        (line-end (line-end-position)) finish)
    ;; 先前向移动到非空白字符
    (when (looking-at "[ \t]+") (goto-char (match-end 0)))
    ;; 光标位于行尾时, 前进到下一行的第一个非空白字符
    (when (= (point) line-end)
      (forward-char 1)
      (when (looking-at "[ \t]+") (goto-char (match-end 0)))
      (setq finish t))
    (save-excursion
      ;; word-end遵循常规的word定义 (即: syntax table)
      (forward-word) (setq word-end (point)))
    (save-excursion
      ;; sign-end仅仅是空白字符的开头
      (when (re-search-forward "[ \t]" nil t)
        (setq sign-end (match-beginning 0))))
    (when (not finish)
      (goto-char (min word-end sign-end line-end)))))

(defun personal-backward-word ()
  "Move backward one word but not cross multiple lines."
  (interactive "^")
  (let ((word-beg (point-min))
        (sign-beg (point-min))
        (line-beg (line-beginning-position)) finish)
    ;; 光标位于行首时, 回退到上一行的非空白字符
    (when (= (point) line-beg)
      (backward-char 1)
      (when (looking-back "[ \t]+" nil t) (goto-char (match-beginning 0)))
      (setq finish t))
    (when (not finish)
      ;; 光标与行首之间只有空白字符时, 回退到行首
      (when (looking-back "[ \t]+" nil t) (goto-char (match-beginning 0)))
      (setq finish (= (point) line-beg)))
    (when (not finish)
      (save-excursion
        ;; word-beg遵循常规的word定义 (即: syntax table)
        (backward-word) (setq word-beg (point)))
      (save-excursion
        ;; sign-beg仅仅是空白字符的结尾
        (when (re-search-backward "[ \t]" nil t)
          (setq sign-beg (match-end 0))))
      (goto-char (max word-beg sign-beg line-beg)))))

(defun personal-kill-word ()
  "Kill word forward but not cross multiple lines."
  (interactive)
  (kill-region (point) (progn (personal-forward-word) (point))))

(defun personal-backward-kill-word ()
  "Kill word backward but not cross multiple lines."
  (interactive)
  (kill-region (point) (progn (personal-backward-word) (point))))

(provide 'personal-util)

;;; personal-util.el ends here
