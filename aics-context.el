;;; aics-context.el --- Context handling for AICS -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sun Yi Ming

;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity

;;; Commentary:
;;
;; Context handling functions for AICS
;;

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'imenu)

(defcustom aics-project-buffer-limit 2
  "Maximum number of project buffers to include when sending context to LLM.
This should be a positive integer."
  :type 'natnum
  :group 'aics
  :safe #'natnump)

(defcustom aics-project-outline-buffer-limit 3
  "Maximum number of project buffers whose outlines will be sent to LLM.
This should be a positive integer."
  :type 'natnum
  :group 'aics
  :safe #'natnump)

(defvar-local aics-context--working-buffer nil
  "Aics working buffer name.")

(defun aics-context--wrap (message contexts)
  "Wrap MESSAGE with CONTEXTS for GPTel."
  (let ((context-string
         (concat (gptel-context--string contexts)
                 "

---

Request context:
**Note**: This context reflects the *latest state* of the user's environment. \
It is dynamically updated during the conversation.


"
                 (aics-context--info aics-context--working-buffer))))
    (message "context: %s" context-string)
    (if (> (length context-string) 0)
        (pcase-exhaustive gptel-use-context
          ('system (concat message "\n\n" context-string))
          ('user   (concat context-string "\n\n" message))
          ('nil    message))
      message)))

(defun aics-context--info (&optional buffer)
  "Get context information for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (concat (aics-context--current-buffer-info)
            "\n\n"
            (aics-context--project-buffers-info))))

(defun aics-context--current-buffer-info ()
  "Get information about current buffer."
  (concat (format "Current buffer: `%s`\n\n" (buffer-name))
          (aics-context--buffer-info)
          "\n\n"
          "Fragment before the cursor:  \n"
          (if (= (point) (point-min))
              "(cursor is at the beginning of the buffer)  "
            (let ((content-before-cursor
                   (buffer-substring-no-properties (point-min) (point)))
                  (fragment-before-cursor (aics-context--fragment-before-cursor)))
              (concat
               (if (equal content-before-cursor fragment-before-cursor)
                   ""
                 "...\n")
               (aics-context--make-fenced-code-block fragment-before-cursor))))
          "\n\n"
          "Fragment after the cursor:  \n"
          (if (= (point) (point-max))
              "(cursor is at the end of the buffer)  "
            (let ((content-after-cursor
                   (buffer-substring-no-properties (point) (point-max)))
                  (fragment-after-cursor (aics-context--fragment-after-cursor)))
              (concat
               (aics-context--make-fenced-code-block fragment-after-cursor)
               (if (equal content-after-cursor fragment-after-cursor)
                   ""
                 "\n..."))))))

(defun aics-context--buffer-info (&optional buffer)
  "Get buffer information including file path and content.
When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((buffer-content
           (buffer-substring-no-properties (point-min) (point-max))))
      (concat (format "Filepath: %s  \n"
                      (if buffer-file-name
                          (concat "`" buffer-file-name "`")
                        "(not associated with a file)"))
              "Content:  \n"
              (if buffer-content
                  (aics-context--make-fenced-code-block buffer-content)
                "(empty)")))))

(defun aics-context--buffer-empty-p (&optional buffer)
  "Check if BUFFER is empty."
  (with-current-buffer (or buffer (current-buffer))
    (= (point-min) (point-max))))

(defun aics-context--buffer-supports-imenu-p (&optional buffer)
  "Return non-nil if BUFFER supports imenu indexing.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (not (eq imenu-create-index-function
                 'imenu-default-create-index-function))
        (or (and imenu-prev-index-position-function
                 imenu-extract-index-name-function)
            (and imenu-generic-expression)))))

(defun aics-context--buffer-outline-info (&optional buffer)
  "Get buffer information including file path and outline.
When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((outline (aics-context--imenu-outline (current-buffer))))
      (concat (format "Filepath: `%s`\n" buffer-file-name)
              "Outline:\n"
              (if (string-empty-p outline)
                  "(empty outline)"
                outline)))))

(defun aics-context--imenu-outline (&optional buffer)
  "Generate hierarchical outline from imenu index of BUFFER.
Return empty string if BUFFER is nil or imenu index unavailable."
  (with-current-buffer (or buffer (current-buffer))
      (let ((imenu-auto-rescan t))
        (let ((index (ignore-errors (imenu--make-index-alist))))
          (if (and index (listp index))
              (aics-context--imenu-index-to-string index 0)
            "")))))

(defun aics-context--imenu-index-to-string (index depth)
  "Convert an imenu INDEX alist to a hierarchical string.
DEPTH is the current depth for indentation."
  (mapconcat
   (lambda (item)
     (cond
      ((and (listp item) (listp (car item)))
       (aics-context--imenu-index-to-string item depth))
      ((listp (cdr item))
       (let ((heading (car item))
             (subitems (cdr item)))
         (unless (string= heading ".")
           (concat
            (make-string (* 2 depth) ?\s)
            (format "- %s\n" (aics-context--imenu-item-title heading))
            (aics-context--imenu-index-to-string subitems (1+ depth))))))
      ((and (consp item) (not (string= (car item) ".")))
       (concat
        (make-string (* 2 depth) ?\s)
        (format "- %s\n"
                (aics-context--imenu-item-title (car item)))))
      (t "")))
   index ""))

(defun aics-context--imenu-item-title (item)
  "Extract the string title from ITEM, stripping text properties if present."
  (cond
   ((stringp item) (substring-no-properties item))
   ((and (vectorp item) (stringp (aref item 0)))
    (substring-no-properties item))
   (t (format "%s" item))))

(defun aics-context--project-buffers-info ()
  "Get information about other buffers in the same project."
  (let* ((buffers (aics-context--project-buffers))
         (recent-buffers (seq-take buffers aics-project-buffer-limit))
         (extra-buffers (seq-drop buffers aics-project-buffer-limit))
         (buffer-infos nil)
         (outline-infos nil))

    (dolist (buf recent-buffers)
      (push (cons buf (aics-context--buffer-info buf)) buffer-infos))

    (let ((current-count 0))
      (cl-loop for buf in extra-buffers
               until (= aics-project-outline-buffer-limit current-count)
               when (and (not (aics-context--buffer-empty-p buf))
                         (aics-context--buffer-supports-imenu-p buf))
               do
               (push (cons buf (aics-context--buffer-outline-info buf))
                     outline-infos)
               (setq current-count (1+ current-count))))

    (concat
     (when buffer-infos
       (concat "Other buffers in the same project:\n\n"
               (mapconcat (lambda (info)
                            (concat "`" (buffer-name (car info)) "`:\n"
                                    (cdr info)))
                          (nreverse buffer-infos) "\n\n")
               "\n\n"))
     (when outline-infos
       (concat "Other buffer outlines in the same project:\n\n"
               (mapconcat (lambda (info)
                            (concat "`" (buffer-name (car info)) "`:\n"
                                    (cdr info)))
                          (nreverse outline-infos) "\n\n")
               "\n")))))

(defun aics-context--project-buffers ()
  "Get buffers in same project as current buffer, excluding current buffer.
Return a list of buffers that belong to the same project as the current
buffer, excluding the current buffer itself. If the current buffer does
not belong to a project, return an empty list."
  (when-let ((project-current (project-current)))
    (let ((project-root (aics-context--project-root project-current))
          (current-buffer (current-buffer)))
      (seq-filter
       (lambda (buf)
         (and (not (eq buf current-buffer)) ; Exclude current buffer
              (buffer-file-name buf)
              (with-current-buffer buf
                (when-let ((buf-project (project-current)))
                  (equal (aics-context--project-root buf-project)
                         project-root)))))
       (buffer-list)))))

(defun aics-context--fragment-before-cursor ()
  "Get a meaningful fragment of text before the cursor.

The function collects text starting from the cursor position and continues
collecting lines backwards until one of the following conditions is met:
1. Reaches the beginning of buffer
2. Finds 3 non-blank lines that form a unique prefix in the buffer
3. The collected text forms a unique pattern in the buffer

Returns a string containing the collected text fragment."
  (let ((point-pos (point))
        (stop nil)
        (prefix)
        (non-blank-line-count 0))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 point-pos)))
      (setq prefix line)
      (unless (string-match-p "\\`[[:space:]]*\\'" line)
        (setq non-blank-line-count (1+ non-blank-line-count))))
    (save-excursion
      (while (not stop)
        (if (= (point) (point-min))
            (setq stop t)
          (progn
            (forward-line -1)
            (setq prefix (buffer-substring-no-properties (point) point-pos))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-match-p "\\`[[:space:]]*\\'" line)
                (setq non-blank-line-count (1+ non-blank-line-count))
                (if (and (>= non-blank-line-count 3)
                         (save-excursion
                           (goto-char (point-min))
                           (let ((count 0))
                             (while (and (< count 2)
                                         (search-forward prefix nil t))
                               (setq count (1+ count)))
                             (= count 1))))
                    (setq stop t))))))))
    prefix))

(defun aics-context--fragment-after-cursor ()
  "Get a meaningful fragment of text after the cursor.

The function collects text starting from the cursor position and continues
collecting lines until one of the following conditions is met:
1. Reaches the end of buffer
2. Finds 3 non-blank lines that form a unique prefix in the buffer
3. The collected text forms a unique pattern in the buffer

Returns a string containing the collected text fragment."
  (let ((point-pos (point))
        (stop nil)
        (suffix)
        (non-blank-line-count 0))
    (let ((line (buffer-substring-no-properties
                 point-pos
                 (line-end-position))))
      (setq suffix line)
      (unless (string-match-p "\\`[[:space:]]*\\'" line)
        (setq non-blank-line-count (1+ non-blank-line-count))))
    (save-excursion
      (while (not stop)
        (if (= (point) (point-max))
            (setq stop t)
          (progn
            (forward-line 1)
            (setq suffix
                  (buffer-substring-no-properties
                   point-pos (line-end-position)))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-match-p "\\`[[:space:]]*\\'" line)
                (setq non-blank-line-count (1+ non-blank-line-count))
                (if (and (>= non-blank-line-count 3)
                         (save-excursion
                           (goto-char (point-min))
                           (let ((count 0))
                             (while (and (< count 2)
                                         (search-forward suffix nil t))
                               (setq count (1+ count)))
                             (= count 1))))
                    (setq stop t))))))))
    suffix))

(defun aics-context--project-current-directory-info ()
  "Return current directory listing as a string if in a project.
If not in a project, return empty string.
The listing includes files and directories, with '/' appended to directory
names."
  (if-let ((proj (project-current)))
      (let ((current-dir (file-name-directory
                          (or buffer-file-name default-directory))))
        (with-temp-buffer
          (insert "Files in the project's current directory:\n```\n")
          (dolist (file (directory-files current-dir))
            (unless (member file '("." ".."))
              (insert file)
              (when (file-directory-p (expand-file-name file current-dir))
                (insert "/"))
              (insert "\n")))
          (insert "```\n")
          (buffer-string)))
    ""))

(defun aics-context--project-root (project)
  "Get the root directory of PROJECT.
Returns: The project root directory as a string, or nil if not found."
  (cond
   ((fboundp 'project-root)
    (project-root project))
   ((fboundp 'project-roots)
    (car (project-roots project)))))

(defun aics-context--project-directory-info ()
  "Return project directory information based on current location."
  (if (project-current)
      (let ((top-info (aics-context--project-top-directory-info))
            (current-info (aics-context--project-current-directory-info)))
        (if (string-empty-p top-info)
            ""
          (if (string= (file-name-directory
                        (or buffer-file-name default-directory))
                       (aics-context--project-root (project-current)))
              top-info
            (concat top-info "\n" current-info))))
    ""))

(defun aics-context--project-top-directory-info ()
  "Return formatted string of top-level directory listing.
If in a project, returns the listing, else returns empty string."
  (if-let ((proj (project-current)))
      (let ((project-root (aics-context--project-root proj)))
        (with-temp-buffer
          (insert "Files in the project's top directory:\n```\n")
          (dolist (file (directory-files project-root))
            (unless (member file '("." ".."))
              (insert file)
              (when (file-directory-p (expand-file-name file project-root))
                (insert "/"))
              (insert "\n")))
          (insert "```\n")
          (buffer-string)))
    ""))

(defun aics-context--make-fenced-code-block (content)
  "Wrap CONTENT in a fenced code block using appropriate fence length.
The fence is generated using `aics-context--make-code-fence' function."
  (let ((fence (aics-context--make-code-fence content)))
    (concat fence "\n" content "\n" fence)))

(defun aics-context--make-code-fence (content)
  "Generate a code fence string that safely encapsulates CONTENT.
The fence length is determined by:
1. The longest sequence of consecutive backticks in CONTENT
2. Always at least one backtick longer than the longest sequence
3. Minimum length of 3 backticks

CONTENT: String to be wrapped in code fence
Returns: String containing the appropriate number of backticks"
  (let ((max-backticks 0)
        (start 0))
    (while (string-match "`+" content start)
      (setq max-backticks (max max-backticks
                               (- (match-end 0) (match-beginning 0))))
      (setq start (match-end 0)))
    (make-string (max 3 (1+ max-backticks)) ?`)))

(defun aics-context--indent (content depth)
  "Indent CONTENT by DEPTH spaces at the start of each line.
Returns the indented content as a string."
  (let ((lines (split-string content "\n")))
    (mapconcat (lambda (line)
                 (concat (make-string depth ? ) line))
               lines
               "\n")))

(provide 'aics-context)
;;; aics-context.el ends here
