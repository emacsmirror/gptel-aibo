;;; gptai-context.el --- Context for gptel-aibo -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sun Yi Ming

;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Context handling functions for gptel-aibo

;;; Code:


(require 'gptel)
(require 'gptel-context)
(require 'imenu)

(defun gptai-context-wrap (message contexts)
  "Wrap MESSAGE with CONTEXTS for gptel."
  (let ((context-string
         (concat "---

Request context:

**Note**: This context reflects the *latest state* of the user's environment. \
Previous suggested actions may be not executed, and the user may have made \
arbitrary modifications outside this conversation.

"
                 (gptai-context-info gptai--working-buffer)
                 (gptel-context--string contexts))))
    ;; (message "context: %s" context-string)
    (if (> (length context-string) 0)
        (pcase-exhaustive gptel-use-context
          ('system (concat message "\n\n" context-string))
          ('user   (concat message "\n\n" context-string))
          ('nil    message))
      message)))

(defun gptai-context-info (&optional buffer)
  "Get context information for BUFFER."
  (concat (gptai-context--working-buffer-info buffer)
          "\n\n"
          (gptai-context--project-buffers-info buffer)))

(defun gptai-context--working-buffer-info (&optional buffer)
  "Get context information about BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((point (point))
           (widen-point-max (save-restriction (widen) (point-max)))
           (active-buffer-size (- (point-max) (point-min)))
           (context-fragment-boundaries
            (gptai-context--fragment-boundaries
             (if (<= active-buffer-size gptai--max-buffer-size)
                 gptai--max-context-fragment-size
               gptai--max-buffer-size)))
           (before-start (car context-fragment-boundaries))
           (after-end (cdr context-fragment-boundaries)))
      (concat
       (format "Current working buffer: `%s`\n\n" (buffer-name))
       (if (<= active-buffer-size gptai--max-buffer-size)
           (gptai-context--buffer-info)
         (gptai-context--buffer-filename-info))
       "\n"
       "Fragment before the cursor:  \n"
       (if (= point 1)
           "(cursor is at the beginning of the buffer)  "
         (concat (unless (= before-start 1) "...\n")
                 (gptai-context--make-fenced-code-block
                  (buffer-substring-no-properties before-start point))))
       "\n\n"
       "Fragment after the cursor:  \n"
       (if (= point widen-point-max)
           "(cursor is at the end of the buffer)  "
         (concat
          (gptai-context--make-fenced-code-block
           (buffer-substring-no-properties point after-end))
          (unless (= after-end widen-point-max)
            "\n...")))))))

(defun gptai-context--buffer-info (&optional buffer)
  "Get buffer information including file path and content.

When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((buffer-content
           (buffer-substring-no-properties (point-min) (point-max)))
          (language-identifier
           (gptai-context--mode-to-language-identifier major-mode)))
      (concat (gptai-context--buffer-filename-info)
              "Content:  \n"
              (if buffer-content
                  (gptai-context--make-fenced-code-block
                   buffer-content
                   language-identifier)
                "(empty)")
              "\n"))))

(defun gptai-context--buffer-filename-info (&optional buffer)
"Return the file path info associated with BUFFER.

BUFFER is the buffer to check, or the current buffer if nil."
  (format "Filepath: %s  \n"
          (if-let ((file-name (buffer-file-name buffer)))
              (concat "`" file-name "`")
            "(not associated with a file)")))

(defun gptai-context--buffer-empty-p (&optional buffer)
  "Check if BUFFER is empty."
  (with-current-buffer (or buffer (current-buffer))
    (= (point-min) (point-max))))

(defun gptai-context--buffer-supports-imenu-p (&optional buffer)
  "Return non-nil if BUFFER supports imenu indexing.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (not (eq imenu-create-index-function
                 'imenu-default-create-index-function))
        (or (and imenu-prev-index-position-function
                 imenu-extract-index-name-function)
            (and imenu-generic-expression)))))

(defun gptai-context--buffer-outline-info (&optional buffer)
  "Get buffer information including file path and outline.
When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if-let ((outline (gptai-context--imenu-outline (current-buffer))))
        (concat (format "Filepath: `%s`\n" buffer-file-name)
                "Outline:\n"
                outline))))

(defun gptai-context--imenu-outline (&optional buffer)
  "Generate hierarchical outline from imenu index of BUFFER.
Return empty string if BUFFER is nil or imenu index unavailable."
  (with-current-buffer (or buffer (current-buffer))
    (when-let
        ((index (let ((imenu-auto-rescan t))
                  (ignore-errors (imenu--make-index-alist)))))
      (gptai-context--imenu-index-to-string index 0))))

(defun gptai-context--imenu-index-to-string (index depth)
  "Convert an imenu INDEX alist to a hierarchical string.
DEPTH is the current depth for indentation."
  (mapconcat
   (lambda (item)
     (cond
      ((and (listp item) (listp (car item)))
       (gptai-context--imenu-index-to-string item depth))
      ((listp (cdr item))
       (let ((heading (car item))
             (subitems (cdr item)))
         (unless (string= heading ".")
           (concat
            (make-string (* 2 depth) ?\s)
            (format "- %s\n" (gptai-context--imenu-item-title heading))
            (gptai-context--imenu-index-to-string subitems (1+ depth))))))
      ((and (consp item) (not (string= (car item) ".")))
       (concat
        (make-string (* 2 depth) ?\s)
        (format "- %s\n"
                (gptai-context--imenu-item-title (car item)))))
      (t "")))
   index ""))

(defun gptai-context--imenu-item-title (item)
  "Extract the string title from ITEM, stripping text properties if present."
  (cond
   ((stringp item) (substring-no-properties item))
   ((and (vectorp item) (stringp (aref item 0)))
    (substring-no-properties item))
   (t (format "%s" item))))

(cl-defun gptai-context--project-buffers-info (&optional buffer quota)
  "Get information about other buffers in the same project of BUFFER.

The total size of the returned information will be limited by QUOTA."
  (let* ((buffers (gptai-context--project-buffers buffer))
         (buffer-infos nil)
         (current-size 0)
         (buffer-count 0))

    (cl-loop
     for buf in buffers
     until (>= buffer-count gptai--max-project-buffer-count)
     do
     (when-let*
         ((buffer-size (buffer-size buf))
          (buffer-info
           (if (<= buffer-size gptai--max-project-buffer-size)
               (gptai-context--buffer-info buf)
             (gptai-context--buffer-outline-info buf)))
          (buffer-info-size (length buffer-info)))
       (when (or (not quota) (<= (+ current-size buffer-info-size) quota))
         (push (cons buf buffer-info) buffer-infos)
         (setq current-size (+ current-size buffer-info-size))
         (setq buffer-count (1+ buffer-count)))))

    (concat
     (when buffer-infos
       (concat "Other buffers in the same project:\n\n"
               (mapconcat (lambda (info)
                            (concat "`" (buffer-name (car info)) "`:\n"
                                    (cdr info)))
                          (nreverse buffer-infos) "\n")
               "\n\n")))))

(defun gptai-context--project-buffers (&optional buffer)
  "Get buffers in the same project as BUFFER, itself excluded."
  (let ((current-buffer (or buffer (current-buffer))))
    (when-let ((project-current (with-current-buffer current-buffer
                                  (project-current))))
      (seq-filter
       (lambda (buf)
         (and (not (eq buf current-buffer))
              (buffer-file-name buf)
              (with-current-buffer buf
                (equal (project-current) project-current))))
       (buffer-list)))))

(defun gptai-context--fragment (max-context-length)
  "Extract the text fragment around the point.

The total length is limited by MAX-CONTEXT-LENGTH"
  (let* ((boundaries (gptai-context--fragment-boundaries max-context-length))
         (start (car boundaries))
         (end (cdr boundaries))
         (before-text (buffer-substring-no-properties start (point)))
         (after-text (buffer-substring-no-properties (point) end)))
    (cons before-text after-text)))

(defun gptai-context--fragment-boundaries
    (max-context-length &optional buffer pos)
  "Compute context boundaries around POS within MAX-CONTEXT-LENGTH chars.

BUFFER is the buffer to use, or the current buffer if nil.
POS is the position to center on, or the current point if nil."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((pos (or pos (point)))
           (before-start
            (save-excursion
              (while (progn
                       (beginning-of-defun)
                       (not (gptai-context--unique-region-p (point) pos))))
              (point)))
           (after-end (save-excursion
                        (end-of-defun)
                        (point)))
           (before-len (- pos before-start))
           (after-len (- after-end pos))
           (total-len (+ before-len after-len)))

      (if (<= total-len max-context-length)
          (cons before-start after-end)

        (let* ((half-limit (/ max-context-length 2))
               (tolerance-limit (* max-context-length 0.6)))
          (cond
           ((and (<= before-len tolerance-limit)
                 (<= before-len after-len))
            (cons before-start
                  (+ pos (- max-context-length before-len))))

           ((and (<= after-len tolerance-limit)
                 (<= after-len before-len))
            (cons (- pos (- max-context-length after-len))
                  after-end))

           (t
            (cons (- pos half-limit)
                  (+ pos half-limit)))))))))

(defun gptai-context--unique-region-p (beg end)
  "Check if the text between BEG and END appears uniquely in the buffer.

BEG is the starting position of the region.
END is the ending position of the region."
  (let ((region-text (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward region-text nil t)
        (eq (match-beginning 0) beg)))))

(defun gptai-context--fragment-before-cursor ()
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

(defun gptai-context--fragment-after-cursor ()
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

(defun gptai-context--project-current-directory-info ()
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

(defun gptai-context--project-root (project)
  "Get the root directory of PROJECT.
Returns: The project root directory as a string, or nil if not found."
  (cond
   ((fboundp 'project-root)
    (project-root project))
   ((fboundp 'project-roots)
    (car (project-roots project)))))

(defun gptai-context--project-directory-info ()
  "Return project directory information based on current location."
  (if (project-current)
      (let ((top-info (gptai-context--project-top-directory-info))
            (current-info (gptai-context--project-current-directory-info)))
        (if (string-empty-p top-info)
            ""
          (if (string= (file-name-directory
                        (or buffer-file-name default-directory))
                       (gptai-context--project-root (project-current)))
              top-info
            (concat top-info "\n" current-info))))
    ""))

(defun gptai-context--project-top-directory-info ()
  "Return formatted string of top-level directory listing.
If in a project, returns the listing, else returns empty string."
  (if-let ((proj (project-current)))
      (let ((project-root (gptai-context--project-root proj)))
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

(defun gptai-context--make-fenced-code-block (content &optional language)
  "Wrap CONTENT in a fenced code block with optional LANGUAGE identifier."
  (let ((fence (gptai-context--make-code-fence content)))
    (concat fence (or language "") "\n" content "\n" fence)))

(defun gptai-context--make-code-fence (content)
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

(defun gptai-context--mode-to-language-identifier (mode)
  "Convert MODE to code block language identifier."
  (let* ((mode-name (symbol-name mode))
         (mode-mapping
          '(("emacs-lisp-mode" . "elisp")
            ("lisp-mode" . "lisp")
            ("clojure-mode" . "clojure")
            ("python-mode" . "python")
            ("ruby-mode" . "ruby")
            ("js-mode" . "javascript")
            ("js2-mode" . "javascript")
            ("typescript-mode" . "typescript")
            ("c-mode" . "c")
            ("c++-mode" . "cpp")
            ("java-mode" . "java")
            ("go-mode" . "go")
            ("rust-mode" . "rust")
            ("sh-mode" . "shell")
            ("shell-mode" . "shell")
            ("css-mode" . "css")
            ("scss-mode" . "scss")
            ("html-mode" . "html")
            ("xml-mode" . "xml")
            ("sql-mode" . "sql")
            ("markdown-mode" . "markdown")
            ("yaml-mode" . "yaml")
            ("dockerfile-mode" . "dockerfile")
            ("json-mode" . "json")
            ("text-mode" . "text")))
         (lang (cdr (assoc mode-name mode-mapping))))
    (or lang
        (replace-regexp-in-string "-mode$" "" mode-name))))

(defun gptai-context--indent (content depth)
  "Indent CONTENT by DEPTH spaces at the start of each line.
Returns the indented content as a string."
  (let ((lines (split-string content "\n")))
    (mapconcat (lambda (line)
                 (concat (make-string depth ? ) line))
               lines
               "\n")))

(provide 'gptai-context)
;;; gptai-context.el ends here
