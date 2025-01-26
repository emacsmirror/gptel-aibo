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

(defvar-local aics-context--working-buffer nil
  "Aics working buffer name.")

(defun aics-context--wrap (message contexts)
  "Wrap MESSAGE with CONTEXTS for GPTel."
  (let ((context-string
         (concat (gptel-context--string contexts)
                 "\n\n---\n\nRequest context:\n\n\n"
                 (aics-context--info aics-context--working-buffer))))
    ;; (message "context: %s" context-string)
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
            (let ((content-before-cursor (buffer-substring-no-properties (point-min) (point)))
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
            (let ((content-after-cursor (buffer-substring-no-properties (point) (point-max)))
                  (fragment-after-cursor (aics-context--fragment-after-cursor)))
              (concat
               (aics-context--make-fenced-code-block fragment-after-cursor)
               (if (equal content-after-cursor fragment-after-cursor)
                   ""
                 "\n..."))))))

(defun aics-context--buffer-info ()
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
    (concat (format "Filepath: %s  \n"
                    (if buffer-file-name
                        (concat "`" buffer-file-name "`")
                      "(not associated with a file)"))
            "Content:  \n"
            (if buffer-content
                (aics-context--make-fenced-code-block buffer-content)
              "(empty)"))))

(defun aics-context--project-buffers-info ()
  "Get information about other buffers in the same project."
  (if-let ((proj (project-current)))
      (let ((buffers (aics-context--project-buffers))
            info)
        (if buffers
            (progn
              (setq info "\nOther buffers in the same project:\n\n")
              (dolist (buf buffers)
                (with-current-buffer buf
                  (setq info (concat
                              info
                              (format "`%s`:  \n" (buffer-name buf))
                              (aics-context--buffer-info)
                              "\n\n"))))
              info)
          ""))
    ""))

(defun aics-context--project-buffers ()
  "Get a list of buffers belonging to the same project as the current buffer, excluding the current buffer itself.
If the current buffer does not belong to a project, return an empty list."
  (when-let ((project-current (project-current)))
    (let ((project-root (project-root project-current))
          (current-buffer (current-buffer)))
      (seq-filter
       (lambda (buf)
         (and (not (eq buf current-buffer)) ; Exclude the current buffer
              (buffer-file-name buf)
              (with-current-buffer buf
                (when-let ((buf-project (project-current)))
                  (equal (project-root buf-project) project-root)))))
       (buffer-list)))))

(defun aics-context--fragment-before-cursor ()
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
            (setq suffix (buffer-substring-no-properties point-pos (line-end-position)))
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
  "Return a string of current directory listing if in a project, else empty string."
  (if-let ((proj (project-current)))
      (let ((current-dir (file-name-directory (or buffer-file-name default-directory))))
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
          (if (string= (file-name-directory (or buffer-file-name default-directory))
                       (aics-context--project-root (project-current)))
              top-info
            (concat top-info "\n" current-info))))
    ""))

(defun aics-context--project-top-directory-info ()
  "Return a string of top-level directory listing if in a project, else empty string."
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
  "Generate a code fence that's long enough to encapsulate CONTENT.
The fence will be one backtick longer than the longest sequence of backticks in CONTENT,
with a minimum of 3 backticks."
  (let ((max-backticks 0)
        (start 0))
    (while (string-match "`+" content start)
      (setq max-backticks (max max-backticks (- (match-end 0) (match-beginning 0))))
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

(defun aics-context--flycheck-current-errors ()
  "Return formatted Flycheck errors in the current buffer as a Markdown unordered list.
If there are no Flycheck errors, return nil."
  (when (boundp 'flycheck-current-errors)
    (if (null flycheck-current-errors)
        nil
      (concat "Flycheck Errors of the buffer:\n"
              (mapconcat
               (lambda (err)
                 (format "- Line %d, Column %d (%s): %s"
                         (flycheck-error-line err)
                         (or (flycheck-error-column err) 0)
                         (capitalize (symbol-name (flycheck-error-level err)))
                         (flycheck-error-message err)))
               flycheck-current-errors
               "\n")))))

(provide 'aics-context)
;;; aics-context.el ends here
