;;; aics.el --- An AI Writing Assistant -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Maintainer: Sun Yi Ming <dolmens@gmail.com>
;; Created: January 09, 2025
;; Modified: January 09, 2025
;; Version: 0.0.1
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity
;; Homepage: https://github.com/sam/aics
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  AICS is an AI-powered writing assistant that helps users create and manage content
;;  in Emacs, including programs, documents, papers, and novels. It provides various
;;  operations like buffer modifications, file creation/deletion, and Elisp execution.
;;
;;; Code:

(require 'gptel)

(defvar aics-system-role
  "You are an expert assistant specializing in helping users with Emacs for \
creating and managing various types of content, including code, documents, \
academic papers, and even novels.")

(defvar aics-system-message
  (concat
   aics-system-role
   "\n"
   "Based on the user's request, you can generate one or more of the following \
actions:
* Modify buffers
* Create files
* Delete files
* Execute a piece of Elisp code

If multiple actions are required, they should be provided in sequence, and the \
program will execute them in the order they are listed. You are free to add \
comments or other explanatory details before, between, or after the actions as \
needed.

The specific format descriptions for these actions are as follows:

## Modify buffers
Start with line:

**OP** MODIFY `<NAME>`

`<NAME>` refers to the name of the buffer being modified. Ensure the name is \
enclosed in backticks.
Following the start line, add one blank line, then specify the SEARCH/REPLACE \
pairs.
Each pair is structured as follows:

Begin with the exact line:

*SEARCH*

Followed by the content to locate, enclosed in a markdown fenced code block.

Then the exact line:

*REPLACE*

Followed by the replacement content, enclosed in a markdown fenced code block.

For example:

**OP** MODIFY `*scratch*`

*SEARCH*
```
hello
```
*REPLACE*
```
good
```
*SEARCH*
```
world
```
*REPLACE*
```
morning
```

**NOTE**
1. Ensure there is **one blank line** between the starting line \
`**OP** MODIFY ...` and the SEARCH/REPLACE pairs.
2. Each SEARCH/REPLACE pair must match the structure shown, with no extra \
content before or after.
3. Consecutive lines that are part of the same modification should be included \
within a single SEARCH/REPLACE pair.
4. Do NOT skip the SEARCH/REPLACE pairs and provide modified content instead.

## Create files
Start with line:

**OP** CREATE `<FILEPATH>`

`<FILEPATH>` is the path of the file to be created and is required.
Followed by the content, enclosed in a markdown fenced code block.

## Delete files
Just one line:

**OP** DELETE `<FILEPATH>`

`<FILEPATH>` is the path of the file to be deleted and is required.

## Execute a piece of Elisp code
Start with line:

**OP** ELISP

Followed by the elisp code snippet, enclosed in a markdown fenced code block.
### Example
**OP** ELISP
```elisp
(insert \"Hello, Emacs!\")
```


"))

(defvar aics-complete-message
  "\nYour task:\n\
Suggest content suitable for insertion at the cursor position.\n\
Requirements:\n\
1. Directly return the content to be inserted. Do not use code blocks, quotes, \
or any other form of wrapping, enclosing, or additional formatting.\n\
2. Do not include any explanations, comments, or extra information.\n\
3. Ensure that the suggested content is consistent with the tone and style of \
the surrounding text.\n\
4. Do not call tools or ask questions to obtain additional information. \
If no suitable content can be suggested, return an empty string.")

;;;###autoload
(define-minor-mode aics-mode
  "Minor mode for aics interacting with LLMs."
  :lighter " aics"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'aics-send)
    map)
  (if aics-mode
      (progn
        (message "enable aics mode")
        (gptel-mode 1))
    (gptel-mode -1)))

;;;###autoload
(defun aics-send ()
  (interactive)
  (let ((system (concat aics-system-message
                        "\n\nRequest context:\n\n"
                        (with-current-buffer "main.cc"
                          (aics--context-info))
                        "\n"
                        "**NOTE**
When including fenced code blocks in your response, ensure the opening \
fence is sufficiently long to correctly encapsulate content containing \
backtick sequences, preventing Markdown parsing issues.\n\n")))
    (message "[[[%s]]]\n" system)
    (gptel-request nil :system system)))

;;;###autoload
(defun aics ()
  (interactive)
  (with-current-buffer
      (get-buffer-create "*aics*")
    (cond
     ((eq gptel-default-mode 'text-mode)
      (text-mode))
     (t (funcall gptel-default-mode)))
    (unless aics-mode (aics-mode 1))))

;;;###autoload
(defun aics-complete-at-point ()
  (interactive)
  (let ((gptel--system-message aics-system-role)
        (prompt (concat (aics--context-info) aics-complete-message)))
    (message prompt)
    (gptel-request prompt)))

(defun aics--context-info ()
  (concat (aics--current-buffer-info)
          "\n\n"
          (aics--project-buffers-info)))

(defun aics--project-directory-info ()
  "Return project directory information based on current location."
  (if (project-current)
      (let ((top-info (aics--project-top-directory-info))
            (current-info (aics--project-current-directory-info)))
        (if (string-empty-p top-info)
            ""
          (if (string= (file-name-directory (or buffer-file-name default-directory))
                       (aics--project-root (project-current)))
              top-info
            (concat top-info "\n" current-info))))
    ""))

(defun aics--project-top-directory-info ()
  "Return a string of top-level directory listing if in a project, else empty string."
  (if-let ((proj (project-current)))
      (let ((project-root (aics--project-root proj)))
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

(defun aics--project-current-directory-info ()
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

(defun aics--project-root (project)
  (cond
   ((fboundp 'project-root)
    (project-root project))
   ((fboundp 'project-roots)
    (car (project-roots project)))))

(defun aics--current-buffer-info ()
  (concat (format "Current buffer: `%s`  \n" (buffer-name))
          (aics--buffer-info)
          "\n"
          "Fragment before the cursor:  \n"
          (if (= (point) (point-min))
              "(cursor is at the beginning of the buffer)  "
            (let ((content-before-cursor (buffer-substring-no-properties (point-min) (point)))
                  (fragment-before-cursor (aics--fragment-before-cursor)))
              (concat
               (if (equal content-before-cursor fragment-before-cursor)
                   ""
                 "\n...\n")
               (aics--make-fenced-code-block fragment-before-cursor))))
          "\n"
          "Fragment after the cursor:  \n"
          (if (= (point) (point-max))
              "(cursor is at the end of the buffer)  "
            (let ((content-after-cursor (buffer-substring-no-properties (point) (point-max)))
                  (fragment-after-cursor (aics--fragment-after-cursor)))
              (concat
               (aics--make-fenced-code-block fragment-after-cursor)
               (if (equal content-after-cursor fragment-after-cursor)
                   ""
                 "\n..."))))))

(defun aics--buffer-info ()
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
    (concat (format "Filepath: %s  \n"
                    (if buffer-file-name
                        (concat "`" buffer-file-name "`")
                      "(not associated with a file)"))
            "Content:  \n"
            (if buffer-content
                (aics--make-fenced-code-block buffer-content)
              "(empty)"))))

(defun aics--flycheck-current-errors ()
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

(defun aics--make-code-fence (content)
  "Generate a code fence that's long enough to encapsulate CONTENT.
The fence will be one backtick longer than the longest sequence of backticks in CONTENT,
with a minimum of 3 backticks."
  (let ((max-backticks 0)
        (start 0))
    (while (string-match "`+" content start)
      (setq max-backticks (max max-backticks (- (match-end 0) (match-beginning 0))))
      (setq start (match-end 0)))
    (make-string (max 3 (1+ max-backticks)) ?`)))

(defun aics--make-fenced-code-block (content)
  "Wrap CONTENT in a fenced code block using appropriate fence length.
The fence is generated using `aics--make-code-fence' function."
  (let ((fence (aics--make-code-fence content)))
    (concat fence "\n" content "\n" fence)))

(defun aics--indent (content depth)
  "Indent CONTENT by DEPTH spaces at the start of each line.
Returns the indented content as a string."
  (let ((lines (split-string content "\n")))
    (mapconcat (lambda (line)
                 (concat (make-string depth ? ) line))
               lines
               "\n")))

(defun aics--fragment-before-cursor ()
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

(defun aics--fragment-after-cursor ()
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

(defun aics--project-buffers-info ()
  (if-let ((proj (project-current)))
      (let ((buffers (aics--project-buffers))
            info)
        (if buffers
            (progn
              (setq info "Other buffers in the same project:\n\n")
              (dolist (buf buffers)
                (with-current-buffer buf
                  (setq info (concat
                              info
                              (format "`%s`:  \n" (buffer-name buf))
                              (aics--buffer-info)
                              "\n\n"))))
              info)
          ""))
    ""))

(defun aics--project-buffers ()
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

(defun aics--process-suggestions-and-apply (input)
  "Parse the INPUT string for OP commands and apply the actions."
  (let ((parse-result (aics--parse-suggestions-ops input)))
    (if (eq (car parse-result) 'error)
        ;; If parsing fails, directly output the error message
        (message "Error parsing suggestions: %s" (cadr parse-result))
      ;; Successfully parsed, start executing operations
      (let ((ops parse-result))
        (message "Applying OPs: %s" ops)
        (catch 'error
          (dolist (op ops)
            (let ((op-type (alist-get :type op)))
              (condition-case err
                  ;; Call the corresponding helper function based on the operation type
                  (cond
                   ((string= op-type "MODIFY") (aics--apply-modify-op op))
                   ((string= op-type "CREATE") (aics--apply-create-op op))
                   ((string= op-type "DELETE") (aics--apply-delete-op op))
                   ((string= op-type "ELISP") (aics--apply-elisp-op op))
                   (t (error "Unknown operation type: %s" op-type)))
                ;; Catch errors during execution
                (error
                 (message "Error applying OP: %s" (error-message-string err))
                 (throw 'error nil)))))
          (message "All operations applied successfully."))))))

(defun aics--parse-suggestions-ops (suggestions)
  "Parse SUGGESTIONS string into a list of operations.
Returns ops list on success, or (error . message) on failure."
  (let ((lines (split-string suggestions "\n"))
        (ops nil)
        (parse-error nil)) ;; Track parsing errors

    (while (and lines (not parse-error))
      (let ((line (car lines)))
        (if (string-match "^\\*\\*OP\\*\\* \\(\\w+\\)\\(?: \\(.*\\)\\)?" line)
            ;; Process matched OP line
            (let ((op-type (match-string 1 line))
                  (filepath (match-string 2 line))
                  (next-lines (cdr lines)))
              (let ((op-parse-result
                     (pcase op-type
                       ("MODIFY" (aics--parse-modify-op filepath next-lines))
                       ("CREATE" (aics--parse-create-op filepath next-lines))
                       ("DELETE" (aics--make-delete-op filepath next-lines))
                       ("ELISP" (aics--parse-elisp-op next-lines))
                       (_ (list 'error (format "Unknown operation type: %s" op-type) lines)))))
                (if (eq (car op-parse-result) 'error)
                    (setq parse-error op-parse-result)
                  (push (car op-parse-result) ops)
                  ;; Update `lines` with remaining lines from `op-parse-result`
                  (setq lines (cdr op-parse-result)))))
          ;; Else: Skip non-OP lines
          (setq lines (cdr lines)))))

    (or parse-error (nreverse ops))))

(defun aics--parse-modify-op (filepath lines)
  "Parse a MODIFY operation from FILEPATH and LINES.
Returns (op-object . remain-lines) on success,
or (list 'error message remain-lines) on failure."
  (let ((result (aics--parse-search-replace-pairs lines)))
    (if (eq (car result) 'error)
        result ;; Directly return the error result
      (cons `((:type . MODIFY)
              (:filepath . ,filepath)
              (:pairs . ,(car result)))
            (cdr result)))))

(defun aics--parse-create-op (filepath lines)
  "Parse a CREATE operation from FILEPATH and LINES.
Returns (op-object . remain-lines) on success,
or (list 'error message remain-lines) on failure."
  (let ((result (aics--parse-code-block lines)))
    (if (eq (car result) 'error)
        result
      (cons `((:type . CREATE)
              (:filepath . ,filepath)
              (:content . ,(car result)))
            (cdr result)))))

(defun aics--make-delete-op (filepath lines)
  "Create a DELETE operation from FILEPATH.
Always succeeds, returning (op-object . remain-lines)."
  (cons `((:type . DELETE)
          (:filepath . ,filepath))
        lines))

(defun aics--parse-elisp-op (lines)
  "Parse an ELISP operation from LINES.
Returns (op-object . remain-lines) on success,
or (list 'error message remain-lines) on failure."
  (let ((result (aics--parse-code-block lines)))
    (if (eq (car result) 'error)
        (list 'error (cadr result) (caddr result))
      (cons `((:type . ELISP)
              (:content . ,(car result)))
            (cdr result)))))

(defun aics--parse-search-replace-pairs (lines)
  "Parse search/replace pairs from LINES.
Returns (pairs . remaining-lines) on success,
or (error . message) on failure."
  (let ((pairs nil)
        (parse-error nil)
        (parse-end nil)) ;; Loop control
    (while (and lines (not parse-error) (not parse-end))
      ;; Skip empty lines
      (while (and lines (string-empty-p (car lines)))
        (setq lines (cdr lines)))

      ;; If no more lines or current line is not *SEARCH*
      (if (or (null lines) (not (string= (car lines) "*SEARCH*")))
          (if (null pairs)
              (setq parse-error (list 'error  "No valid search/replace pairs found" lines))
            (setq parse-end t)) ;; Exit loop if pairs are not empty
        ;; Found *SEARCH*, parse the search block
        (let ((search-parse-result (aics--parse-code-block (cdr lines))))
          (if (eq (car search-parse-result) 'error)
              (setq parse-error search-parse-result)
            (let ((search-content (car search-parse-result))
                  (remain-lines (cdr search-parse-result)))
              ;; Skip empty lines before *REPLACE*
              (while (and remain-lines (string-empty-p (car remain-lines)))
                (setq remain-lines (cdr remain-lines)))

              ;; Check for *REPLACE* line
              (if (or (null remain-lines)
                      (not (string= (car remain-lines) "*REPLACE*")))
                  (setq parse-error (list 'error "Expected *REPLACE* after search block" remain-lines))
                (let ((replace-parse-result (aics--parse-code-block (cdr remain-lines))))
                  (if (eq (car replace-parse-result) 'error)
                      (setq parse-error replace-parse-result)
                    ;; Add parsed pair
                    (push (cons search-content (car replace-parse-result)) pairs)
                    ;; Update lines to after replace block
                    (setq lines (cdr replace-parse-result))))))))))

    ;; Return results or error
    (if parse-error
        parse-error
      (cons (nreverse pairs) lines))))

(defun aics--parse-code-block (lines)
  "Parse a fenced code block from LINES.
Returns (content . remaining-lines) on success,
or (error . message) on failure.
A fenced code block starts with a line of three or more backticks and ends with a matching line.
Empty lines before the start fence are ignored."
  (if (null lines)
      (list 'error "Empty input when expecting code block" lines)
    (let ((start-fence-regex "^\\(`\\{3,\\}\\)")) ; Simplified regex for fences
      ;; Skip leading empty lines
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      ;; Check for the start fence
      (if (null lines)
          (list 'error "Empty input after skipping empty lines" lines)
        (if (not (string-match start-fence-regex (car lines)))
            (list 'error "Expected code block start fence" lines)
          (let ((fence (match-string 1 (car lines))) ; Capture the exact backtick sequence
                (lines (cdr lines)) ; Skip the start fence
                (content '())
                found-end)
            ;; Collect content until the matching end fence is found
            (while (and lines (not found-end))
              (let ((line (car lines))) ; Localize current line
                (if (string= line fence) ; Match exact fence for the end
                    (setq found-end t)
                  (push line content)
                  (setq lines (cdr lines)))))
            ;; Check if the end fence was found
            (if (not found-end)
                (list 'error "Unclosed code block" lines)
              ;; Return parsed content and remaining lines
              (cons (string-join (nreverse content) "\n") (cdr lines)))))))))

(defun aics--apply-modify-op (op)
  "Apply a MODIFY operation from OP."
  (let ((file (alist-get :filepath op))
        (pairs (alist-get :pairs op)))
    (message "Applying MODIFY on %s" file)
    (unless (file-exists-p file)
      (error "File not found: %s" file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (dolist (pair pairs)
          (let ((search (car pair))
                (replace (cdr pair)))
            (message "Searching for: '%s' to replace with: '%s'" search replace)
            (unless (search-forward search nil t)
              (error "Search string '%s' not found in file: %s" search file))
            (replace-match replace t t))))
        (save-buffer))))

(defun aics--apply-create-op (op)
  "Apply a CREATE operation from OP."
  (let ((file (alist-get :filepath op))
        (content (alist-get :content op)))
    (message "Applying CREATE on %s" file)
    (when (file-exists-p file)
      (error "File already exists: %s" file))
    (with-temp-file file
      (insert content))))

(defun aics--apply-delete-op (op)
  "Apply a DELETE operation from OP."
  (let ((file (alist-get :filepath op)))
    (message "Applying DELETE on %s" file)
    (unless (file-exists-p file)
      (error "File not found: %s" file))
    (delete-file file)))

(defun aics--apply-elisp-op (op)
  "Apply an ELISP operation from OP."
  (let ((content (alist-get :content op)))
    (message "Evaluating ELISP: %s" content)
    (eval (read content))))

(provide 'aics)
;;; aics.el ends here
