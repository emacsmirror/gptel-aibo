;;; aics.el --- An AI Coding Assistant for Emacs -*- lexical-binding: t; -*-
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
;;  AICS is an AI-powered coding assistant that helps users create and manage content
;;  in Emacs, including programs, documents, papers, and novels. It provides various
;;  operations like buffer modifications, file creation/deletion, and Elisp execution.
;;
;;; Code:

(require 'gptel)

(defvar aics-system-message
  "You are a highly capable assistant helping users work with Emacs to create \
and manage content, including programs, documents, papers, and novels.
Based on the user's request, you can generate one of the following actions:
* Modify buffers
* Create files
* Delete files
* Execute a piece of Elisp code

If multiple actions are required, they should be provided in sequence, and the \
program will execute them in the order they are listed.

The specific format descriptions for these actions are as follows:

## Modify buffers
Start with line:
```
**OP** MODIFY <FILEPATH>
```
`<FILEPATH>` refers to the filepath associated with the buffer. If the buffer is \
not linked to any file, leave it empty.

Following the start line, add one or more blank lines, then specify the
SEARCH/REPLACE pairs.

Each pair is structured as follows:

Begin with the exact line:
```
*SEARCH*
```
Followed by the content to locate, enclosed in a markdown fenced code block.

Then the exact line:
```
*REPLACE*
```
Followed by the replacement content, enclosed in a markdown fenced code block.

**NOTE**
1. MODIFY operations can only be generated when the complete content of the \
target buffer is known
2. MODIFY operations must consist of only SEARCH/REPLACE pairs, with no \
additional content or full file replacements

### Rules for SEARCH/REPLACE:
* Always include both `*SEARCH*` and `*REPLACE*` lines for every pair. \
Missing either line is invalid
* Operates on a line-by-line basis
* Use the smallest number of consecutive lines necessary to uniquely identify \
the target
* Only the first match is processed for each SEARCH/REPLACE pair
* Modifications to consecutive lines should be handled together within one \
SEARCH/REPLACE pair whenever possible
* Multiple SEARCH/REPLACE pairs can be used sequentially, each subsequent pair \
operates on the result of the previous ones
* Ensure that the code block fence is sufficiently long to correctly \
encapsulate SEARCH/REPLACE content that contains backtick sequences, \
preventing any Markdown parsing issues

### Example
Content of the current buffer:
````
hi world
hi there
...
...
...
hi world
```hi world
````
**Task**: Replace all occurrences of `hi` with `hello`.

Generated MODIFY operation:

**OP** MODIFY

*SEARCH*
```
hi world
hi there
```
*REPLACE*
```
hello world
hello there
```
*SEARCH*
````
hi world
```hi world
````
*REPLACE*
````
hello world
```hello world
````
Explanation：
1. Consecutive lines are included within a single SEARCH/REPLACE pair.
2. Since the SEARCH/REPLACE content contains backtick sequences, \
a longer code block fence is used to ensure proper encapsulation \
and avoid Markdown parsing issues.

## Create files
Start with line:
```
**OP** CREATE <FILEPATH>
```
`<FILEPATH>` is the path of the file to be created and is required.

Followed by the content, enclosed in a markdown fenced code block.

## Delete files
Just one line:
```
**OP** DELETE <FILEPATH>
```
`<FILEPATH>` is the path of the file to be deleted and is required.

## Execute a piece of Elisp code
Start with line:
```
**OP** ELISP
```
Followed by the elisp code snippet, enclosed in a markdown fenced code block.
### Example
**OP** ELISP
```elisp
(insert \"Hello, Emacs!\")
```
")

(defvar aics-system-message-for-complete
  "You are a highly capable assistant helping users work with Emacs to create \
and manage content, including programs, documents, papers, and novels.")

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
(defun aics-request (request)
  (let ((gptel--system-message aics-system-message)
        (prompt (concat (aics--context-info)
                        "\nUser Request:\n"
                        request)))
    (gptel-request prompt
      :callback
      (lambda (response info)
        (if (stringp response)
            (let ((current-buffer (plist-get info :buffer)))
              (with-current-buffer current-buffer
                (aics--process-suggestions-and-apply response)))
          (message "gptel-request failed with message: %s"
                   (plist-get info :status)))))))

;;;###autoload
(defun aics-complete-at-point ()
  (interactive)
  (let ((gptel--system-message aics-system-message-for-complete)
        (prompt (concat (aics--context-info) aics-complete-message)))
    (message prompt)
    (gptel-request prompt)))

(defun aics--context-info ()
  (concat (aics--project-directory-info)
          "\n"
          (aics--current-buffer-info)
          "\n"
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
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
    (concat (aics--current-buffer-filepath-info)
            (format "Content of the current buffer:\n%s\n"
                    (aics--make-fenced-code-block buffer-content))
            (cond ((= (point) (point-min))
                   "Current cursor is at the beginning of the buffer.\n")
                  ((= (point) (point-max))
                   "Current cursor is at the end of the buffer.\n")
                  (t
                   (let ((point-prefix-content (aics--point-prefix-content))
                         (point-suffix-content (aics--point-suffix-content)))
                     (format "Content fragment before the cursor:\n%s\nContent fragment after the cursor:\n%s\n"
                             (aics--make-fenced-code-block point-prefix-content)
                             (aics--make-fenced-code-block point-suffix-content))))))))

(defun aics--current-buffer-filepath-info ()
  (if buffer-file-name
      (format "Current buffer's filepath: %s\n\n" buffer-file-name)
    ""))

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

(defun aics--point-prefix-content ()
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

(defun aics--point-suffix-content ()
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
  "Return formatted information about other buffers in the same project.
Output format:
The other buffers of this project:

filepath: xxx
content:
```
<content>
```

Multiple buffers will be listed sequentially."
  (if-let ((proj (project-current)))
      (let ((buffers (aics--project-buffers))
            info)
        (if buffers
            (progn
              (setq info "The other buffers of this project:\n\n")
              (dolist (buf buffers)
                (with-current-buffer buf
                  (setq info (concat info
                                     "- filepath: " (or buffer-file-name "(no file)") "\n\n"
                                     "  content:\n"
                                     (aics--indent
                                      (aics--make-fenced-code-block
                                       (buffer-substring-no-properties (point-min) (point-max)))
                                      2)
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
