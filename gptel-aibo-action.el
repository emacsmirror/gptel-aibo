;;; gptel-aibo-action.el --- Action for gptel-aibo -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
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

;; Actions parse and apply for gptel-aibo

;;; Code:

(require 'gptel-aibo-context)
(require 'text-property-search)
(require 'cl-lib)

(defun gptel-aibo-apply-last-suggestions ()
  "Parse and apply the last LLM response in current buffer.

This function searches backward from the end of buffer for the last
GPT response (marked with ''gptel ''response text property), extracts
its content, and applies any valid operations found in the response.

See `gptel-aibo--apply-suggestions' for implementation details."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if-let ((prop (text-property-search-backward 'gptel 'response t)))
        (let ((working-buffer
               (get-text-property (prop-match-beginning prop) 'gptai)))
          (cond
           ((not working-buffer)
            (message "The response has no working-buffer."))
           ((not (buffer-live-p working-buffer))
            (message "The response's working-buffer has been closed."))
           (t
            (let* ((begin (prop-match-beginning prop))
                   (end (prop-match-end prop))
                   (response (buffer-substring-no-properties begin end)))
              (with-current-buffer working-buffer
                (and
                 (eq (gptel-aibo--apply-suggestions response 'dry-run) t)
                 (gptel-aibo--apply-suggestions response)))))))
      (message "No response found."))))

(defvar gptel-aibo--delete-confirmation nil
  "Stores user confirmation preference for file deletion.")

(defun gptel-aibo--apply-suggestions (response &optional dry-run)
  "Parse the RESPONSE for OP commands and apply the actions.

If DRY-RUN is non-nil, simulate operations without making any actual changes.

Returns:
- t if all operations were applied successfully
- nil if an error occurred during parsing or execution
- :no-op if no operations were found"
  (setq gptel-aibo--delete-confirmation nil)
  (let ((parse-result (gptel-aibo--parse-suggestions response)))
    (cond
     ((eq (car parse-result) 'error)
      (message "Error parsing suggestions: %s" (cadr parse-result))
      nil)
     ((null parse-result)
      (message "No operations found in response")
      :no-op)
     (t
      (message "Applying OPs%s" (if dry-run "[dry-run]" ""))
      (and (cl-loop
            for op in parse-result
            always
            (condition-case err
                (progn
                  (gptel-aibo-execute op dry-run)
                  t)
              (error
               (message "Error applying OP: %s" (error-message-string err))
               nil)))
           (message "All operations applied%s successfully."
                    (if dry-run "[dry-run]" ""))
           t)))))

(defun gptel-aibo--parse-suggestions (response)
  "Parse RESPONSE string into a list of operations.
Returns ops list on success, or (error . message) on failure."
  (let ((lines (split-string response "\n"))
        (ops nil)
        (parse-error nil)) ;; Track parsing errors
    (while (and lines (not parse-error))
      (let ((line (car lines)))
        (cond
         ((string-match "^\\*\\*OP\\*\\*\\s-+\\(\\w+\\)\\(.*\\)$" line)
          ;; Process matched OP line
          (let* ((op (match-string 1 line))
                 (target (gptel-aibo--markdown-unbacktick
                          (string-trim (match-string 2 line))))
                 (next-lines (cdr lines))
                 (op-parse-result
                  (if-let ((op-parser (gptel-aibo--create-op-parser op)))
                      (gptel-aibo-parse-op op-parser target next-lines)
                    (list 'error
                          (format "Unknown operation type: %s" op)
                          lines))))
            (if (eq (car op-parse-result) 'error)
                (setq parse-error op-parse-result)
              (push (car op-parse-result) ops)
              ;; Update `lines` with remaining lines from `op-parse-result`
              (setq lines (cdr op-parse-result)))))
         (t
          (setq lines (cdr lines))))))
    (or parse-error (nreverse ops))))

(defun gptel-aibo--markdown-unbacktick (str)
  "Remove markdown code block backticks from STR."
  (if (and (string-match "^\\(`+\\)\\(.*\\)\\1$" str)
           (> (length (match-string 1 str)) 0))
      (match-string 2 str)
    str))

(defun gptel-aibo--create-op-parser (op)
  "Construct the appropriate parser for the given operation type OP.

Returns the corresponding parser, or nil if the operation type is unknown."
  (pcase op
    ("MODIFY" (gptel-aibo-make-mod-op-parser))
    ("CREATE" (gptel-aibo-make-creation-op-parser))
    ("DELETE" (gptel-aibo-make-del-op-parser))
    (_ nil)))

(cl-defgeneric gptel-aibo-parse-op (parser target lines)
  "Parse the operation using a specific PARSER instance, TARGET, and LINES.")

(cl-defstruct (gptel-aibo-op-parser (:constructor gptel-aibo-make-op-parser))
  "Base class for all gptel-aibo operation parsers.")

(cl-defstruct (gptel-aibo-mod-op-parser
               (:include gptel-aibo-op-parser)
               (:constructor gptel-aibo-make-mod-op-parser))
  "Parser for modification operations.")

(cl-defmethod gptel-aibo-parse-op
  ((_parser gptel-aibo-mod-op-parser) target lines)
  "Parse a modification operation from TARGET and LINES."
  (while (and lines (string-blank-p (car lines)))
    (setq lines (cdr lines)))
  (cond
   ((null lines)
    (list 'error "Empty input after skipping empty lines" lines))

   ;; While we’ve instructed the LLM to use search/replace pairs, it doesn’t
   ;; always follow faithfully.
   ((string-match "^\\(`\\{3,\\}\\)" (car lines))
    (let ((result (gptel-aibo--parse-code-block lines)))
      (if (eq (car result) 'error)
          result
        (cons (gptel-aibo-make-mod-op
               :target target
               :full-content (car result))
              (cdr result)))))

   (t
    (let ((result (gptel-aibo--parse-search-replace-pairs lines)))
      (if (eq (car result) 'error)
          result
        (cons (gptel-aibo-make-mod-op
               :target target
               :replacements (car result))
              (cdr result)))))))

(cl-defstruct (gptel-aibo-creation-op-parser
               (:include gptel-aibo-op-parser)
               (:constructor gptel-aibo-make-creation-op-parser))
  "Parser for creation operations.")

(cl-defmethod gptel-aibo-parse-op
  ((_parser gptel-aibo-creation-op-parser) target lines)
  "Parse a creation operation from TARGET and LINES."
  (let ((result (gptel-aibo--parse-code-block lines)))
    (if (eq (car result) 'error)
        result
      (cons (gptel-aibo-make-creation-op
             :filename target
             :content (car result))
            (cdr result)))))

(cl-defstruct (gptel-aibo-del-op-parser
               (:include gptel-aibo-op-parser)
               (:constructor gptel-aibo-make-del-op-parser))
  "Parser for deletion operations.")

(cl-defmethod gptel-aibo-parse-op
  ((_parser gptel-aibo-del-op-parser) target lines)
  "Parse a deletion operation from TARGET and LINES."
  (cons (gptel-aibo-make-del-op
         :filename target)
        lines))

(defun gptel-aibo--parse-search-replace-pairs (lines)
  "Parse search/replace pairs from LINES.
Returns (replacements . remaining-lines) on success,
or (error . message) on failure."
  (let ((replacements nil)
        (parse-error nil)
        (parse-end nil)) ;; Loop control
    (while (and lines (not parse-error) (not parse-end))
      ;; Skip empty lines
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))

      ;; If no more lines or current line is not *SEARCH*
      (if (or (null lines) (not (string= (car lines) "*SEARCH*")))
          (if (null replacements)
              (setq parse-error
                    (list 'error  "No valid search/replace pairs found" lines))
            (setq parse-end t)) ;; Exit loop if pairs are not empty
        ;; Found *SEARCH*, parse the search block
        (let ((search-parse-result (gptel-aibo--parse-code-block (cdr lines))))
          (if (eq (car search-parse-result) 'error)
              (setq parse-error search-parse-result)
            (let ((search-content (car search-parse-result))
                  (remain-lines (cdr search-parse-result)))
              ;; Skip empty lines before *REPLACE*
              (while (and remain-lines (string-blank-p (car remain-lines)))
                (setq remain-lines (cdr remain-lines)))

              ;; Check for *REPLACE* line
              (if (or (null remain-lines)
                      (not (string= (car remain-lines) "*REPLACE*")))
                  (setq parse-error
                        (list 'error "Expected *REPLACE* after search block"
                              remain-lines))
                (let ((replace-parse-result
                       (gptel-aibo--parse-code-block (cdr remain-lines))))
                  (if (eq (car replace-parse-result) 'error)
                      (setq parse-error replace-parse-result)
                    ;; Add parsed pair
                    (push (cons search-content (car replace-parse-result))
                          replacements)
                    ;; Update lines to after replace block
                    (setq lines (cdr replace-parse-result))))))))))

    ;; Return results or error
    (if parse-error
        parse-error
      (cons (nreverse replacements) lines))))

(defun gptel-aibo--parse-code-block (lines)
  "Parse a fenced code block from LINES.
Returns (content . remaining-lines) on success,
or (error . message) on failure."
  (if (null lines)
      (list 'error "Empty input when expecting code block" lines)
    (let ((start-fence-regex "^\\(`\\{3,\\}\\)"))
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      (if (null lines)
          (list 'error "Empty input after skipping empty lines" lines)
        (if (not (string-match start-fence-regex (car lines)))
            (list 'error "Expected code block start fence" lines)
          (let ((fence (match-string 1 (car lines)))
                (lines (cdr lines))
                (content '())
                found-end)
            (while (and lines (not found-end))
              (let ((line (car lines)))
                (if (string-match-p (concat "^" (regexp-quote fence) "\\s-*$")
                                    line)
                    (setq found-end t)
                  (push line content)
                  (setq lines (cdr lines)))))
            (if (not found-end)
                (list 'error "Unclosed code block" lines)
              (cons (string-join (nreverse content) "\n") (cdr lines)))))))))

(defun gptel-aibo--is-in-project (working-buffer buffer-or-filename)
  "Return t if BUFFER-OR-FILENAME is in the same project as WORKING-BUFFER.

- If BUFFER-OR-FILENAME is a buffer and equals WORKING-BUFFER, return t.
- If WORKING-BUFFER belongs to a project, return t if BUFFER-OR-FILENAME
  (either a buffer or a filename) is within that project's root.
- Otherwise, return nil."
  (cond
   ((and (bufferp buffer-or-filename) (eq working-buffer buffer-or-filename))
    t) ;; Case 1: Same buffer

   (t
    (when-let* ((working-dir
                 (buffer-local-value 'default-directory working-buffer))
                (project (project-current nil working-dir))
                (project-root-dir (gptel-aibo--project-root project))
                (file-path
                 (if (bufferp buffer-or-filename)
                     (buffer-local-value 'default-directory buffer-or-filename)
                   buffer-or-filename)))
      (file-in-directory-p file-path project-root-dir)))))

(cl-defgeneric gptel-aibo-execute (op &optional dry-run)
  "Execute an operation OP. If DRY-RUN is non-nil, simulate the operation.")

(cl-defstruct (gptel-aibo-op (:constructor gptel-aibo-make-op))
  "Base class for all gptel-aibo operations.")

(cl-defstruct (gptel-aibo-mod-op (:include gptel-aibo-op)
                                 (:constructor gptel-aibo-make-mod-op))
  "Represents a buffer modification operation."
  target
  replacements
  full-content)

(cl-defmethod gptel-aibo-execute ((op gptel-aibo-mod-op) &optional dry-run)
  "Execute a modification operation OP.
If DRY-RUN is non-nil, simulate the operation without making any changes."
  (let ((buffer-name (gptel-aibo-mod-op-target op))
        (replacements (gptel-aibo-mod-op-replacements op))
        (full-content (gptel-aibo-mod-op-full-content op)))
    (message "Applying MODIFY%s: %s" (if dry-run "[dry-run]" "") buffer-name)
    ;; We require LLM to use buffer name, but LLM doesn't always follow it.
    (let ((op-buffer (or (get-buffer buffer-name)
                         (get-file-buffer buffer-name))))
      (cond
       ((not op-buffer)
        (error "Buffer not found: %s" buffer-name))
       ((not (gptel-aibo--is-in-project (current-buffer) op-buffer))
        (error "Modifications outside the working project are not allowed: %s"
               buffer-name))
       (full-content
        (with-current-buffer op-buffer
          (erase-buffer)
          (insert full-content)
          (when (buffer-file-name)
            (save-buffer))))
       (t
        (with-current-buffer op-buffer
          (goto-char (point-min))
          (dolist (search-replace-pair replacements)
            (goto-char (point-min))
            (let ((search (car search-replace-pair))
                  (replace (cdr search-replace-pair)))
              (unless (search-forward search nil t)
                (error "Searching fail: [%s]" search))
              (unless dry-run
                (replace-match replace t t))))
          (when (buffer-file-name)
            (save-buffer))))))))

(cl-defstruct (gptel-aibo-creation-op (:include gptel-aibo-op)
                                      (:constructor gptel-aibo-make-creation-op))
  "Represents a file creation operation."
  filename
  content)

(cl-defmethod gptel-aibo-execute ((op gptel-aibo-creation-op) &optional dry-run)
  "Execute a creation operation OP.
If DRY-RUN is non-nil, simulate the operation without creating the file."
  (let ((filename (gptel-aibo-creation-op-filename op))
        (content (gptel-aibo-creation-op-content op)))
    (message "Applying CREATE%s: %s" (if dry-run "[dry-run]" "") filename)
    (unless (gptel-aibo--is-in-project (current-buffer) filename)
      (error "Creating file outside the working project is not allowed: %s"
             filename))
    (when (file-exists-p filename)
      (error "File already exists: %s" filename))
    (unless dry-run
      (with-current-buffer (create-file-buffer filename)
        (insert content)
        (set-visited-file-name filename)
        (save-buffer)))))

(cl-defstruct (gptel-aibo-del-op (:include gptel-aibo-op)
                                 (:constructor gptel-aibo-make-del-op))
  "Represents a file deletion operation."
  filename)

(cl-defmethod gptel-aibo-execute ((op gptel-aibo-del-op) &optional dry-run)
  "Execute a deletion operation OP.
If DRY-RUN is non-nil, simulate deletion without actually removing the file."
  (let ((filename (gptel-aibo-del-op-filename op)))
    (message "Applying DELETE%s: %s" (if dry-run "[dry-run]" "") filename)
    (unless (gptel-aibo--is-in-project (current-buffer) filename)
      (error "Deleting files outside the working project is not allowed: %s"
             filename))
    (unless (file-exists-p filename)
      (error "File not found: %s" filename))
    (unless dry-run
      (when-let ((file-buffer (get-file-buffer filename)))
        (kill-buffer file-buffer)))
    (unless dry-run
      (cond
       ((eq gptel-aibo--delete-confirmation 'never)
        (message "File deletion refused by user: %s" filename))
       ((eq gptel-aibo--delete-confirmation 'always)
        (delete-file filename))
       (t
        (let ((response
               (read-char-choice
                (format "Delete file %s? (y)es/(n)o/(a)lways/(N)ever: " filename)
                '(?y ?n ?a ?N))))
          (pcase response
            (?y (delete-file filename))
            (?n (message "File deletion refused by user: %s" filename))
            (?a (setq gptel-aibo--delete-confirmation 'always)
                (delete-file filename))
            (?N (setq gptel-aibo--delete-confirmation 'never)
                (message "File deletion refused by user: %s" filename)))))))))

(provide 'gptel-aibo-action)
;;; gptel-aibo-action.el ends here
