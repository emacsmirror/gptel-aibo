;;; gptai-action.el --- Action for gptel-aibo -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity
;;
;;; Commentary:
;;
;; Actions parse and apply for gptel-aibo
;;
;;; Code:

(require 'gptai-context)
(require 'text-property-search)
(require 'cl-lib)

(defun gptai-apply-last-suggestions ()
  "Parse and apply the last LLM response in current buffer.

This function searches backward from the end of buffer for the last
GPT response (marked with ''gptel ''response text property), extracts
its content, and applies any valid operations found in the response.

See `gptai--apply-suggestions' for implementation details."
  (interactive)
  (goto-char (point-max))
  (if-let ((prop (text-property-search-backward 'gptel 'response t)))
      (let ((working-buffer (get-text-property (prop-match-beginning prop)
                                               'gptai--working-buffer)))
        (cond
         ((not working-buffer)
          (message "The response has no gptai--working-buffer."))
         ((not (buffer-live-p working-buffer))
          (message "The response's working-buffer has been closed."))
         (t
          (let* ((begin (prop-match-beginning prop))
                 (end (prop-match-end prop))
                 (response (buffer-substring-no-properties begin end)))
            (with-current-buffer working-buffer
              (and
               (eq (gptai--apply-suggestions response 'dry-run) t)
               (gptai--apply-suggestions response)))))))
    (message "No response found.")))

(defvar gptai--delete-confirmation nil
  "Stores user confirmation preference for file deletion.")

(defun gptai--apply-suggestions (response &optional dry-run)
  "Parse the RESPONSE for OP commands and apply the actions.

If DRY-RUN is non-nil, simulate operations without making any actual changes.

Returns:
- t if all operations were applied successfully
- nil if an error occurred during parsing or execution
- :no-op if no operations were found"
  (setq gptai--delete-confirmation nil)
  (let ((parse-result (gptai--parse-suggestions response)))
    (cond
     ((eq (car parse-result) 'error)
      (message "Error parsing suggestions: %s" (cadr parse-result))
      nil)
     ((null parse-result)
      (message "No operations found in response")
      :no-op)
     (t
      (message "Applying OPs%s" (if dry-run "[dry-run]" ""))
      (and
       (cl-loop
        for op in parse-result
        always
        (let ((op-type (alist-get :type op)))
          (condition-case err
              (progn
                (pcase op-type
                  ('MODIFY (gptai--apply-modify-op op dry-run))
                  ('CREATE (gptai--apply-create-op op dry-run))
                  ('DELETE (gptai--apply-delete-op op dry-run)))
                t)
            (error
             (message "Error applying OP: %s" (error-message-string err))
             nil))))
       (message "All operations applied%s successfully."
                (if dry-run "[dry-run]" ""))
       t)))))

(defun gptai--parse-suggestions (response)
  "Parse RESPONSE string into a list of operations.
Returns ops list on success, or (error . message) on failure."
  (let ((lines (split-string response "\n"))
        (ops nil)
        (parse-error nil)) ;; Track parsing errors

    (while (and lines (not parse-error))
      (let ((line (car lines)))
        (if (string-match "^\\*\\*OP\\*\\* \\(\\w+\\)\\(?: \\(.*\\)\\)?" line)
            ;; Process matched OP line
            (let* ((op (match-string 1 line))
                   (target (gptai--markdown-unbacktick (match-string 2 line)))
                   (next-lines (cdr lines))
                   (op-parse-result
                    (pcase op
                      ("MODIFY" (gptai--parse-modify-op target next-lines))
                      ("CREATE" (gptai--parse-create-op target next-lines))
                      ("DELETE" (gptai--make-delete-op target next-lines))
                      (_ (list 'error (format "Unknown operation type: %s" op)
                               lines)))))
              (if (eq (car op-parse-result) 'error)
                  (setq parse-error op-parse-result)
                (push (car op-parse-result) ops)
                ;; Update `lines` with remaining lines from `op-parse-result`
                (setq lines (cdr op-parse-result))))
          (setq lines (cdr lines)))))
    (or parse-error (nreverse ops))))

(defun gptai--markdown-unbacktick (str)
  "Remove markdown code block backticks from STR."
  (if (and (string-match "^\\(`+\\)\\(.*\\)\\1$" str)
           (> (length (match-string 1 str)) 0))
      (match-string 2 str)
    str))

(defun gptai--parse-modify-op (target lines)
  "Parse a MODIFY operation from TARGET and LINES.
Returns (op-object . remain-lines) on success,
or (list ''error message remain-lines) on failure."
  (while (and lines (string-blank-p (car lines)))
    (setq lines (cdr lines)))
  (cond
   ((null lines)
    (list 'error "Empty input after skipping empty lines" lines))

   ((string-match "^\\(`\\{3,\\}\\)" (car lines))
    (let ((result (gptai--parse-code-block lines)))
      (if (eq (car result) 'error)
          result
        (cons `((:type . MODIFY)
                (:target . ,target)
                (:full-content . ,(car result)))
              (cdr result)))))

   (t
    (let ((result (gptai--parse-search-replace-pairs lines)))
      (if (eq (car result) 'error)
          result
        (cons `((:type . MODIFY)
                (:target . ,target)
                (:replacements . ,(car result)))
              (cdr result)))))))

(defun gptai--parse-create-op (filename lines)
  "Parse a CREATE operation from FILENAME and LINES.
Returns (op-object . remain-lines) on success,
or (list ''error message remain-lines) on failure."
  (let ((result (gptai--parse-code-block lines)))
    (if (eq (car result) 'error)
        result
      (cons `((:type . CREATE)
              (:filename . ,filename)
              (:content . ,(car result)))
            (cdr result)))))

(defun gptai--make-delete-op (filename lines)
  "Create a DELETE operation from FILENAME and LINES.
Always succeeds, returning (op-object . remain-lines)."
  (cons `((:type . DELETE)
          (:filename . ,filename))
        lines))

(defun gptai--parse-search-replace-pairs (lines)
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
        (let ((search-parse-result (gptai--parse-code-block (cdr lines))))
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
                       (gptai--parse-code-block (cdr remain-lines))))
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

(defun gptai--parse-code-block (lines)
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
                (if (string= line fence)
                    (setq found-end t)
                  (push line content)
                  (setq lines (cdr lines)))))
            (if (not found-end)
                (list 'error "Unclosed code block" lines)
              (cons (string-join (nreverse content) "\n") (cdr lines)))))))))

(defun gptai--is-in-project (working-buffer buffer-or-filename)
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
                (project-root (gptai-context--project-root project))
                (file-path (if (bufferp buffer-or-filename)
                               (buffer-file-name buffer-or-filename)
                             buffer-or-filename)))
      (file-in-directory-p file-path project-root)))))

(defun gptai--apply-modify-op (op &optional dry-run)
  "Apply a modify operation from OP.

If DRY-RUN is non-nil, only search the modification without applying it."
  (let ((buffer-name (alist-get :target op))
        (replacements (alist-get :replacements op)))
    (message "Applying MODIFY%s: %s" (if dry-run "[dry-run]" "") buffer-name)
    ;; We require LLM to use buffer name, but LLM doesn't always follow it.
    (let ((op-buffer (or (get-buffer buffer-name)
                         (get-file-buffer buffer-name))))
      (cond
       ((not op-buffer)
        (error "Buffer not found: %s" buffer-name))
       ((not (gptai--is-in-project (current-buffer) op-buffer))
        (error "Modifications outside the working project are not allowed: %s"
               buffer-name))

       ((alist-get :full-content op)
        (with-current-buffer op-buffer
          (erase-buffer)
          (insert (alist-get :full-content op))))

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

(defun gptai--apply-create-op (op &optional dry-run)
  "Apply a create operation from OP.

If DRY-RUN is non-nil, simulate the operation without modifying anything."
  (let ((filename (alist-get :filename op))
        (content (alist-get :content op)))
    (message "Applying CREATE%s: %s" (if dry-run "[dry-run]" "") filename)
    (unless (gptai--is-in-project (current-buffer) filename)
      (error "Creating file outside the working project is not allowed: %s"
             filename))
    (when (file-exists-p filename)
      (error "File already exists: %s" filename))

    (unless dry-run
      (with-current-buffer (create-file-buffer filename)
        (insert content)
        (set-visited-file-name filename)
        (save-buffer)))))

(defun gptai--apply-delete-op (op &optional dry-run)
  "Apply a delete operation from OP with user confirmation.

If DRY-RUN is non-nil, simulate deletion without making changes."
  (let ((filename (alist-get :filename op)))
    (message "Applying DELETE%s: %s" (if dry-run "[dry-run]" "") filename)
    (unless (gptai--is-in-project (current-buffer) filename)
      (error "Deleting files outside the working project is not allowed: %s"
             filename))
    (unless (file-exists-p filename)
      (error "File not found: %s" filename))

    (unless dry-run
      (when-let ((file-buffer (get-file-buffer filename)))
        (kill-buffer file-buffer)))

    (unless dry-run
      (cond
       ((eq gptai--delete-confirmation 'never)
        (message "File deletion refused by user: %s" filename))
       ((eq gptai--delete-confirmation 'always)
        (delete-file filename))
       (t
        (let ((response
               (read-char-choice
                (format "Delete file %s? (y)es/(n)o/(a)lways/(N)ever: " filename)
                '(?y ?n ?a ?N))))
          (pcase response
            (?y (delete-file filename))
            (?n (message "File deletion refused by user: %s" filename))
            (?a (setq gptai--delete-confirmation 'always)
                (delete-file filename))
            (?N (setq gptai--delete-confirmation 'never)
                (message "File deletion refused by user: %s" filename)))))))))

(provide 'gptai-action)
;;; gptai-action.el ends here
