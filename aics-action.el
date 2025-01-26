;;; aics-action.el --- Action handling for AICS -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity
;;
;;; Commentary:
;;
;; Actions parse and apply for AICS
;;
;;; Code:

(require 'text-property-search)

(defun aics--parse-last-suggestions-and-apply ()
  (interactive)
  (goto-char (point-max))
  (if-let ((prop (text-property-search-backward 'gptel 'response t)))
      (let* ((begin(prop-match-beginning prop))
             (end (prop-match-end prop))
             (response (buffer-substring-no-properties begin end)))
        (aics--parse-suggestions-and-apply response))
    (message "No response found.")))

(defvar aics--delete-confirmation nil
  "Stores user confirmation preference for file deletion.")

(defun aics--parse-suggestions-and-apply (response)
  "Parse the RESPONSE string for OP commands and apply the actions."
  ;; Reset deletion confirmation state for this batch
  (setq aics--delete-confirmation nil)
  (let ((parse-result (aics--parse-suggestions-ops response)))
    (cond
     ((eq (car parse-result) 'error)
      (message "Error parsing suggestions: %s" (cadr parse-result)))
     ((null parse-result)
      (message "No operations found in response"))
     (t
      (let ((ops parse-result))
        (message "Applying OPs")
        (catch 'error
          (dolist (op ops)
            (let ((op-type (alist-get :type op)))
              (condition-case err
                  ;; Call the corresponding helper function based on the operation type
                  (cond
                   ((eq op-type 'MODIFY) (aics--apply-modify-op op))
                   ((eq op-type 'CREATE) (aics--apply-create-op op))
                   ((eq op-type 'DELETE) (aics--apply-delete-op op))
                   ((eq op-type 'ELISP) (aics--apply-elisp-op op))
                   (t (error "Unknown operation type: %s" op-type)))
                ;; Catch errors during execution
                (error
                 (message "Error applying OP: %s" (error-message-string err))
                 nil))))
          (message "All operations applied successfully.")))))))

(defun aics--parse-suggestions-ops (response)
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
                   (target (aics--markdown-unbacktick (match-string 2 line)))
                   (next-lines (cdr lines))
                   (op-parse-result
                    (pcase op
                      ("MODIFY" (aics--parse-modify-op target next-lines))
                      ("CREATE" (aics--parse-create-op target next-lines))
                      ("DELETE" (aics--make-delete-op target next-lines))
                      ("ELISP" (aics--parse-elisp-op next-lines))
                      (_ (list 'error (format "Unknown operation type: %s" op) lines)))))
              (if (eq (car op-parse-result) 'error)
                  (setq parse-error op-parse-result)
                (push (car op-parse-result) ops)
                ;; Update `lines` with remaining lines from `op-parse-result`
                (setq lines (cdr op-parse-result))))
          (setq lines (cdr lines)))))
    (or parse-error (nreverse ops))))

(defun aics--markdown-unbacktick (str)
  (if (and (string-match "^\\(`+\\)\\(.*\\)\\1$" str)
           (> (length (match-string 1 str)) 0))
      (match-string 2 str)
    str))

(defun aics--parse-modify-op (target lines)
  "Parse a MODIFY operation from TARGET and LINES.
Returns (op-object . remain-lines) on success,
or (list 'error message remain-lines) on failure."
  (let ((result (aics--parse-search-replace-pairs lines)))
    (if (eq (car result) 'error)
        result ;; Directly return the error result
      (cons `((:type . MODIFY)
              (:target . ,target)
              (:replacements . ,(car result)))
            (cdr result)))))

(defun aics--parse-create-op (target lines)
  "Parse a CREATE operation from TARGET and LINES.
Returns (op-object . remain-lines) on success,
or (list 'error message remain-lines) on failure."
  (let ((result (aics--parse-code-block lines)))
    (if (eq (car result) 'error)
        result
      (cons `((:type . CREATE)
              (:target . ,target)
              (:content . ,(car result)))
            (cdr result)))))

(defun aics--make-delete-op (target lines)
  "Create a DELETE operation from TARGET.
Always succeeds, returning (op-object . remain-lines)."
  (cons `((:type . DELETE)
          (:target . ,target))
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
Returns (replacements . remaining-lines) on success,
or (error . message) on failure."
  (let ((replacements nil)
        (parse-error nil)
        (parse-end nil)) ;; Loop control
    (while (and lines (not parse-error) (not parse-end))
      ;; Skip empty lines
      (while (and lines (string-empty-p (car lines)))
        (setq lines (cdr lines)))

      ;; If no more lines or current line is not *SEARCH*
      (if (or (null lines) (not (string= (car lines) "*SEARCH*")))
          (if (null replacements)
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
                    (push (cons search-content (car replace-parse-result)) replacements)
                    ;; Update lines to after replace block
                    (setq lines (cdr replace-parse-result))))))))))

    ;; Return results or error
    (if parse-error
        parse-error
      (cons (nreverse replacements) lines))))

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
  (let ((buffer-name (alist-get :target op))
        (replacements (alist-get :replacements op)))
    (message "Applying MODIFY on buffer: %s" buffer-name)
    (with-current-buffer (or (get-buffer buffer-name)
                             (error "Buffer not found: %s" buffer-name))
      (goto-char (point-min))
      (dolist (search-replace-pair replacements)
        (goto-char (point-min))
        (let ((search (car search-replace-pair))
              (replace (cdr search-replace-pair)))
          (unless (search-forward search nil t)
            (error "Searching fail: [%s]" search))
          (replace-match replace t t)))
      (when (buffer-file-name)
        (save-buffer)))))

(defun aics--apply-create-op (op)
  "Apply a CREATE operation from OP.
Creates a new buffer with the specified content and immediately saves it to file,
without switching to or closing the buffer."
  (let ((file (alist-get :target op))
        (content (alist-get :content op)))
    (message "Creating and saving file: %s" file)
    (when (file-exists-p file)
      (error "File already exists: %s" file))
    (with-current-buffer (find-file-noselect file)
      (insert content)
      (save-buffer))))

(defun aics--apply-delete-op (op)
  "Apply a DELETE operation from OP with user confirmation."
  (let ((file (alist-get :target op)))
    (message "Applying DELETE on %s" file)
    (unless (file-exists-p file)
      (error "File not found: %s" file))

    (if-let ((file-buffer (get-file-buffer file)))
        (kill-buffer file-buffer))

    (cond
     ((eq aics--delete-confirmation 'never)
      (message "File deletion refused by user: %s" file))
     ((eq aics--delete-confirmation 'always)
      (delete-file file))
     (t
      (let ((response (read-char-choice
                       (format "Delete file %s? (y)es/(n)o/(a)lways/(N)ever: " file)
                       '(?y ?n ?a ?N))))
        (pcase response
          (?y (delete-file file))
          (?n (message "File deletion refused by user: %s" file))
          (?a (setq aics--delete-confirmation 'always)
              (delete-file file))
          (?N (setq aics--delete-confirmation 'never)
              (message "File deletion refused by user: %s" file))))))))

(defun aics--apply-elisp-op (op)
  "Apply an ELISP operation from OP."
  (let ((content (alist-get :content op)))
    (message "Evaluating ELISP(SKIPPED): %s" content)
    ;;(eval (read content))
    ))

(provide 'aics-action)
;;; aics-action.el ends here
