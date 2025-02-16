;;; gptel-aibo-planner.el --- Action executor -*- lexical-binding: t; -*-
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

;; Action executor for gptel-aibo

;;; Code:

(declare-function gptel-aibo-make-action-org-parser "gptel-aibo-action-org-parser")
(declare-function gptel-aibo-make-action-parser "gptel-aibo-action-parser")

(require 'gptel-aibo-action)

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
              (and
               (eq (gptel-aibo--apply-suggestions response 'dry-run) t)
               (gptel-aibo--apply-suggestions response))))))
      (message "No response found."))))

(defun gptel-aibo--apply-suggestions (response &optional dry-run)
  "Parse the RESPONSE for OP commands and apply the actions.

If DRY-RUN is non-nil, simulate operations without making any actual changes.

Returns:
- t if all operations were applied successfully
- nil if an error occurred during parsing or execution
- :no-op if no operations were found"
  (setq gptel-aibo--delete-confirmation nil)
  (let* ((parser (cond
                  ((derived-mode-p 'org-mode)
                   (require 'gptel-aibo-action-org-parser)
                   (gptel-aibo-make-action-org-parser))
                  (t
                   (require 'gptel-aibo-action-parser)
                   (gptel-aibo-make-action-parser))))
         (parse-result (gptel-aibo-parse-action parser response)))
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

(provide 'gptel-aibo-planner)
;;; gptel-aibo-planner.el ends here
