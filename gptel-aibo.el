;;; gptel-aibo.el --- An AI Writing Assistant -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Maintainer: Sun Yi Ming <dolmens@gmail.com>
;; Created: January 09, 2025
;; Modified: January 09, 2025
;; Version: 0.0.1
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity
;; Homepage: https://github.com/dolmens/gptel-aibo
;; Package-Requires: ((emacs "27.1") (gptel "20250211"))

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

;;  gptel-aibo is an AI-powered writing assistant that helps users create and
;;  manage content in Emacs, including programs, documents, papers, and novels.
;;  It provides various operations like buffer modifications, file creation and
;;  deletion.

;;; Code:

(defvar gptel-aibo--system-role
  "You are an expert assistant specializing in helping users with Emacs for
creating and managing various types of content, including code, documents,
and even novels.")

(defvar gptel-aibo--system-message
  (concat
   gptel-aibo--system-role
   "\n"
   "
Based on the user's request, you can generate one or more of the following actions:
* Modify buffers
* Create files
* Delete files

If multiple actions are required, they should be provided in the order in which
they should be executed.

### Action Formatting Rules

#### Modify buffers
Start with the line:

**OP** MODIFY `<NAME>`

`<NAME>` is the name of the buffer being modified, enclosed in backticks.

Next, leave one blank line, then specify the SEARCH/REPLACE pairs. Each pair is
structured as follows:

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
1. Ensure there is **one blank line** between the starting line
`**OP** MODIFY ...` and the SEARCH/REPLACE pairs.
2. Each SEARCH/REPLACE pair must match the structure shown, with no extra
content before or after.
3. Consecutive lines that are part of the same modification should be included
within a single SEARCH/REPLACE pair.
4. Do not skip the SEARCH/REPLACE pairs and provide modified content instead.

#### Create files
Start with the line:

**OP** CREATE `<FILEPATH>`

`<FILEPATH>` is the path of the file to be created and must be provided.
Then, include the file content, enclosed in a markdown fenced code block.

#### Delete files
Use a single-line command:

**OP** DELETE `<FILEPATH>`

`<FILEPATH>` is the path of the file to be deleted.

---

### Handling Code Block Fences

When generating code blocks, use a fence long enough for proper parsing. If the
content contains a sequence of three backticks, use four for the fence. If it
contains four backticks, use five, and so on for longer sequences.

### Additional Notes

You are free to add thoughts, reasoning, comments, or other relevant information
before, between, or after the operations as needed, but never start a line of
such content with `**OP**`, as it may be misinterpreted as an operation, or
insert descriptive material inside an operation, as it may disrupt the parsing.
"))

(defvar gptel-aibo--complete-message
  "
Your task:
Suggest content suitable for insertion at the cursor position.

Requirements:
1. Directly return the content to be inserted. Do not use code blocks, quotes,
or any other form of wrapping, enclosing, or additional formatting.
2. Do not include any explanations, comments, or extra information.
3. Ensure that the suggested content is consistent with the tone and style of
the surrounding text.
4. Do not call tools or ask questions to obtain additional information. If no
suitable content can be suggested, return an empty string.")

(defvar-local gptel-aibo--old-directives nil)

(defvar-local gptel-aibo--old-system-message nil)

(defvar-local gptel-aibo--old-use-context nil)

(defvar-local gptel-aibo--old-context-wrap-function nil)

(defvar-local gptel-aibo--from-gptel-mode nil
  "If this from `gptel-mode'.")

(require 'gptel)
(require 'gptel-context)
(require 'gptel-aibo-context)
(require 'gptel-aibo-planner)

(defvar gptel-aibo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-aibo-send)
    (define-key map (kbd "C-c !") #'gptel-aibo-apply-last-suggestions)
    map)
  "Keymap for `gptel-aibo-mode'.")

;;;###autoload
(define-minor-mode gptel-aibo-mode
  "Minor mode for gptel-aibo interacting with LLMs."
  :lighter " GPTel-Aibo"
  :keymap gptel-aibo-mode-map
  (if gptel-aibo-mode
      (progn
        (if gptel-mode
            (setq gptel-aibo--from-gptel-mode gptel-mode)
          (gptel-mode 1))
        (setq gptel-aibo--old-directives gptel-directives)
        (setq-local gptel-directives (cons `(Aibo . ,gptel-aibo--system-message)
                                           gptel-directives))
        (setq gptel-aibo--old-system-message gptel--system-message)
        (setq-local gptel--system-message gptel-aibo--system-message)
        (setq gptel-aibo--old-use-context gptel-use-context)
        (setq-local gptel-use-context 'user)
        (setq gptel-aibo--old-context-wrap-function gptel-context-wrap-function)
        (setq-local gptel-context-wrap-function #'gptel-aibo-context-wrap)
        (when gptel-use-header-line
          (setq header-line-format
                (cons '(:eval (concat
                               (propertize " " 'display '(space :align-to 0))
                               "<"
                               (buffer-name gptel-aibo--working-buffer)
                               ">"))
                      (cdr header-line-format))))
        (add-hook 'window-selection-change-functions
                  #'gptel-aibo--window-selection-change nil t)
        (message "gptel-aibo-mode enabled"))
    (remove-hook 'window-selection-change-functions
                 #'gptel-aibo--window-selection-change)
    (setq-local gptel-directives gptel-aibo--old-directives)
    (setq-local gptel--system-message gptel-aibo--old-system-message)
    (setq-local gptel-use-context gptel-aibo--old-use-context)
    (setq-local gptel-context-wrap-function gptel-aibo--old-context-wrap-function)
    (unless gptel-aibo--from-gptel-mode
      (gptel-mode -1))
    (message "gptel-aibo-mode disabled")))

(defun gptel-aibo-context-wrap (message contexts)
  "Wrap MESSAGE with CONTEXTS for gptel."
  (let ((context-string
         (concat "---

Request context:

**Note**: This context reflects the user's **most recent** working state. If
there is a conflict with inferred content, the context takes precedence, as
previous suggested actions may not have been executed and the user may have made
changes outside this conversation.

"
                 (gptel-aibo-context-info gptel-aibo--working-buffer)
                 (gptel-context--string contexts))))
    ;; (message "context: %s" context-string)
    (if (> (length context-string) 0)
        (pcase-exhaustive gptel-use-context
          ('system (concat message "\n\n" context-string))
          ('user   (concat message "\n\n" context-string))
          ('nil    message))
      message)))

(defun gptel-aibo--window-selection-change (window)
  "Handle window selection change to update working buffer and project.

WINDOW is the newly selected window."
  (when (eq window (selected-window))
    (if gptel-aibo--trigger-buffer
        (progn
          (unless (eq gptel-aibo--working-buffer gptel-aibo--trigger-buffer)
            (setq gptel-aibo--working-buffer gptel-aibo--trigger-buffer))
          (setq gptel-aibo--trigger-buffer nil))
      (let ((working-buffer-cand (window-buffer (old-selected-window))))
        (unless (eq gptel-aibo--working-buffer working-buffer-cand)
          (when (or (not gptel-aibo--working-project)
                    (when-let* ((candidate-project
                                 (with-current-buffer working-buffer-cand
                                   (project-current)))
                                (candidate-project-root
                                 (gptel-aibo--project-root candidate-project)))
                      (equal gptel-aibo--working-project candidate-project-root)))
            (setq gptel-aibo--working-buffer working-buffer-cand)))))))

(defun gptel-aibo--get-console ()
  "Retrieve a console matching current buffer."
  (if-let ((project (project-current)))
      (gptel-aibo--get-project-console project)
    (get-buffer-create "*gptel-aibo*")))

(defun gptel-aibo--get-project-console (project)
  "Retrieve a console matching the PROJECT."
  (let*  ((project-root-dir (gptel-aibo--project-root project))
          (project-readable-name (gptel-aibo--project-name project))
          (base-name (format "*gptel-aibo: %s*" project-readable-name)))
    (if-let* ((buffer-candidate (get-buffer base-name))
              ((equal (buffer-local-value 'gptel-aibo--working-project
                                          buffer-candidate)
                      project-root-dir)))
        buffer-candidate
      (gptel-aibo--get-project-match-console base-name project-root-dir))))

(defun gptel-aibo--get-project-match-console (base-name project-root-dir)
  "Retrieve a buffer matching BASE-NAME and PROJECT-ROOT-DIR."
  (let ((name-pattern (format "^%s<\\([0-9]+\\)>$" (regexp-quote base-name))))
    (if-let ((matching-buffer
              (seq-find
               (lambda (buffer)
                 (and (string-match name-pattern (buffer-name buffer))
                      (equal (buffer-local-value 'gptel-aibo--working-project
                                                 buffer)
                             project-root-dir)))
               (buffer-list))))
        matching-buffer
      (generate-new-buffer base-name))))

;;;###autoload
(defun gptel-aibo (&optional buffer)
  "Open or initialize a GPTEL-AIBO console buffer.

If called interactively with a prefix argument, prompt for a project-specific
console buffer or create a new one. Sets up the buffer to use `gptel-aibo-mode`
and handles project-specific configurations if applicable. Displays the console
buffer after initialization.

Optional argument BUFFER specifies the name of the buffer to manage."
  (interactive
   (list
    (and current-prefix-arg
         (let ((project-root-dir (when-let ((project (project-current)))
                                   (gptel-aibo--project-root project))))
           (read-buffer
            "Create or choose gptel-aibo console: "
            nil nil
            (lambda (b)
              (and-let*
                  ((buf (get-buffer (or (car-safe b) b)))
                   ((buffer-local-value 'gptel-aibo-mode buf))
                   ((equal
                     (buffer-local-value 'gptel-aibo--working-project buf)
                     project-root-dir))))))))))
  (let ((trigger-buffer (current-buffer))
        (console-buffer
         (if buffer (get-buffer-create buffer)
           (gptel-aibo--get-console))))
    (with-current-buffer console-buffer
      (cond ;Set major mode
       ((eq major-mode gptel-default-mode))
       ((eq gptel-default-mode 'text-mode)
        (text-mode)
        (visual-line-mode 1))
       (t (funcall gptel-default-mode)))
      (unless (local-variable-p 'gptel-prompt-prefix-alist)
        (setq-local
         gptel-prompt-prefix-alist
         (mapcar (lambda (pair)
                   (cond
                    ((eq (car pair) 'markdown-mode) (cons 'markdown-mode "\\> "))
                    ((eq (car pair) 'text-mode) (cons 'text-mode "\\> "))
                    (t pair)))
                 gptel-prompt-prefix-alist)))
      (when-let* ((current-project (project-current))
                  (project-root-dir (gptel-aibo--project-root current-project)))
        (setq gptel-aibo--working-project project-root-dir))
      (unless gptel-aibo-mode (gptel-aibo-mode 1))
      (unless (local-variable-p 'gptel-aibo--console)
        (setq-local gptel-aibo--console t)
        (if (bobp) (insert (gptel-prompt-prefix-string)))
        (when (and (bound-and-true-p evil-local-mode)
                   (fboundp 'evil-insert-state))
          (evil-insert-state)))
      (when (called-interactively-p 'any)
        (setq gptel-aibo--trigger-buffer trigger-buffer)
        (display-buffer (current-buffer) gptel-display-buffer-action)))))

(defvar gptel-aibo-send--handlers
  `((WAIT ,#'gptel--handle-wait)
    (TYPE ,#'gptel--handle-pre-insert)
    (ERRS ,#'gptel--handle-error ,#'gptel--fsm-last)
    (TOOL ,#'gptel--handle-tool-use)
    (DONE ,#'gptel-aibo--handle-post-insert
          ,#'gptel--handle-post-insert
          ,#'gptel--fsm-last))
  "Alist specifying handlers for `gptel-aibo-send' state transitions.

See `gptel-request--handlers' for details.")

;;;###autoload
(defun gptel-aibo-send ()
  "Send the current context and request to GPT for processing."
  (interactive)

  ;; HACK: gptel requires a non-empty context alist for context wrapping.
  (unless gptel-context--alist
    (setq gptel-context--alist (list (cons gptel-aibo--working-buffer nil))))

  (gptel-request nil
    :stream gptel-stream
    :context `(:working-buffer ,gptel-aibo--working-buffer)
    :fsm (gptel-make-fsm :handlers gptel-aibo-send--handlers)))

(defun gptel-aibo--handle-post-insert (fsm)
  "Handle post-insert operations for FSM.

Add text property ''gptai to the response."
  (let* ((info (gptel-fsm-info fsm))
         (working-context (plist-get info :context))
         (working-buffer (plist-get working-context :working-buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (or (plist-get info :tracking-marker)
                              start-marker))
         ;; start-marker may have been moved if :buffer was read-only
         (gptel-buffer (marker-buffer start-marker)))
    (unless (eq start-marker tracking-marker)
      (with-current-buffer gptel-buffer
        (add-text-properties start-marker tracking-marker
                             `(gptai ,working-buffer))))))

(defun gptel-aibo--insert-response (response info)
  "Insert the LLM RESPONSE into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((gptel-buffer (plist-get info :buffer))
         (working-context (plist-get info :context))
         (working-buffer (plist-get working-context :working-buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (cond
     ((stringp response)                ;Response text
      (with-current-buffer gptel-buffer
        (when-let* ((transformer (plist-get info :transformer)))
          (setq response (funcall transformer response)))
        (when tracking-marker           ;separate from previous response
          (setq response (concat "\n\n" response)))
        (save-excursion
          (add-text-properties
           0 (length response) `(gptel response
                                 gptai ,working-buffer
                                 front-sticky (gptel gptai))
           response)
          (with-current-buffer (marker-buffer start-marker)
            (goto-char (or tracking-marker start-marker))
            ;; (run-hooks 'gptel-pre-response-hook)
            (unless (or (bobp) (plist-get info :in-place)
                        tracking-marker)
              (insert "\n\n")
              (when gptel-mode
                (insert (gptel-response-prefix-string)))
              (move-marker start-marker (point)))
            (insert response)
            (plist-put info :tracking-marker
                       (setq tracking-marker (point-marker)))
            ;; for uniformity with streaming responses
            (set-marker-insertion-type tracking-marker t)))))
     ((consp response)                  ;tool call or tool result?
      (gptel--display-tool-calls response info)))))

(defun gptel-aibo--stream-insert-response (response info)
  "Insert streaming RESPONSE from an LLM into the gptel buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((working-context (plist-get info :context))
         (working-buffer (plist-get working-context :working-buffer)))
    (cond
     ((stringp response)
      (let ((start-marker (plist-get info :position))
            (tracking-marker (plist-get info :tracking-marker))
            (transformer (plist-get info :transformer)))
        (with-current-buffer (marker-buffer start-marker)
          (save-excursion
            (unless tracking-marker
              (goto-char start-marker)
              (unless (or (bobp) (plist-get info :in-place))
                (insert "\n\n")
                (when gptel-mode
                  ;; Put prefix before AI response.
                  (insert (gptel-response-prefix-string)))
                (move-marker start-marker (point)))
              (setq tracking-marker (set-marker (make-marker) (point)))
              (set-marker-insertion-type tracking-marker t)
              (plist-put info :tracking-marker tracking-marker))

            (when transformer
              (setq response (funcall transformer response)))

            (add-text-properties
             0 (length response) `(gptel response
                                   gptai ,working-buffer
                                   front-sticky (gptel gptai))
             response)
            (goto-char tracking-marker)
            ;; (run-hooks 'gptel-pre-stream-hook)
            (insert response)
            (run-hooks 'gptel-post-stream-hook)))))
     ((consp response)
      (gptel--display-tool-calls response info)))))

;;;###autoload
(defun gptel-aibo-complete-at-point ()
  "Complete text at point using LLM suggestions.

The response is inserted as an overlay with these keybindings:
- TAB or RET: Accept and move to the end of the overlay.
- Any other key: Reject and execute its normal action."
  (interactive)
  (let ((gptel--system-message gptel-aibo--system-role)
        (prompt (concat (gptel-aibo-context-info) gptel-aibo--complete-message)))
    (message "Requesting LLM suggestions...")
    ;; (message prompt)
    (gptel-request prompt
      :position (point)
      :callback #'gptel-aibo--insert-completion)))

(defun gptel-aibo--insert-completion (response info)
  "Insert the LLM RESPONSE into the calling buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (cond
   ((stringp response)
    (message "LLM response received")
    (let ((marker (plist-get info :position)))
      (with-current-buffer (marker-buffer marker)
        (when (= (point) (marker-position marker))
          (save-excursion
            (let* ((beg (point))
                   (_ (insert response))
                   (end (point))
                   (ov (make-overlay beg end))
                   (map (make-sparse-keymap)))
              (overlay-put ov 'face '(:background "lightgray"))
              (overlay-put ov 'keymap map)

              (define-key map (kbd "TAB")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map (kbd "<tab>")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map (kbd "RET")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map (kbd "<return>")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map [t]
                          (lambda ()
                            (interactive)
                            (delete-region beg end)
                            (delete-overlay ov)
                            (let ((cmd (key-binding (this-command-keys-vector))))
                              (when cmd
                                (call-interactively cmd)))))))))))
   (t
    (message "The LLM did not respond as requested."))))

(defvar gptel-aibo-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i i") 'gptel-aibo-complete-at-point)
    map)
  "Keymap used for `gptel-aibo-complete-mode`.")

;;;###autoload
(define-minor-mode gptel-aibo-complete-mode
  "Minor mode gptel-aibo llm completions."
  :lighter " GPTel-Aibo-Complete"
  :keymap gptel-aibo-complete-mode-map)

(provide 'gptel-aibo)
;;; gptel-aibo.el ends here
