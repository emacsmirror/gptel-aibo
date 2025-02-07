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
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))

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

(defvar gptai--system-role
  "You are an expert assistant specializing in helping users with Emacs for \
creating and managing various types of content, including code, documents, \
academic papers, and even novels.")

(defvar gptai--system-message
  (concat
   gptai--system-role
   "
Based on the user's request, you can generate one or more of the following \
actions:
* Modify buffers
* Create files
* Delete files

If multiple actions are required, they should be provided in sequence, and the \
program will execute them in the order they are listed.

You are free to add comments, thoughts, reasoning or other explanatory details \
before, between, or after the actions as needed. However, please note that \
these explanatory texts should not use the action format (i.e., lines starting \
with `**OP**`) to avoid being mistakenly executed as part of the solution.

The specific format descriptions for these actions are as follows:

### Modify buffers
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

### Create files
Start with line:

**OP** CREATE `<FILEPATH>`

`<FILEPATH>` is the path of the file to be created and is required.
Followed by the content, enclosed in a markdown fenced code block.

### Delete files
Just one line:

**OP** DELETE `<FILEPATH>`

`<FILEPATH>` is the path of the file to be deleted and is required.

---

### IMPORTANT: General Rule for Fenced Code Blocks

When generating code blocks, **always** ensure the fence length is adjusted \
dynamically to be long enough for the content. Avoid relying on a fixed fence \
length, such as triple backticks, unless it is already adequate.

#### Rules

1. **Inspect Content**: Check the longest sequence of consecutive backticks \
inside the content.
2. **Determine Fence Length**: Use opening and closing fences that are at least \
one backtick longer than the longest sequence in the content.
3. **Validate Before Returning**: Verify the opening and closing fences length \
to confirm they meet the rule before submitting your response.

#### Examples

````
```
Content with ```
```
````

Since the text contains triple backticks, it must be enclosed using at least a \
quadruple backtick fence.

"))

(defvar gptai--complete-message
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

(defcustom gptai--max-buffer-size 16000
  "The maximum size of working buffer's content to include in the context.

If the working buffer's content exceeds this size, only the context fragment
will be sent"
  :type 'natnum
  :group 'gptai
  :safe #'natnump)

(defcustom gptai--max-context-fragment-size 1024
  "Maximum size (in characters) for context fragments around cursor position."
  :type 'natnum
  :group 'gptai
  :safe #'natnump)

(defcustom gptai--max-project-buffer-count 2
  "The maximum number of buffers to include in the project context."
  :type 'natnum
  :group 'gptai
  :safe #'natnump)

(defcustom gptai--max-project-buffer-size 16000
  "The maximum size of a buffer's content to include in the project context.

If a buffer's content exceeds this size, only its outline will be sent"
  :type 'natnum
  :group 'gptai
  :safe #'natnump)

(defvar-local gptai--working-buffer nil
  "Current working buffer of gptel-aibo.")

(defvar-local gptai--ui-buffer nil
  "Current ui buffer of gptel-aibo.")

(defvar-local gptai--old-directives nil)

(defvar-local gptai--old-system-message nil)

(defvar-local gptai--old-use-context nil)

(defvar-local gptai--old-context-wrap-function nil)

(defvar-local gptai--from-gptel-mode nil
  "If this from `gptel-mode'.")

(require 'gptel)
(require 'gptel-context)
(require 'gptai-context)
(require 'gptai-action)

(defvar gptai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptai-send)
    (define-key map (kbd "C-c !") #'gptai-apply-last-suggestions)
    map)
  "Keymap for `gptai-mode'.")

;;;###autoload
(define-minor-mode gptai-mode
  "Minor mode for gptel-aibo interacting with LLMs."
  :lighter " GPTAi"
  :keymap gptai-mode-map
  (if gptai-mode
      (progn
        (if gptel-mode
            (setq gptai--from-gptel-mode gptel-mode)
          (gptel-mode 1))
        (setq gptai--old-directives gptel-directives)
        (setq-local gptel-directives (cons `(GPTAi . ,gptai--system-message)
                                           gptel-directives))
        (setq gptai--old-system-message gptel--system-message)
        (setq-local gptel--system-message gptai--system-message)
        (setq gptai--old-use-context gptel-use-context)
        (setq-local gptel-use-context 'user)
        (setq gptai--old-context-wrap-function gptel-context-wrap-function)
        (setq-local gptel-context-wrap-function #'gptai-context-wrap)

        (setq gptai--ui-buffer (current-buffer))
        (setq gptai--working-buffer (other-buffer gptai--ui-buffer t))
        ;; (setq header-line-format
        ;;       (list '(:eval (concat "<"
        ;;                             (buffer-name gptai--working-buffer)
        ;;                             ">"))))
        (add-hook 'buffer-list-update-hook #'gptai-mode--check-buffer-list nil t)
        (message "gptai-mode enabled"))
    (remove-hook 'buffer-list-update-hook #'gptai-mode--check-buffer-list)
    (setq-local gptel-directives gptai--old-directives)
    (setq-local gptel--system-message gptai--old-system-message)
    (setq-local gptel-use-context gptai--old-use-context)
    (setq-local gptel-context-wrap-function gptai--old-context-wrap-function)
    (unless gptai--from-gptel-mode
      (gptel-mode -1))
    (message "gptai-mode disabled")))

(defun gptai-mode--check-buffer-list ()
  "Check and update the working buffer for gptai mode."
  (when (eq gptai--ui-buffer (car (buffer-list)))
    (setq gptai--working-buffer (other-buffer gptai--ui-buffer t))))

;;;###autoload
(defun gptai (name)
  "Create or switch to an gptai buffer with NAME."
  (interactive
   (let* ((backend (default-value 'gptel-backend))
          (backend-name
           (format "*gptai-%s*" (gptel-backend-name backend))))
     (list (read-buffer
            "Create or choose gptel-aibo buffer: "
            backend-name
            nil
            (lambda (b)
              (and-let* ((buf (get-buffer (or (car-safe b) b))))
                (buffer-local-value 'gptai-mode buf)))))))
  (with-current-buffer (get-buffer-create name)
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
    (unless gptai-mode (gptai-mode 1))
    (if (bobp) (insert (gptel-prompt-prefix-string)))
    (when (called-interactively-p 'any)
      (display-buffer (current-buffer) gptel-display-buffer-action))))

;;;###autoload
(defun gptai-send ()
  "Send the current context and request to GPT for processing."
  (interactive)

  (when (or (not gptai--working-buffer)
            (not (buffer-live-p gptai--working-buffer)))
    (setq gptai--working-buffer (other-buffer gptai--ui-buffer t)))

  ;; HACK: gptel requires a non-empty context alist for context wrapping.
  (unless gptel-context--alist
    (setq gptel-context--alist (list (cons gptai--working-buffer nil))))

  (gptel-request nil
    :stream gptel-stream
    :callback (if gptel-stream #'gptai--stream-insert-response
                #'gptai--insert-response)
    :context `(:working-buffer ,gptai--working-buffer)
    :fsm (gptel-make-fsm :handlers gptel-send--handlers)))

(defun gptai--insert-response (response info)
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
                                 gptai--working-buffer ,working-buffer
                                 front-sticky (gptel gptai--working-buffer))
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
            (plist-put info :tracking-marker (setq tracking-marker (point-marker)))
            ;; for uniformity with streaming responses
            (set-marker-insertion-type tracking-marker t)))))
     ((consp response)                  ;tool call or tool result?
      (gptel--display-tool-calls response info)))))

(defun gptai--stream-insert-response (response info)
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
                                   gptai--working-buffer ,working-buffer
                                   front-sticky (gptel gptai--woring-buffer))
             response)
            (goto-char tracking-marker)
            ;; (run-hooks 'gptel-pre-stream-hook)
            (insert response)
            (run-hooks 'gptel-post-stream-hook)))))
     ((consp response)
      (gptel--display-tool-calls response info)))))

;;;###autoload
(defun gptai-complete-at-point ()
  "Complete text at point using LLM suggestions.

The response is inserted as an overlay with these keybindings:
- TAB or RET: Accept and move to the end of the overlay.
- Any other key: Reject and execute its normal action."
  (interactive)
  (let ((gptel--system-message gptai--system-role)
        (prompt (concat (gptai-context-info) gptai--complete-message)))
    (message "Requesting LLM suggestions...")
    ;; (message prompt)
    (gptel-request prompt
      :position (point)
      :callback #'gptai--insert-completion)))

(defun gptai--insert-completion (response info)
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

(defvar gptai-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i i") 'gptai-complete-at-point)
    map)
  "Keymap used for `gptai-complete-mode`.")

;;;###autoload
(define-minor-mode gptai-complete-mode
  "Minor mode gptai llm completions."
  :lighter " GPTAi-Complete"
  :keymap gptai-complete-mode-map)

(provide 'gptel-aibo)
;;; gptel-aibo.el ends here
