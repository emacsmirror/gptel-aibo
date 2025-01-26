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
;; Homepage: https://github.com/dolmens/aics
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
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
(require 'gptel-context)
(require 'aics-context)
(require 'aics-action)

(defvar aics--system-role
  "You are an expert assistant specializing in helping users with Emacs for \
creating and managing various types of content, including code, documents, \
academic papers, and even novels.")

(defvar aics--system-message
  (concat
   aics--system-role
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

### Execute a piece of Elisp code
Start with line:

**OP** ELISP

Followed by the elisp code snippet, enclosed in a markdown fenced code block.
For example:

**OP** ELISP
```elisp
(insert \"Hello, Emacs!\")
```

---

### IMPORTANT: General Rule for Fenced Code Blocks

When generating code blocks, **always** ensure the fence length is adjusted \
dynamically to be long enough for the content. Avoid relying on a fixed fence \
length, such as triple backticks, unless it is already adequate.

#### Rules

1. **Inspect Content**: Check the longest sequence of consecutive backticks \
inside the content.
2. **Determine Fence Length**: Use opening and closing fences that are at least one \
backtick longer than the longest sequence in the content.
3. **Validate Before Returning**: Verify the opening and closing fences length \
to confirm they meet the rule before submitting your response.

#### Examples

````
```
Content with ```
```
````

Note: Since the text contains triple backticks, it must be enclosed using at \
least a quadruple backtick fence.

"))

(defvar aics--complete-message
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

(defvar-local aics--ui-buffer nil
  "Aics current ui buffer.")

(defvar-local aics--from-gptel-mode nil
  "If this from gptel-mode.")

;;;###autoload
(define-minor-mode aics-mode
  "Minor mode for aics interacting with LLMs."
  :lighter " aics"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'aics-send)
    (define-key map (kbd "C-c !") #'aics--parse-last-suggestions-and-apply)
    map)
  (if aics-mode
      (progn
        (if gptel-mode
            (setq aics--from-gptel-mode gptel-mode)
          (gptel-mode 1))
        (setq-local gptel--system-message aics--system-message)
        (setq aics--ui-buffer (current-buffer))
        (setq aics-context--working-buffer (other-buffer nil t))
        ;; (setq header-line-format
        ;;       (list '(:eval (concat "<"
        ;;                             (buffer-name aics-context--working-buffer)
        ;;                             ">"))))
        (add-hook 'buffer-list-update-hook #'aics-mode--check-buffer-list nil t)
        (message "aics-mode enabled"))
    (unless aics--from-gptel-mode
      (gptel-mode -1))
    (remove-hook 'buffer-list-update-hook #'aics-mode--check-buffer-list)
    (message "aics-mode disabled")))

(defun aics-mode--check-buffer-list ()
  (when (eq aics--ui-buffer (car (buffer-list)))
    (setq aics-context--working-buffer (other-buffer nil t))))

;;;###autoload
(defun aics-send ()
  (interactive)

  (when (not (buffer-live-p aics-context--working-buffer))
    (setq aics-context--working-buffer (other-buffer t nil)))

  (let ((current-gptel-context-wrap-function gptel-context-wrap-function))
    (setq gptel-context--alist (cons (cons aics-context--working-buffer nil)
                                     gptel-context--alist))
    (setq gptel-context-wrap-function #'aics-context--wrap)
    (gptel-request nil
      :stream gptel-stream
      :fsm (gptel-make-fsm :handlers gptel-send--handlers))
    (setq gptel-context-wrap-function current-gptel-context-wrap-function)))

;;;###autoload
(defun aics (name)
  (interactive
   (let* ((backend (default-value 'gptel-backend))
          (backend-name
           (format "*aics-%s*" (gptel-backend-name backend))))
     (list (read-buffer
            "Create or choose aics buffer: "
            backend-name
            nil
            (lambda (b)
              (and-let* ((buf (get-buffer (or (car-safe b) b))))
                (buffer-local-value 'aics-mode buf)))))))
  (with-current-buffer (get-buffer-create name)
    (cond ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (unless aics-mode (aics-mode 1))
    (if (bobp) (insert (gptel-prompt-prefix-string)))
    (when (called-interactively-p 'any)
      (display-buffer (current-buffer) gptel-display-buffer-action))))

;;;###autoload
(defun aics-complete-at-point ()
  (interactive)
  (let ((gptel--system-message aics--system-role)
        (prompt (concat (aics-context--info) aics--complete-message)))
    (message prompt)
    (gptel-request prompt)))

(provide 'aics)
;;; aics.el ends here
