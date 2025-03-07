;;; gptel-aibo-summon.el --- summon parse and apply -*- lexical-binding: t; -*-
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

;;; Summon to your fingertips

;;; Code:

(require 'gptel-aibo-action-parser)
(require 'gptel-aibo-inplace-diff)
(require 'gptel-aibo-face)

(require 'gptel)
(require 'gptel-context)

(defvar gptel-aibo--summon-prompt
  "**Your Task**:
Provide content suitable for insertion at the cursor position to complete the
unfinished statements. Use a SEARCH/REPLACE pair to apply the change.
The format is as follows:

*SEARCH*
```language-id (OPTIONAL)
{{Content around the cursor to be replaced}}
```
*REPLACE*
```language-id (OPTIONAL)
{{Content to replace with}}
```

Please replace the placeholder text inside the markers `{{` and `}}`, along with
the markers themselves, with the actual content of the SEARCH or REPLACE.

### Rules
1. Only provide concrete, definitive content at the insertion point. Do not use a
   template or outline, or mix with explanations.
2. Do not modify other parts of the content. If the insertion leads to changes
   that should be made to the next few lines, place those changes in the
   \"Nearby Modification\" section, which will be introduced later.
3. The SEARCH content should not include irrelevant content, as changes in
   irrelevant content could make the SEARCH/REPLACE unidentifiable.
4. If the context is unclear, provide only a minimal insertion that stands on its
   own. For example, if the context suggests adding one or multiple items
   (such as elements, statements, or other definitions) but the exact number cannot
   be inferred, adding a single item is sufficient, and the possibly second one
   can be placed in the \"Next Predicts\" section, which will be introduced later.

### Nearby Modification
After inserting the provided content, if the next few lines in the original
content are very likely to require modification accordingly, you may also provide
a SEARCH/REPLACE pair to modify them. Start Nearby Modification with the marker line
```
=Nearby Modification=
```

### Next Predicts
After inserting the provided content, you may also optionally provide a
Next Predicts section, still in a SEARCH/REPLACE pair format.

**Key Difference With Nearby Modification**

- Nearby Modification is for changes to the existing content that ensure consistency
  or correctness after the insertion.
- Next Predicts is about predicting what content will logically come next, based on
  the structure of the document.

Like the primary insertion, Next Predicts should provide concrete, definitive
content rather than a template or outline.

The SEARCH part of Next Predicts would often refer to the updated document
structure after this insertion.

For simplicity, do not provide Nearby Modification for Next Predicts.

Start Next Predicts with the marker line
```
=Next Predicts=
```

### Final Notes
1. Make decisions based solely on the information provided to you. Do not call
   tools or ask questions to obtain additional information.
2. If there’s no applicable content to provide, return an empty string as the
   entire result.


")

(defvar gptel-aibo--system-role)

;;;###autoload
(defun gptel-aibo-summon ()
  "Summon AIBO to assist with code completion at point.
This function initiates an asynchronous completion request using
the current buffer's content and position."
  (interactive)
  (let ((gptel--system-message gptel-aibo--system-role)
        (prompt (concat "Request context:\n\n"
                        (gptel-aibo-summon-context-info)
                        gptel-aibo--summon-prompt)))
    (message "Requesting LLM suggestions...")
    ;; (message prompt)
    (gptel-request prompt
      :position (point)
      :callback #'gptel-aibo--summon-callback)))

(defun gptel-aibo-fake-summon ()
  "Fake summon."
  (interactive)
  (let ((response "
*SEARCH*
```
set(SPDLOG_FMT_EXTERNAL ON)
FetchContent_Declare()

FetchContent_MakeAvailable(fmt absl)
```
*REPLACE*
```
set(SPDLOG_FMT_EXTERNAL ON)
FetchContent_Declare(
  hello
)

FetchContent_MakeAvailable(fmt absl)
```

=Nearby Modification=

*SEARCH*
```
FetchContent_MakeAvailable(fmt absl)
```

*REPLACE*
```
FetchContent_MakeAvailable(fmt absl spdlog)
```

=Next Predicts=

*SEARCH*
```
add_executable(tt main.cc)
```

*REPLACE*
```
add_executable(tt main.cc)
target_link_libraries(tt PRIVATE fmt absl::absl spdlog)
```

"))
    (run-at-time 0 nil #'gptel-aibo--summon-on-response
                 (current-buffer) (point) response)))


(defun gptel-aibo--summon-callback (response info)
  "Insert the LLM RESPONSE into the calling buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  ;; (message "Response: [%s]" response)
  (cond
   ((stringp response)
    (message "LLM response received")
    (let ((marker (plist-get info :position)))
      (with-current-buffer (marker-buffer marker)
        (when (= (point) (marker-position marker))
          (save-excursion
            (gptel-aibo--summon-on-response
             (current-buffer) (point) response))))))
   (t
    (message "The LLM responds with errors: %s"
             (plist-get info :status)))))

(defun gptel-aibo--summon-on-response (buffer point response)
  "Parse RESPONSE and apply modifcations on BUFFER at POINT."
  (unless (string-match-p "\\`[ \t\n\r]*\\'" response)
    (let* ((cursor-symbol
            (gptel-aibo--cursor-symbol
             (buffer-substring-no-properties (point-min) (point-max))))
           (parse-result
            (gptel-aibo--summon-parse-response response cursor-symbol)))
      (if (eq (car parse-result) 'error)
          (message "LLM responds with format error: %s" (cadr parse-result))
        (let ((insertion (car parse-result))
              (nearby-modification (cadr parse-result))
              (next-predicts (nth 2 parse-result)))
          (unless (equal (car insertion) (cdr insertion))
            (when (and nearby-modification
                       (equal (car nearby-modification) (cdr nearby-modification)))
              (setq nearby-modification nil))
            (when (and next-predicts
                       (equal (car next-predicts) (cdr next-predicts)))
              (setq next-predicts nil))
            (gptel-aibo--summon-apply
             buffer point insertion
             nearby-modification
             next-predicts)))))))

(defun gptel-aibo--summon-parse-response (response &optional cursor-symbol)
  "Parse GPT response into insertion and nearby modification.
RESPONSE is the raw string response from GPT. Returns a list of
two elements: (INSERTION NEARBY-MODIFICATION), where:
- INSERTION is the primary content to insert
- NEARBY-MODIFICATION is a search/replace pair for nearby changes

The response format should contain:
1. A primary insertion block
2. Optionally, a Nearby Modification section with a search/replace pair.

CURSOR-SYMBOL if provided, will be used to removed from the RESPONSE."
  (let ((lines (split-string response "\n"))
        (insertion nil)
        (nearby-modification nil)
        (next-predicts nil))
    (catch 'parse-result
      (let ((insertion-parse-result
             (gptel-aibo--parse-search-replace-pair lines)))
        (when (null (car insertion-parse-result))
          (throw 'parse-result (list 'error "No valid insertion.")))
        (when (eq (car insertion-parse-result) 'error)
          (throw 'parse-result insertion-parse-result))
        (setq insertion (car insertion-parse-result))
        (when (and insertion cursor-symbol
                   (string-match-p (regexp-quote cursor-symbol) (car insertion)))
          (setf (car insertion) (replace-regexp-in-string
                                 (regexp-quote cursor-symbol) ""
                                 (car insertion))))
        (setq lines (cdr insertion-parse-result))

        (while (and lines (string-blank-p (car lines)))
          (setq lines (cdr lines)))

        (when (and lines (string= (car lines) "=Nearby Modification="))
          (let ((nearby-modification-parse-result
                 (gptel-aibo--parse-search-replace-pair (cdr lines))))
            (when (eq (car nearby-modification-parse-result) 'error)
              (throw 'parse-result (list insertion nil)))
            (setq nearby-modification (car nearby-modification-parse-result))
            (setq lines (cdr nearby-modification-parse-result))))

        (while (and lines (string-blank-p (car lines)))
          (setq lines (cdr lines)))

        (when (and lines (string= (car lines) "=Next Predicts="))
          (let ((next-predicts-parse-result
                 (gptel-aibo--parse-search-replace-pair (cdr lines))))
            (when (eq (car next-predicts-parse-result) 'error)
              (throw 'parse-result (list insertion nearby-modification)))
            (setq next-predicts (car next-predicts-parse-result))))

        (list insertion nearby-modification next-predicts)))))

(defun gptel-aibo--summon-apply
    (buffer point insertion &optional nearby-modification next-predicts)
  "Apply INSERTION and NEARBY-MODIFICATION at POINT in BUFFER."
  (let* ((insertion-diff-parts
          (gptel-aibo--extract-diff-lines (car insertion) (cdr insertion)))
         (insertion-diff-lines (car insertion-diff-parts))
         (insertion-diff-offsets (cadr insertion-diff-parts))
         (nearby-diff-parts
          (when nearby-modification
            (gptel-aibo--extract-diff-lines
             (car nearby-modification) (cdr nearby-modification))))
         (nearby-diff-lines
          (when nearby-diff-parts (car nearby-diff-parts)))
         (nearby-diff-offsets
          (when nearby-diff-parts (cadr nearby-diff-parts)))
         (insertion-diffs nil)
         (nearby-modification-diffs nil)
         (insertion-ready nil)
         (modification-ready nil))

    (cl-labels
        ((maybe-apply-diffs ()
           (when (and insertion-ready
                      (or (not nearby-modification) modification-ready))
             (gptel-aibo--summon-apply-with-diffs
              buffer point
              (list (car insertion)
                    insertion-diff-offsets
                    insertion-diff-lines
                    insertion-diffs)
              (when nearby-modification
                (list (car nearby-modification)
                      nearby-diff-offsets
                      nearby-diff-lines
                      nearby-modification-diffs))
              next-predicts))))

      (gptel-aibo--inplace-diff
       (car insertion-diff-lines)
       (cdr insertion-diff-lines)
       (lambda (diffs)
         (setq insertion-diffs diffs
               insertion-ready t)
         (maybe-apply-diffs)))

      (when nearby-modification
        (gptel-aibo--inplace-diff
         (car nearby-diff-lines)
         (cdr nearby-diff-lines)
         (lambda (diffs)
           (setq nearby-modification-diffs diffs
                 modification-ready t)
           (maybe-apply-diffs)))))))

(defun gptel-aibo--summon-apply-with-diffs
    (buffer point insertion nearby-modification next-predicts)
  "Apply INSERTION at POINT in BUFFER with completion overlay.

If NEARBY-MODIFICATION and/or NEXT-PREDICTS is provided, they will be applied
also."
  (with-current-buffer buffer
    (catch 'apply-fail
      (unless (= (point) point)
        (throw 'apply-fail nil))
      (let* ((search (car insertion))
             (insertion-diff-offsets (cadr insertion))
             (insertion-diff-lines (nth 2 insertion))
             (diffs (nth 3 insertion))
             (search-len (length search))
             (search-beg (max (point-min) (- point search-len)))
             (search-end (min (point-max) (+ point search-len))))
        (goto-char search-beg)
        (unless (search-forward search search-end t)
          (throw 'apply-fail nil))

        (let* ((insertion-diff-start (+ (match-beginning 0)
                                        (car insertion-diff-offsets)))
               (insertion-diff-end (- (match-end 0)
                                      (cdr insertion-diff-offsets)))
               (ins-ov (gptel-aibo--inplace-render-diffs
                        insertion-diff-start insertion-diff-end diffs))
               (sub-ovs (overlay-get ins-ov 'sub-ovs))
               (mod-ov nil)
               (keymap (make-sparse-keymap)))

          (when nearby-modification
            (let ((search (car nearby-modification))
                  (nearby-diff-offsets (cadr nearby-modification))
                  (diffs (nth 3 nearby-modification)))
              (goto-char point)
              (when (search-forward search nil t)
                (let ((nearby-diff-start
                       (+ (match-beginning 0) (car nearby-diff-offsets)))
                      (nearby-diff-end
                       (- (match-end 0) (cdr nearby-diff-offsets))))
                  (when (>= nearby-diff-start insertion-diff-end)
                    (setq mod-ov
                          (gptel-aibo--inplace-render-diffs
                           nearby-diff-start
                           nearby-diff-end
                           diffs)))))))

          (overlay-put ins-ov 'keymap keymap)
          (when mod-ov
            (overlay-put mod-ov 'keymap keymap))

          (cl-loop
           for key in '("RET" "<return>")
           do
           (define-key
            keymap (kbd key)
            (lambda ()
              (interactive)

              (when next-predicts
                (let* ((next-predicts-diff-parts
                        (gptel-aibo--extract-diff-lines (car next-predicts)
                                                        (cdr next-predicts)))
                       (next-predicts-diff-lines (car next-predicts-diff-parts))
                       (next-predicts-diff-offsets (cadr next-predicts-diff-parts)))
                  (gptel-aibo--inplace-diff
                   (car next-predicts-diff-lines)
                   (cdr next-predicts-diff-lines)
                   (lambda (diffs)
                     (gptel-aibo--summon-render-diffs
                      (car next-predicts)
                      next-predicts-diff-offsets
                      next-predicts-diff-lines
                      diffs)))))

              (let ((end (overlay-end ins-ov)))

                (dolist (ov sub-ovs)
                  (let ((diff (overlay-get ov 'aibo-diff)))
                    (if (eq (gptel-aibo-diff-type diff) :removed)
                        (delete-region (overlay-start ov) (overlay-end ov))
                      (delete-overlay ov))))
                (delete-overlay ins-ov)

                (when (and nearby-modification mod-ov)
                  (let ((sub-ovs (overlay-get mod-ov 'sub-ovs)))
                    (dolist (ov sub-ovs)
                      (let ((diff (overlay-get ov 'aibo-diff)))
                        (if (eq (gptel-aibo-diff-type diff) :removed)
                            (delete-region (overlay-start ov) (overlay-end ov))
                          (delete-overlay ov)))))
                  (delete-overlay mod-ov))

                (goto-char end)))))

          (define-key
           keymap (kbd "<escape>")
           (lambda ()
             (interactive)
             (let ((start (overlay-start ins-ov))
                   (end (overlay-end ins-ov)))
               (delete-region start end)
               (goto-char start)
               (insert (car insertion-diff-lines)))

             (when (and nearby-modification mod-ov)
               (save-excursion
                 (let ((start (overlay-start mod-ov))
                       (end (overlay-end mod-ov)))
                   (delete-region start end)
                   (goto-char start)
                   (insert (car (nth 2 nearby-modification))))))

             (goto-char point)))

          (goto-char point))))))

(defun gptel-aibo--summon-render-diffs
    (search diff-offsets diff-lines diffs &optional search-beg search-end)
  "Render DIFFS in-place with interactive keybindings.

SEARCH is the text to find, DIFF-OFFSETS specifies match bounds,
DIFF-LINES contains original content. Optional SEARCH-BEG/END
limit search region. Creates overlays with RET/ESC bindings:
RET applies changes, ESC reverts to original."
  (when search-beg
    (goto-char search-beg))
  (when (search-forward search (or search-end (point-max)) t)
    (let* ((ov (gptel-aibo--inplace-render-diffs
                   (+ (match-beginning 0) (car diff-offsets))
                   (- (match-end 0) (cdr diff-offsets))
                   diffs))
              (sub-ovs (overlay-get ov 'sub-ovs))
              (keymap (make-sparse-keymap)))

      (overlay-put ov 'keymap keymap)

      (cl-loop
       for key in '("RET" "<return>")
       do
       (define-key
        keymap (kbd key)
        (lambda ()
          (interactive)
          (let ((end (overlay-end ov)))
            (dolist (ov sub-ovs)
              (let ((diff (overlay-get ov 'aibo-diff)))
                (if (eq (gptel-aibo-diff-type diff) :removed)
                    (delete-region (overlay-start ov) (overlay-end ov))
                  (delete-overlay ov))))
            (delete-overlay ov)
            (goto-char end)))))

      (define-key
       keymap (kbd "<escape>")
       (lambda ()
         (interactive)
         (let ((start (overlay-start ov))
               (end (overlay-end ov)))
           (delete-region start end)
           (goto-char start)
           (insert (car diff-lines))
           (goto-char start))))

      (goto-char (overlay-start ov)))))

(defun gptel-aibo--extract-diff-lines (str1 str2)
  "Extract the differing lines between STR1 and STR2.

Returning ''((str1-diff-part . str2-diff-part)
             (str1-diff-start-offset . str1-diff-end-offset))."
  (let* ((str1-lines (split-string str1 "\n"))
         (str2-lines (split-string str2 "\n"))
         (len-str1 (length str1-lines))
         (len-str2 (length str2-lines))
         (start 0)
         (end-str1 len-str1)
         (end-str2 len-str2))

    (while (and (< start len-str1) (< start len-str2)
                (string= (nth start str1-lines) (nth start str2-lines)))
      (setq start (1+ start)))

    (while (and (> end-str1 start) (> end-str2 start)
                (string= (nth (1- end-str1) str1-lines)
                         (nth (1- end-str2) str2-lines)))
      (setq end-str1 (1- end-str1))
      (setq end-str2 (1- end-str2)))

    (let* ((str1-start-offset
            (length (string-join (cl-subseq str1-lines 0 start) "\n")))
           (str2-start-offset
            (length (string-join (cl-subseq str2-lines 0 start) "\n")))
           (str1-end-offset
            (length (string-join (cl-subseq str1-lines end-str1) "\n")))
           (str2-end-offset
            (length (string-join (cl-subseq str2-lines end-str2) "\n"))))

      (when (and (> start 0) (< start len-str1) (< start len-str2))
        (setq str1-start-offset (1+ str1-start-offset))
        (setq str2-start-offset (1+ str2-start-offset)))

      (when (and (< end-str1 len-str1) (> end-str1 start) (> end-str2 start))
        (setq str1-end-offset (1+ str1-end-offset))
        (setq str2-end-offset (1+ str2-end-offset)))

      (let* ((str1-diff-part (substring str1 str1-start-offset
                                        (when (> str1-end-offset 0)
                                          (- str1-end-offset))))
             (str2-diff-part (substring str2 str2-start-offset
                                        (when (> str2-end-offset 0)
                                           (- str2-end-offset)))))
        (list (cons str1-diff-part str2-diff-part)
              (cons str1-start-offset str1-end-offset))))))

(defun gptel-aibo--string-common-suffix (s1 s2)
  "Find the common suffix between two strings S1 and S2."
  (let* ((len1 (length s1))
         (len2 (length s2))
         (p1 len1)
         (p2 len2))
    (while (and (> p1 0) (> p2 0)
                (eq (aref s1 (1- p1))
                    (aref s2 (1- p2))))
      (setq p1 (1- p1)
            p2 (1- p2)))
    (cond
     ((= p1 0) s1)
     ((= p2 0) s2)
     ((= p1 len1) "")
     (t (substring s1 p1)))))

(provide 'gptel-aibo-summon)
;;; gptel-aibo-summon.el ends here
