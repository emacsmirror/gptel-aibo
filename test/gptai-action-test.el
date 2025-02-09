(require 'ert)
(require 'gptai-action)

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.

The temporary directory will be deleted on exit/error."
  `(let ((,dir (make-temp-file "test-" t)))
    (unwind-protect
        (progn ,@body)
      (delete-directory ,dir t))))

(ert-deftest test-gptai--create-op-parser ()
  (should (equal (gptai--create-op-parser "MODIFY")
                 (gptai-make-modification-op-parser)))
  (should (equal (gptai--create-op-parser "CREATE")
                 (gptai-make-creation-op-parser)))
  (should (equal (gptai--create-op-parser "DELETE")
                 (gptai-make-deletion-op-parser)))
  (should-not (gptai--create-op-parser "?")))

(ert-deftest test-gptai-parse-modification-op-full-content ()
  "Test gptai-parse-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

```
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptai-make-modification-op-parser))
         (result
          (gptai-parse-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptai-make-modification-op
                     :target target
                     :full-content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptai-parse-modification-op-replacements ()
  "Test gptai-parse-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

*SEARCH*
```
hello
```
*REPLACE*
```
world
```


*SEARCH*
```
hi
```

*REPLACE*
```
ha
```



lines remain")
         (lines (split-string input "\n"))
         (parser (gptai-make-modification-op-parser))
         (result
          (gptai-parse-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptai-make-modification-op
                     :target target
                     :replacements '(("hello" . "world") ("hi" . "ha")))))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptai-modification-op-parser-invalid ()
  "Test gptai-parse-op with modification op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
*SEARCH*
```
hello
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptai-make-modification-op-parser))
         (result
          (gptai-parse-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptai-parse-creation-op ()
  "Test gptai-parse-op with creation op."
  (let* ((filename "filename1")
         (input "
 \n

```
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptai-make-creation-op-parser))
         (result
          (gptai-parse-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptai-make-creation-op
                     :filename filename
                     :content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptai-parse-creation-op-invalid ()
  "Test gptai-parse-op with creation op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
not begin with code block
```
hello
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptai-make-creation-op-parser))
         (result
          (gptai-parse-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptai--parse-code-block ()
  "Test gptai--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (result (gptai--parse-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptai--parse-code-block-end-fence-with-space ()
  "Test gptai--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
``` \nlines remain")
         (lines (split-string input "\n"))
         (result (gptai--parse-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptai--parse-code-block-end-fence-with-others ()
  "Test gptai--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
```NO!
lines remain")
         (lines (split-string input "\n"))
         (result (gptai--parse-code-block lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptai-parse-deletion-op ()
  "Test gptai-parse-op with deletion op."
  (let* ((filename "filename1")
         (lines '("lines remain"))
         (parser (gptai-make-deletion-op-parser))
         (result
          (gptai-parse-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptai-make-deletion-op
                     :filename filename)))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptai-execute-modification-op-full-content ()
  "Test gptai-execute with modification op with full-content."
  (with-temp-buffer
    (insert "hello")
    (let* ((buffer-name (buffer-name))
           (full-content "world")
           (op (gptai-make-modification-op
                :target buffer-name
                :full-content full-content)))
      (gptai-execute op)
      (with-current-buffer buffer-name
        (should (string= (buffer-string) full-content))))))

(ert-deftest test-gptai-execute-modification-op-replacements ()
  "Test gptai-execute with modification op with replacements."
  (with-temp-buffer
    (insert "hello
hello
world")
    (let* ((buffer-name (buffer-name))
           (op (gptai-make-modification-op
                :target buffer-name
                :replacements '(("hello" . "hi") ("world" . "earth")))))
      (gptai-execute op)
      (with-current-buffer buffer-name
        (should (string= (buffer-string) "hi
hello
earth"))))))

(ert-deftest test-gptai-execute-modification-op-full-content-not-exist ()
  "Test gptai-execute with modification op with full-content not exit."
  (let* ((buffer-name (make-temp-name "*not-exist-"))
         (full-content "world")
         (op (gptai-make-modification-op
              :target buffer-name
              :full-content full-content)))
    (should-error (gptai-execute op) :type 'error)))

(ert-deftest test-gptai-execute-modification-op-full-content-not-project ()
  "Test gptai-execute with modification op with full-content not project."
  (with-temp-directory
   working-dir
   ;; NOT a project
   ;; (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let ((working-buffer (current-buffer)))
       (with-temp-buffer
         (insert "hello")
         (let* ((target-buffer (current-buffer))
                (full-content "world")
                (op (gptai-make-modification-op
                     :target (buffer-name target-buffer)
                     :full-content full-content)))
           (with-current-buffer working-buffer
             (should-error (gptai-execute op) :type 'error))))))))

(ert-deftest test-gptai-execute-modification-op-full-content-other-project ()
  "Test gptai-execute with modification op with full-content in other project."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (with-temp-directory
    target-dir
    (make-directory (expand-file-name ".git" target-dir))
    (with-temp-buffer
      (setq default-directory working-dir)
      (let ((working-buffer (current-buffer)))
        (with-temp-buffer
          (setq default-directory target-dir)
          (insert "hello")
          (let* ((target-buffer (current-buffer))
                 (full-content "world")
                 (op (gptai-make-modification-op
                      :target (buffer-name target-buffer)
                      :full-content full-content)))
            (with-current-buffer working-buffer
              (should-error (gptai-execute op) :type 'error)))))))))

(ert-deftest test-gptai-execute-modification-op-full-content-in-project ()
  "Test gptai-execute with modification op with full-content in project."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let ((working-buffer (current-buffer)))
       (with-temp-buffer
         (insert "hello")
         (let* ((target-buffer (current-buffer))
                (full-content "world")
                (op (gptai-make-modification-op
                     :target (buffer-name target-buffer)
                     :full-content full-content)))
           (with-current-buffer working-buffer
             (gptai-execute op)
             (with-current-buffer target-buffer
               (should (string= (buffer-string) full-content))))))))))

(ert-deftest test-gptai-execute-modification-op-replacements-in-project ()
  "Test gptai-execute with modification op with full-content in project."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let ((working-buffer (current-buffer)))
       (with-temp-buffer
         (insert "hello
hello
world")
         (let* ((target-buffer (current-buffer))
                (op (gptai-make-modification-op
                     :target (buffer-name target-buffer)
                     :replacements '(("hello" . "hi") ("world" . "earth")))))
           (with-current-buffer working-buffer
             (gptai-execute op)
             (with-current-buffer target-buffer
               (should (string= (buffer-string) "hi
hello
earth"))))))))))

(ert-deftest test-gptai-execute-creation-op ()
  "Test gptai-execute with creation op."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((recentf-exclude '(".*"))
            (require-final-newline nil)
            (filename (make-temp-name (concat working-dir "/")))
            (content "hello")
            (op (gptai-make-creation-op
                 :filename filename
                 :content content)))
       (gptai-execute op)
       (find-file filename)
       (should (string= (buffer-string) content))
       (kill-buffer)))))

(ert-deftest test-gptai-execute-creation-op-not-project ()
  "Test gptai-execute with creation op not project."
  (with-temp-directory
   working-dir
   ;; NOT a project
   ;; (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (content "hello")
            (op (gptai-make-creation-op
                 :filename filename
                 :content content)))
       (should-error (gptai-execute op))))))

(ert-deftest test-gptai-execute-creation-op-other-project ()
  "Test gptai-execute with creation op other project."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (with-temp-directory
    target-dir
    (make-directory (expand-file-name ".git" target-dir))
    (with-temp-buffer
      (setq default-directory working-dir)
      (let* ((filename (make-temp-name (concat target-dir "/")))
             (content "hello")
             (op (gptai-make-creation-op
                  :filename filename
                  :content content)))
        (should-error (gptai-execute op)))))))

(ert-deftest test-gptai-execute-deletion-op ()
  "Test gpti-execute with deletion op."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (op (gptai-make-deletion-op
                 :filename filename)))
       (setq gptai--delete-confirmation nil)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?y)))
         (gptai-execute op))
       (should-not (file-exists-p filename))

       (should-not gptai--delete-confirmation)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?n)))
         (gptai-execute op))
       (should (file-exists-p filename))

       (should-not gptai--delete-confirmation)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?a)))
         (gptai-execute op))
       (should-not (file-exists-p filename))

       (should (eq gptai--delete-confirmation 'always))

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice)
                  (lambda (&rest _)
                    (error "Should not call read-char-choice when always"))))
         (gptai-execute op))
       (should-not (file-exists-p filename))

       (setq gptai--delete-confirmation nil)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?N)))
         (gptai-execute op))
       (should (file-exists-p filename))

       (should (eq gptai--delete-confirmation 'never))

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice)
                  (lambda (&rest _)
                    (error "Should not call read-char-choice when never"))))
         (gptai-execute op))
       (should (file-exists-p filename))))))

(ert-deftest test-gptai-execute-deletion-not-exist ()
  "Test gptai-execute with deletion op not exist."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (op (gptai-make-deletion-op
                 :filename filename)))
       (setq gptai--delete-confirmation nil)
       (should-not (file-exists-p filename))
       (should-error (gptai-execute op))))))

(ert-deftest test-gptai-execute-deletion-op-not-project ()
  "Test gptai-execute with deletion op not project."
  (setq gptai--delete-confirmation nil)
  (with-temp-directory
   working-dir
   ;; NOT a project
   ;; (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (op (gptai-make-deletion-op
                 :filename filename)))
       (write-region "" nil filename)
       (should (file-exists-p filename))
       (should-error (gptai-execute op))))))

(ert-deftest test-gptai-execute-deletion-op-other-project ()
  "Test gptai-execute with deletion op not project."
  (setq gptai--delete-confirmation nil)
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (with-temp-directory
    target-dir
    (make-directory (expand-file-name ".git" target-dir))
    (with-temp-buffer
      (setq default-directory working-dir)
      (let ((working-buffer (current-buffer)))
        (with-temp-buffer
          (setq default-directory target-dir)
          (let* ((filename (make-temp-name (concat target-dir "/")))
                 (op (gptai-make-deletion-op
                      :filename filename)))
            (write-region "" nil filename)
            (should (file-exists-p filename))
            (with-current-buffer working-buffer
              (should-error (gptai-execute op))))))))))

(ert-deftest test-gptai--parse-suggestions ()
  "Test gptai--parse-suggestions"
  (let* ((input "

---

**OP** MODIFY `buffer1`
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

---

**OP** CREATE `filename1`
```
hello
world
```

---

**OP** DELETE `filename2`

---

lines remain
")
         (result (gptai--parse-suggestions input))
         (expect (list
                  (gptai-make-modification-op
                   :target "buffer1"
                   :replacements '(("hello" . "world")))
                  (gptai-make-creation-op :filename "filename1"
                                          :content "hello\nworld")
                  (gptai-make-deletion-op :filename "filename2"))))
    (should (equal result expect))))

(ert-deftest test-gptai--apply-suggestions ()
  "Test gptai--apply-suggestions"
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (insert "hello")
     (let* ((recentf-exclude '(".*"))
            (require-final-newline nil)
            (filename1 (make-temp-name (concat working-dir "/")))
            (filename2 (make-temp-name (concat working-dir "/")))
            (input
             (format "
---

**OP** MODIFY `%s`
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

---

**OP** CREATE `%s`
```
hello
world
```

---

**OP** DELETE `%s`

---

lines remain
"
                     (buffer-name) filename1 filename2)))
       (write-region "" nil filename2)
       (should-not (file-exists-p filename1))
       (should (file-exists-p filename2))
       (cl-letf (((symbol-function 'read-char-choice)
                  (lambda (&rest _)
                    ?y)))
         (should (gptai--apply-suggestions input))
         (should (equal (buffer-string) "world"))
         (should (file-exists-p filename1))
         (find-file filename1)
         (should (string= (buffer-string) "hello\nworld"))
         (kill-buffer)
         (should-not (file-exists-p filename2)))))))

(ert-deftest test-gptai-apply-last-suggestions ()
  "Test gptai--apply-suggestions"
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (insert "hello")
     (message "hh: %s" (buffer-name))
     (let* ((recentf-exclude '(".*"))
            (require-final-newline nil)
            (working-buffer (current-buffer))
            (filename1 (make-temp-name (concat working-dir "/")))
            (filename2 (make-temp-name (concat working-dir "/")))
            (response
             (format "

---

**OP** MODIFY `%s`
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

---

**OP** CREATE `%s`
```
hello
world
```

---

**OP** DELETE `%s`

---

lines remain
"
                     (buffer-name working-buffer) filename1 filename2)))
       (write-region "" nil filename2)
       (should-not (file-exists-p filename1))
       (should (file-exists-p filename2))
       (with-temp-buffer
         (add-text-properties
          0 (length response)
          `(gptel response gptai ,working-buffer)
          response)
         (insert response)
         (cl-letf (((symbol-function 'read-char-choice)
                    (lambda (&rest _)
                      ?y)))
           (should (gptai-apply-last-suggestions))
           (should (equal (with-current-buffer working-buffer (buffer-string))
                          "world"))
           (should (file-exists-p filename1))
           (find-file filename1)
           (should (string= (buffer-string) "hello\nworld"))
           (kill-buffer)
           (should-not (file-exists-p filename2))))))))
