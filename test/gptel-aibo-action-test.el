(require 'ert)
(require 'gptel-aibo-action)

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.

The temporary directory will be deleted on exit/error."
  `(let ((,dir (make-temp-file "gptel-aibo-test-" t)))
    (unwind-protect
        (progn ,@body)
      (delete-directory ,dir t))))

(ert-deftest test-gptel-aibo--create-op-parser ()
  (should (equal (gptel-aibo--create-op-parser "MODIFY")
                 (gptel-aibo-make-mod-op-parser)))
  (should (equal (gptel-aibo--create-op-parser "CREATE")
                 (gptel-aibo-make-creation-op-parser)))
  (should (equal (gptel-aibo--create-op-parser "DELETE")
                 (gptel-aibo-make-del-op-parser)))
  (should-not (gptel-aibo--create-op-parser "?")))

(ert-deftest test-gptel-aibo-parse-mod-op-full-content ()
  "Test gptel-aibo-parse-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

```
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-mod-op
                     :target target
                     :full-content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-parse-mod-op-replacements ()
  "Test gptel-aibo-parse-op with modification op with full-content."
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
         (parser (gptel-aibo-make-mod-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-mod-op
                     :target target
                     :replacements '(("hello" . "world") ("hi" . "ha")))))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-mod-op-parser-invalid ()
  "Test gptel-aibo-parse-op with modification op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
*SEARCH*
```
hello
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo-parse-creation-op ()
  "Test gptel-aibo-parse-op with creation op."
  (let* ((filename "filename1")
         (input "
 \n

```
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-creation-op-parser))
         (result
          (gptel-aibo-parse-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-creation-op
                     :filename filename
                     :content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-parse-creation-op-invalid ()
  "Test gptel-aibo-parse-op with creation op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
not begin with code block
```
hello
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-creation-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo--parse-code-block ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-code-block-end-fence-with-space ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
``` \nlines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-code-block-end-fence-with-others ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
```NO!
lines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-code-block lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo-parse-del-op ()
  "Test gptel-aibo-parse-op with deletion op."
  (let* ((filename "filename1")
         (lines '("lines remain"))
         (parser (gptel-aibo-make-del-op-parser))
         (result
          (gptel-aibo-parse-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-del-op
                     :filename filename)))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-execute-mod-op-full-content ()
  "Test gptel-aibo-execute with modification op with full-content."
  (with-temp-buffer
    (insert "hello")
    (let* ((buffer-name (buffer-name))
           (full-content "world")
           (op (gptel-aibo-make-mod-op
                :target buffer-name
                :full-content full-content)))
      (gptel-aibo-execute op)
      (with-current-buffer buffer-name
        (should (string= (buffer-string) full-content))))))

(ert-deftest test-gptel-aibo-execute-mod-op-replacements ()
  "Test gptel-aibo-execute with modification op with replacements."
  (with-temp-buffer
    (insert "hello
hello
world")
    (let* ((buffer-name (buffer-name))
           (op (gptel-aibo-make-mod-op
                :target buffer-name
                :replacements '(("hello" . "hi") ("world" . "earth")))))
      (gptel-aibo-execute op)
      (with-current-buffer buffer-name
        (should (string= (buffer-string) "hi
hello
earth"))))))

(ert-deftest test-gptel-aibo-execute-mod-op-full-content-not-exist ()
  "Test gptel-aibo-execute with modification op with full-content not exit."
  (let* ((buffer-name (make-temp-name "*not-exist-"))
         (full-content "world")
         (op (gptel-aibo-make-mod-op
              :target buffer-name
              :full-content full-content)))
    (should-error (gptel-aibo-execute op) :type 'error)))

(ert-deftest test-gptel-aibo-execute-mod-op-full-content-not-project ()
  "Test gptel-aibo-execute with modification op with full-content not project."
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
                (op (gptel-aibo-make-mod-op
                     :target (buffer-name target-buffer)
                     :full-content full-content)))
           (with-current-buffer working-buffer
             (should-error (gptel-aibo-execute op) :type 'error))))))))

(ert-deftest test-gptel-aibo-execute-mod-op-full-content-other-project ()
  "Test gptel-aibo-execute with modification op with full-content in other project."
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
                 (op (gptel-aibo-make-mod-op
                      :target (buffer-name target-buffer)
                      :full-content full-content)))
            (with-current-buffer working-buffer
              (should-error (gptel-aibo-execute op) :type 'error)))))))))

(ert-deftest test-gptel-aibo-execute-mod-op-full-content-in-project ()
  "Test gptel-aibo-execute with modification op with full-content in project."
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
                (op (gptel-aibo-make-mod-op
                     :target (buffer-name target-buffer)
                     :full-content full-content)))
           (with-current-buffer working-buffer
             (gptel-aibo-execute op)
             (with-current-buffer target-buffer
               (should (string= (buffer-string) full-content))))))))))

(ert-deftest test-gptel-aibo-execute-mod-op-replacements-in-project ()
  "Test gptel-aibo-execute with modification op with full-content in project."
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
                (op (gptel-aibo-make-mod-op
                     :target (buffer-name target-buffer)
                     :replacements '(("hello" . "hi") ("world" . "earth")))))
           (with-current-buffer working-buffer
             (gptel-aibo-execute op)
             (with-current-buffer target-buffer
               (should (string= (buffer-string) "hi
hello
earth"))))))))))

(ert-deftest test-gptel-aibo-execute-creation-op ()
  "Test gptel-aibo-execute with creation op."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((recentf-exclude '(".*"))
            (require-final-newline nil)
            (filename (make-temp-name (concat working-dir "/")))
            (content "hello")
            (op (gptel-aibo-make-creation-op
                 :filename filename
                 :content content)))
       (gptel-aibo-execute op)
       (find-file filename)
       (should (string= (buffer-string) content))
       (kill-buffer)))))

(ert-deftest test-gptel-aibo-execute-creation-op-not-project ()
  "Test gptel-aibo-execute with creation op not project."
  (with-temp-directory
   working-dir
   ;; NOT a project
   ;; (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (content "hello")
            (op (gptel-aibo-make-creation-op
                 :filename filename
                 :content content)))
       (should-error (gptel-aibo-execute op))))))

(ert-deftest test-gptel-aibo-execute-creation-op-other-project ()
  "Test gptel-aibo-execute with creation op other project."
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
             (op (gptel-aibo-make-creation-op
                  :filename filename
                  :content content)))
        (should-error (gptel-aibo-execute op)))))))

(ert-deftest test-gptel-aibo-execute-del-op ()
  "Test gpti-execute with deletion op."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (op (gptel-aibo-make-del-op
                 :filename filename)))
       (setq gptel-aibo--delete-confirmation nil)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?y)))
         (gptel-aibo-execute op))
       (should-not (file-exists-p filename))

       (should-not gptel-aibo--delete-confirmation)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?n)))
         (gptel-aibo-execute op))
       (should (file-exists-p filename))

       (should-not gptel-aibo--delete-confirmation)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?a)))
         (gptel-aibo-execute op))
       (should-not (file-exists-p filename))

       (should (eq gptel-aibo--delete-confirmation 'always))

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice)
                  (lambda (&rest _)
                    (error "Should not call read-char-choice when always"))))
         (gptel-aibo-execute op))
       (should-not (file-exists-p filename))

       (setq gptel-aibo--delete-confirmation nil)

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _)
                                                        ?N)))
         (gptel-aibo-execute op))
       (should (file-exists-p filename))

       (should (eq gptel-aibo--delete-confirmation 'never))

       (write-region "" nil filename)
       (should (file-exists-p filename))
       (cl-letf (((symbol-function 'read-char-choice)
                  (lambda (&rest _)
                    (error "Should not call read-char-choice when never"))))
         (gptel-aibo-execute op))
       (should (file-exists-p filename))))))

(ert-deftest test-gptel-aibo-execute-del-not-exist ()
  "Test gptel-aibo-execute with deletion op not exist."
  (with-temp-directory
   working-dir
   (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (op (gptel-aibo-make-del-op
                 :filename filename)))
       (setq gptel-aibo--delete-confirmation nil)
       (should-not (file-exists-p filename))
       (should-error (gptel-aibo-execute op))))))

(ert-deftest test-gptel-aibo-execute-del-op-not-project ()
  "Test gptel-aibo-execute with deletion op not project."
  (setq gptel-aibo--delete-confirmation nil)
  (with-temp-directory
   working-dir
   ;; NOT a project
   ;; (make-directory (expand-file-name ".git" working-dir))
   (setq default-directory working-dir)
   (with-temp-buffer
     (let* ((filename (make-temp-name (concat working-dir "/")))
            (op (gptel-aibo-make-del-op
                 :filename filename)))
       (write-region "" nil filename)
       (should (file-exists-p filename))
       (should-error (gptel-aibo-execute op))))))

(ert-deftest test-gptel-aibo-execute-del-op-other-project ()
  "Test gptel-aibo-execute with deletion op not project."
  (setq gptel-aibo--delete-confirmation nil)
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
                 (op (gptel-aibo-make-del-op
                      :filename filename)))
            (write-region "" nil filename)
            (should (file-exists-p filename))
            (with-current-buffer working-buffer
              (should-error (gptel-aibo-execute op))))))))))

(ert-deftest test-gptel-aibo--parse-suggestions ()
  "Test gptel-aibo--parse-suggestions"
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
         (result (gptel-aibo--parse-suggestions input))
         (expect (list
                  (gptel-aibo-make-mod-op
                   :target "buffer1"
                   :replacements '(("hello" . "world")))
                  (gptel-aibo-make-creation-op :filename "filename1"
                                          :content "hello\nworld")
                  (gptel-aibo-make-del-op :filename "filename2"))))
    (should (equal result expect))))

(ert-deftest test-gptel-aibo--apply-suggestions ()
  "Test gptel-aibo--apply-suggestions"
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
         (should (gptel-aibo--apply-suggestions input))
         (should (equal (buffer-string) "world"))
         (should (file-exists-p filename1))
         (find-file filename1)
         (should (string= (buffer-string) "hello\nworld"))
         (kill-buffer)
         (should-not (file-exists-p filename2)))))))

(ert-deftest test-gptel-aibo-apply-last-suggestions ()
  "Test gptel-aibo--apply-suggestions"
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
           (should (gptel-aibo-apply-last-suggestions))
           (should (equal (with-current-buffer working-buffer (buffer-string))
                          "world"))
           (should (file-exists-p filename1))
           (find-file filename1)
           (should (string= (buffer-string) "hello\nworld"))
           (kill-buffer)
           (should-not (file-exists-p filename2))))))))
