(require 'ert)
(require 'gptel-aibo-planner)

(ert-deftest test-gptel-aibo--apply-suggestions ()
  "Test gptel-aibo--apply-suggestions."
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
         (with-temp-buffer
           (insert-file-contents filename1)
           (should (string= (buffer-string) "hello\nworld")))
         (should-not (file-exists-p filename2)))))))

(ert-deftest test-gptel-aibo--apply-suggestions-dry-run-fail ()
  "Test gptel-aibo--apply-suggestions fail in dry-run.

Nothing should be changed."
   (with-temp-buffer
     (insert "hello\nworld\n")
     (let* ((buf-name (buffer-name))
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
hi
```

---

**OP** MODIFY `%s`
*SEARCH*
```
noworld
```
*REPLACE*
```
earth
```

---

lines remain
"
                     buf-name buf-name)))
       (should (gptel-aibo--apply-suggestions input))
       (should (equal (buffer-string) "hello\nworld\n")))))

(ert-deftest test-gptel-aibo-apply-last-suggestions ()
  "Test gptel-aibo-apply-last-suggestions."
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
           (with-temp-buffer
             (insert-file-contents filename1)
             (should (string= (buffer-string) "hello\nworld")))
           (should-not (file-exists-p filename2))))))))
