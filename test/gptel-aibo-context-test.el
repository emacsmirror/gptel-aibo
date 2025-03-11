(require 'ert)
(require 'gptel-aibo-context)
(require 'cl-lib)

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.

The temporary directory will be deleted on exit/error."
  `(let ((,dir (make-temp-file "gptel-aibo-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(defmacro with-multi-temp-directories (vars &rest body)
  "Generate temporary directories, bind them to VARS, and run BODY.

VARS is a list of symbols to bind to the directory paths.
Deletes the directories after BODY execution."
  (let ((bindings (mapcar (lambda (var)
                            `(,var (make-temp-file "gptel-aibo-test-" t)))
                          vars)))
    `(let ,bindings
       (unwind-protect
           (progn ,@body)
         ;; Cleanup - delete the directories
         (mapc (lambda (dir) (when (and dir (file-exists-p dir))
                               (delete-directory dir t)))
               (list ,@vars))))))

(defmacro with-multi-temp-buffers-attatch (buf-file-pairs &rest body)
  "Execute BODY with BUF-FILE-PAIRS as temp buffers bound to symbols.

BUF-FILE-PAIRS is a list of pairs: the first element is the buffer symbol
and the second is the associated file path."
  (let ((bindings
         (mapcar (lambda (pair)
                   (let ((buffer-sym (car pair))
                         (file-path (cadr pair)))
                     `(,buffer-sym
                       (let ((buffer (generate-new-buffer "gptel-aibo-temp")))
                         (with-current-buffer buffer
                           (setq buffer-file-name ,file-path)
                           (setq default-directory
                                 (file-name-directory ,file-path)))
                         buffer))))
                 buf-file-pairs)))
    `(let ,bindings
       (unwind-protect
           (progn ,@body)
         (mapc (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (set-buffer-modified-p nil)
                     (kill-buffer buf))))
               (list ,@(mapcar #'car buf-file-pairs)))))))


(ert-deftest test-gptel-aibo--project-buffers ()
  (with-multi-temp-directories
   (dir1 dir2)
   (with-multi-temp-buffers-attatch
    ((buf1 (expand-file-name "file1.txt" dir1))
     (buf2 (expand-file-name "file2.txt" dir1))
     (buf3 (expand-file-name "file3.txt" dir1))
     (buf4 (expand-file-name "file4.txt" dir2)))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _)
                 (list 'transient default-directory))))
      (should (equal (gptel-aibo--project-buffers buf1)
                     (list buf2 buf3)))

      (should (equal (gptel-aibo--project-buffers buf2)
                     (list buf1 buf3)))

      ;; Single
      (should-not (gptel-aibo--project-buffers buf4)))

    ;; No project
    (should-not (gptel-aibo--project-buffers buf1)))))

(ert-deftest test-gptel-aibo-context-info ()
  (with-temp-directory
   dir
   (with-multi-temp-buffers-attatch
    ((buf1 (expand-file-name "file1.txt" dir))
     (buf2 (expand-file-name "file2.txt" dir))
     (buf3 (expand-file-name "file3.txt" dir)))
    (with-current-buffer buf1
      (insert "hello"))
    (with-current-buffer buf2
      (insert "world"))
    (with-current-buffer buf3
      (insert "ok"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _)
                 (list 'transient default-directory))))

      (let* ((expect (format "Current working buffer: `%s`
Filepath: `%s`
Content:
```fundamental
hello
```
The cursor is positioned at the end of the buffer.

Other buffers in the same project:

`%s`:
Filepath: `%s`
Content:
```fundamental
world
```

`%s`:
Filepath: `%s`
Content:
```fundamental
ok
```


"
                             (buffer-name buf1)
                             (buffer-file-name buf1)
                             (buffer-name buf2)
                             (buffer-file-name buf2)
                             (buffer-name buf3)
                             (buffer-file-name buf3)))
             (gptel-aibo-max-buffer-size 10240)
             (gptel-aibo-max-buffer-count 2)
             (info (gptel-aibo-context-info buf1)))
        (should (equal info expect)))))))

(ert-deftest test-gptel-aibo--working-buffer-info ()
  (with-temp-buffer
    (insert "hello")
    (let* ((expect (format "Current working buffer: `%s`
Filepath: (not associated with a file)
Content:
```fundamental
hello
```
The cursor is positioned at the end of the buffer." (buffer-name)))
           (gptel-aibo-max-buffer-size 10240)
           (info (gptel-aibo--working-buffer-info)))
      (should (equal info expect)))

    (let* ((expect (format "Current working buffer: `%s`
Filepath: (not associated with a file)
Fragment around the cursor:
<<< TRUNCATED >>>
```fundamental
o
```
<<< END OF CONTENT >>>

The cursor is positioned at the end of the fragment." (buffer-name)))
           (gptel-aibo-max-buffer-size 2)
           (info (gptel-aibo--working-buffer-info)))
      (should (equal info expect)))))

(ert-deftest test-gptel-aibo--fragment-info ()
  (with-temp-buffer
    (insert "world")
    (let* ((expect "Filepath: (not associated with a file)
Fragment around the cursor:
```fundamental
world
```
The cursor is positioned at the end of the fragment.")
           (gptel-aibo-max-buffer-size 10240)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    (goto-char (point-min))
    (let* ((expect "Filepath: (not associated with a file)
Fragment around the cursor:
```fundamental
world
```
The cursor is positioned at the beginning of the fragment.")
           (gptel-aibo-max-buffer-size 10240)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    (goto-char 3)
    (let* ((expect "Filepath: (not associated with a file)
Fragment around the cursor:
```fundamental
world
```
The cursor is on the first line of the fragment, after `wo` and before `rld`.")
           (gptel-aibo-max-buffer-size 10240)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    (let* ((expect "Filepath: (not associated with a file)
Fragment around the cursor:
<<< TRUNCATED >>>
```fundamental
orl
```
<<< REMAINING OMITTED >>>

The cursor is on the first line of the fragment, after `o` and before `rl`.")
           (gptel-aibo-max-buffer-size 3)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))))

(ert-deftest test-gptel-aibo--project-buffers-info ()
  (with-temp-directory
   dir
   (with-multi-temp-buffers-attatch
    ((buf1 (expand-file-name "file1.txt" dir))
     (buf2 (expand-file-name "file2.txt" dir))
     (buf3 (expand-file-name "file3.txt" dir)))
    (with-current-buffer buf2
      (insert "hello"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _)
                 (list 'transient default-directory))))

      (let* ((expect (format "Other buffers in the same project:

`%s`:
Filepath: `%s`
Content:
```fundamental
hello
```

`%s`:
Filepath: `%s`
Content:
```fundamental

```


"
                             (buffer-name buf2)
                             (buffer-file-name buf2)
                             (buffer-name buf3)
                             (buffer-file-name buf3)))
             (gptel-aibo-max-buffer-size 10240)
             (gptel-aibo-max-buffer-count 2)
             (info (gptel-aibo--project-buffers-info buf1)))
        (should (equal info expect)))

      ;; Test buffer size limit
      ;; One buffer exceeds size, no outline, just skip the buffer
      (let* ((expect (format "Other buffers in the same project:

`%s`:
Filepath: `%s`
Content:
```fundamental

```


"
                             (buffer-name buf3)
                             (buffer-file-name buf3)))
             (gptel-aibo-max-buffer-size 2)
             (gptel-aibo-max-buffer-count 2)
             (info (gptel-aibo--project-buffers-info buf1)))
        (should (equal info expect)))

      (with-current-buffer buf2
        (setq imenu-create-index-function
              (lambda ()
                (list
                 (cons "f" 1)
                 ))))
      (let* ((expect (format "Other buffers in the same project:

`%s`:
Filepath: `%s`
Outline:
- f

`%s`:
Filepath: `%s`
Content:
```fundamental

```


"
                             (buffer-name buf2)
                             (buffer-file-name buf2)
                             (buffer-name buf3)
                             (buffer-file-name buf3)))
             (gptel-aibo-max-buffer-size 2)
             (gptel-aibo-max-buffer-count 2)
             (info (gptel-aibo--project-buffers-info buf1)))
        (should (equal info expect)))

      ;; Test buffer count limit
      (let* ((expect (format "Other buffers in the same project:

`%s`:
Filepath: `%s`
Content:
```fundamental
hello
```


"
                             (buffer-name buf2)
                             (buffer-file-name buf2)))
             (gptel-aibo-max-buffer-size 10240)
             (gptel-aibo-max-buffer-count 1)
             (info (gptel-aibo--project-buffers-info buf1)))
        (should (equal info expect)))))))

(ert-deftest test-gptel-aibo--make-code-block ()
  (should (equal (gptel-aibo--make-code-block "aa`bb")
                 "```\naa`bb\n```"))
  (should (equal (gptel-aibo--make-code-block "aa```bb")
                 "````\naa```bb\n````")))

(ert-deftest test-gptel-aibo--make-code-fence ()
  (should (equal (gptel-aibo--make-code-fence "aa`bb") "```"))
  (should (equal (gptel-aibo--make-code-fence "aa```bb") "````"))
  (should (equal (gptel-aibo--make-code-fence "aa`````bb") "``````")))

(ert-deftest test-gptel-aibo--cursor-on-first-line-info ()
  "Test suite for gptel-aibo--cursor-on-first-line-info function."
  ;; Use with-temp-buffer to avoid manual cleanup
  (with-temp-buffer
    ;; 1. Test case: Cursor at the beginning of the first line
    (insert "This is the first line.")
    (goto-char (point-min)) ;; Move cursor to the beginning of the first line
    (should
     (equal
      (gptel-aibo--cursor-on-first-line-info)
      "The cursor is positioned at the beginning of the first line of the buffer."))

    ;; 2. Test case: Cursor at the end of the first line
    (erase-buffer)
    (insert "This is the first line.")
    (goto-char (point-max)) ;; Move cursor to the end of the first line
    (should
     (equal
      (gptel-aibo--cursor-on-first-line-info)
      "The cursor is positioned at the end of the first line of the buffer."))

    ;; 3. Test case: Cursor in the middle of the first line
    (erase-buffer)
    (insert "This is the first line.")
    (goto-char (+ (point-min) 5)) ;; Move cursor to the middle of the first line
    (should
     (equal
      (gptel-aibo--cursor-on-first-line-info)
      "The cursor is on the first line of the buffer, after `This ` and before `is the first line.`."))))

(ert-deftest test-gptel-aibo--cursor-on-last-line-info ()
  "Test suite for gptel-aibo--cursor-on-last-line-info function."
  ;; Use with-temp-buffer to avoid manual cleanup
  (with-temp-buffer
    ;; 1. Test case: Cursor at the beginning of the last line
    (insert "This is the first line.\n")
    (insert "This is the second line.\n")
    (goto-char (point-min)) ;; Move cursor to the beginning of the buffer
    (forward-line 1)       ;; Move to the second line
    (forward-line 1)       ;; Move to the last line
    (goto-char (line-beginning-position)) ;; Move cursor to the beginning of the last line
    (should
     (equal
      (gptel-aibo--cursor-on-last-line-info)
      "The cursor is positioned at the beginning of the last line of the buffer."))

    ;; 2. Test case: Cursor at the end of the last line
    (erase-buffer)
    (insert "This is the first line.\n")
    (insert "This is the second line.\n")
    (goto-char (point-min)) ;; Move cursor to the beginning of the buffer
    (forward-line 1)       ;; Move to the last line
    (goto-char (line-end-position)) ;; Move cursor to the end of the last line
    (should
     (equal
      (gptel-aibo--cursor-on-last-line-info)
      "The cursor is positioned at the end of the last line of the buffer."))

    ;; 3. Test case: Cursor in the middle of the last line
    (erase-buffer)
    (insert "This is the first line.\n")
    (insert "This is the second line.\n")
    (goto-char (point-min)) ;; Move cursor to the beginning of the buffer
    (forward-line 1)       ;; Move to the last line
    (goto-char (+ (line-beginning-position) 5)) ;; Move cursor to the middle of the last line
    (should
     (equal
      (gptel-aibo--cursor-on-last-line-info)
      "The cursor is on the last line of the buffer, after `This ` and before `is the second line.`."))))

(ert-deftest test-gptel-aibo--cursor-line-info ()
  "Test suite for gptel-aibo--cursor-line-info function."
  (with-temp-buffer
    ;; 1. Test case: Cursor at the beginning of the line
    (insert "This is a test line.")
    (goto-char (point-min))
    (should
     (equal
      (gptel-aibo--cursor-line-info)
      "The cursor is positioned at the beginning of the following line:
```fundamental
This is a test line.
```"))

    ;; 2. Test case: Cursor at the end of the line
    (goto-char (point-max))
    (should
     (equal
      (gptel-aibo--cursor-line-info)
      "The cursor is positioned at the end of the following line:
```fundamental
This is a test line.
```"))

    ;; 3. Test case: Cursor in the middle of the line
    (goto-char (+ (point-min) 5)) ;; Move cursor after "This "
    (should
     (equal
      (gptel-aibo--cursor-line-info)
      "The cursor is on the following line:
```fundamental
This is a test line.
```
and is positioned after `This ` and before `is a test line.`."))

    ;; 4. Test case: Cursor in a longer line
    (erase-buffer)
    (let ((long-line "This is a very long test line with a lot of text to exceed the twenty character limit."))
      (insert long-line)
      (goto-char (+ (point-min) 30)) ;; Move cursor somewhere in the middle
      (should
       (equal
        (gptel-aibo--cursor-line-info)
        (format "The cursor is on the following line:
```fundamental
%s
```
and is positioned after `%s` and before `%s`."
                long-line
                (substring long-line 10 30)
                (substring long-line 30 50)))))))


(ert-deftest test-gptel-aibo--cursor-snippet-info ()
  "Test suite for gptel-aibo--cursor-snippet-info function."
  (with-temp-buffer
    ;; 1. Test case: Cursor at the beginning of the last line
    (insert "This is the first line.\n")
    (insert "This is the second line.\n")
    (insert "This is the last line.")
    (goto-char (point-min))
    (forward-line 2)  ;; Move cursor to the beginning of the last line
    (should
     (equal
      (gptel-aibo--cursor-snippet-info)
      "The cursor is positioned at the beginning of the last line in the snippet below:
```fundamental
This is the second line.
This is the last line.
```"))

    ;; 2. Test case: Cursor at the end of the last line
    (erase-buffer)
    (insert "This is the first line.\n")
    (insert "This is the second line.\n")
    (insert "This is the last line.")
    (goto-char (point-min))
    (forward-line 2)  ;; Move cursor to the last line
    (goto-char (line-end-position))  ;; Move cursor to the end of the last line
    (should
     (equal
      (gptel-aibo--cursor-snippet-info)
      "The cursor is positioned at the end of the last line in the snippet below:
```fundamental
This is the second line.
This is the last line.
```"))

    ;; 3. Test case: Cursor in the middle of the last line
    (erase-buffer)
    (insert "This is the first line.\n")
    (insert "This is the second line.\n")
    (insert "This is the last line.")
    (goto-char (point-min))
    (forward-line 2)  ;; Move cursor to the last line
    (goto-char (+ (line-beginning-position) 5)) ;; Move cursor to the middle of the last line
    (should
     (equal
      (gptel-aibo--cursor-snippet-info)
      "The cursor is on the last line in the snippet below:
```fundamental
This is the second line.
This is the last line.
```
and is positioned after `This ` and before `is the last line.`."
))))

(ert-deftest test-gptel-aibo--cursor-position-info ()
  "Test suite for gptel-aibo--cursor-position-info function."
  (with-temp-buffer
    ;; 1. Cursor at the beginning of the buffer
    (should (equal (gptel-aibo--cursor-position-info)
                   "The cursor is positioned at the beginning of the buffer."))

    ;; 2. Cursor at the end of the buffer
    (insert "Test content")
    (goto-char (point-max))
    (should (equal (gptel-aibo--cursor-position-info)
                   "The cursor is positioned at the end of the buffer."))

    ;; 3. Cursor on the first line (delegates to gptel-aibo--cursor-on-first-line-info)
    (erase-buffer)
    (insert "First line content\nSecond line content")
    (goto-char (1+ (point-min)))
    (should (equal (gptel-aibo--cursor-position-info)
                   (gptel-aibo--cursor-on-first-line-info)))

    ;; 4. Cursor on the last line (delegates to gptel-aibo--cursor-on-last-line-info)
    (goto-char (1- (point-max)))
    (should (equal (gptel-aibo--cursor-position-info)
                   (gptel-aibo--cursor-on-last-line-info)))

    ;; 5. Cursor on a distinct line (delegates to gptel-aibo--cursor-line-info)
    (erase-buffer)
    (insert "first line\nUnique long line 12345\nAnother line")
    (goto-char (point-min))
    (forward-line)
    (should (equal (gptel-aibo--cursor-position-info)
                   (gptel-aibo--cursor-line-info)))

    ;; 6. Cursor on a non-distinct line (delegates to gptel-aibo--cursor-snippet-info)
    (erase-buffer)
    (insert "Repeated line\nRepeated line\nRepeated line")
    (goto-char (point-min))
    (forward-line)
    (should (equal (gptel-aibo--cursor-position-info)
                   (gptel-aibo--cursor-snippet-info)))))


(ert-deftest test-gptel-aibo--cursor-snippet-info-in-fragment ()
  "Test suite for gptel-aibo--cursor-snippet-info-in-fragment function."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\n")

    ;; 1. Cursor at the beginning of a line within the fragment range
    (goto-char (point-min))
    (forward-line)
    (should
     (equal
      (gptel-aibo--cursor-snippet-info-in-fragment 2 12)
      (concat
       "The cursor is positioned at the beginning of the last line in the snippet below:
```fundamental
ine 1
Line
```")))

    ;; 2. Cursor at the end of a line within the fragment range
    (forward-char 4)
    (should
     (equal
      (gptel-aibo--cursor-snippet-info-in-fragment 2 12)
      (concat "The cursor is positioned at the end of the last line in the snippet below:
```fundamental
ine 1
Line
```")))

    ;; 3. Cursor in the middle of a line within the fragment range
    (let ((before (gptel-aibo--make-inline-code-block "Line "))
          (after (gptel-aibo--make-inline-code-block "2")))
      (should
       (equal
        (gptel-aibo--cursor-snippet-info-in-fragment 2 14)
        "The cursor is on the last line in the snippet below:
```fundamental
ine 1
Line 2
```
and is positioned after `Line` and before ` 2`.")))


    ;; 4. Testing `point-min` and `point-max` constraints (restricting to line 2 only)
    (should
     (equal
      (gptel-aibo--cursor-snippet-info-in-fragment 8 14)
      "The cursor is on the last line in the snippet below:
```fundamental
Line 2
```
and is positioned after `Line` and before ` 2`."))))

(ert-deftest test-gptel-aibo--cursor-position-info-in-fragment ()
  "Test `gptel-aibo--cursor-position-info-in-fragment` function."
  (with-temp-buffer
    (insert "Line 1
Line 2
Line 3 unique and long enough
Line 4
Line long enough but not unique
Line long enough but not unique
Line 5
")

    ;; Define a fragment, avoiding using `point-min` and `point-max`
    (let ((start (save-excursion
                   (goto-char (point-min))
                   (forward-line 1)
                   (1+ (point))))
          (end (save-excursion
                 (goto-char (point-min))
                 (forward-line 6)
                 (1- (line-end-position)))))

      ;; 1. Cursor at the start of the fragment
      (goto-char start)
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     "The cursor is positioned at the beginning of the fragment."))

      ;; 2. Cursor at the end of the fragment
      (goto-char end)
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     "The cursor is positioned at the end of the fragment."))

      ;; 3. Cursor on the first line of the fragment but not at the beginning
      (goto-char (1+ start))
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     (gptel-aibo--cursor-on-first-line-info-in-fragment start end)))

      ;; 4. Cursor on the last line of the fragment
      (goto-char (1- end))
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     (gptel-aibo--cursor-on-last-line-info-in-fragment start end)))

      ;; 5. Cursor on a unique and long enough line
      (goto-char start)
      (forward-line 1)
      (forward-char 4)
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     (gptel-aibo--cursor-line-info)))

      ;; 6. Cursor on a short line
      (forward-line 1)
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     (gptel-aibo--cursor-snippet-info-in-fragment start end)))

      ;; 7. Cursor on non-unique line
      (forward-line 1)
      (should (equal (gptel-aibo--cursor-position-info-in-fragment start end)
                     (gptel-aibo--cursor-snippet-info-in-fragment start end))))))
