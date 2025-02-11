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

Fragment before the cursor:
```
hello
```

Fragment after the cursor:
(cursor is at the end of the buffer)


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

Fragment before the cursor:
```
hello
```

Fragment after the cursor:
(cursor is at the end of the buffer)

" (buffer-name)))
           (gptel-aibo-max-buffer-size 10240)
           (info (gptel-aibo--working-buffer-info)))
      (should (equal info expect)))

    (let* ((expect (format "Current working buffer: `%s`

Filepath: (not associated with a file)

Fragment before the cursor:
```
hello
```

Fragment after the cursor:
(cursor is at the end of the buffer)

" (buffer-name)))
           (gptel-aibo-max-buffer-size 2)
           (info (gptel-aibo--working-buffer-info)))
      (should (equal info expect)))))

(ert-deftest test-gptel-aibo--fragment-info ()
  (with-temp-buffer
    (insert "hello")
    (let* ((expect "Fragment before the cursor:
```
hello
```

Fragment after the cursor:
(cursor is at the end of the buffer)

")
           (gptel-aibo-max-buffer-size 10240)
           (gptel-aibo-max-fragment-size 1024)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    (goto-char (point-min))
    (let* ((expect "Fragment before the cursor:
(cursor is at the beginning of the buffer)

Fragment after the cursor:
```
hello
```

")
           (gptel-aibo-max-buffer-size 10240)
           (gptel-aibo-max-fragment-size 1024)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    (goto-char 3)
    (let* ((expect "Fragment before the cursor:
```
he
```

Fragment after the cursor:
```
llo
```

")
           (gptel-aibo-max-buffer-size 10240)
           (gptel-aibo-max-fragment-size 2)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    ;; Trimmed, but expanded
    (let* ((expect "Fragment before the cursor:
```
he
```

Fragment after the cursor:
```
llo
```

")
           (gptel-aibo-max-buffer-size 10240)
           (gptel-aibo-max-fragment-size 2)
           (gptel-aibo-max-fragment-expand 1024)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))

    ;; Trimmed
    (let* ((expect "Fragment before the cursor:
...
```
e
```

Fragment after the cursor:
```
l
```
...

")
           (gptel-aibo-max-buffer-size 10240)
           (gptel-aibo-max-fragment-size 2)
           (gptel-aibo-max-fragment-expand nil)
           (info (gptel-aibo--fragment-info)))
      (should (equal info expect)))
    ))

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

(ert-deftest test-gptel-aibo--fragment ()
  (with-temp-buffer
    (insert "void f1() {
// f1
}

void f2() {
// f2
}

void f3() {
// f3
}
")
    (c-mode)

    (goto-char (point-min))
    (should (equal (gptel-aibo--fragment 1024)
                   '(""
                     .
                     "void f1() {\n// f1\n}\n")))

    (forward-line 5)
    (should (equal (gptel-aibo--fragment 1024)
                   '("void f2() {\n"
                     .
                     "// f2\n}\n")))

    (goto-char (point-max))
    (should (equal (gptel-aibo--fragment 1024)
                   '("void f3() {\n// f3\n}\n"
                     .
                     "")))))

(ert-deftest test-gptel-aibo--fragment-unique ()
  (with-temp-buffer
    (insert "class Dog {
void hello() {

}
};

class Cat {
void hello() {

}
};

void hello() {

}
")
    (c++-mode)

    (goto-char (point-min))

    (forward-line 2)
    (should (equal (gptel-aibo--fragment 1024)
                   '("void hello() {\n"
                     .
                     "\n}\n")))

    (forward-line 6)
    (should (equal (gptel-aibo--fragment 1024)
                   '("class Cat {\nvoid hello() {\n"
                     .
                     "\n}\n")))))


(ert-deftest test-gptel-aibo--fragment-trimming-after ()
  (with-temp-buffer
    (let ((f1 "void f1() {  }\n")
          (f2 "void f2() {
//lines
//lines
//lines
//lines
}
"))
      (insert f1)
      (insert f2)
      (c++-mode)

      (goto-char (point-min))
      (forward-line 1)
      (should (equal (gptel-aibo--fragment 1024)
                     (cons f1 f2)))

      ;; Before is small, trim after
      (should (equal (gptel-aibo--fragment 25)
                    (cons f1 "void f2() ")))

      ;; Trim both
      (should (equal (gptel-aibo--fragment 20)
                    (cons "f1() {  }\n" "void f2() "))))))

(ert-deftest test-gptel-aibo--fragment-trimming-before ()
  (with-temp-buffer
    (let ((f1 "void f1() {  }\n")
          (f2 "void f2() {
//lines
//lines
//lines
//lines
}
"))
      (insert f2)
      (insert f1)
      (c++-mode)

      (goto-char (point-min))
      (forward-line 6)
      (should (equal (gptel-aibo--fragment 1024)
                     (cons f2 f1)))

      ;; After is small, trim before
      (should (equal (gptel-aibo--fragment 25)
                    (cons "//lines\n}\n" f1))))))

(ert-deftest test-gptel-aibo--fragment-trimming-expand ()
  (with-temp-buffer
    (let ((f1 "void f1() {  }\n")
          (f2 "void f2() {
//lines
//lines
//lines
//lines
}
"))
      (insert f1)
      (insert f2)
      (c++-mode)

      (goto-char (point-min))
      (forward-line 1)
      (should (equal (gptel-aibo--fragment 1024)
                     (cons f1 f2)))

      ;; Before is small, trim after, expand
      (should (equal (gptel-aibo--fragment 25 20)
                    (cons f1 "void f2() {")))

      ;; Trim both, expand
      (should (equal (gptel-aibo--fragment 20 20)
                    (cons "void f1() {  }\n" "void f2() {"))))))


(ert-deftest test-gptel-aibo--make-code-block ()
  (should (equal (gptel-aibo--make-code-block "aa`bb")
                 "```\naa`bb\n```"))
  (should (equal (gptel-aibo--make-code-block "aa```bb")
                 "````\naa```bb\n````")))

(ert-deftest test-gptel-aibo--make-code-fence ()
  (should (equal (gptel-aibo--make-code-fence "aa`bb") "```"))
  (should (equal (gptel-aibo--make-code-fence "aa```bb") "````"))
  (should (equal (gptel-aibo--make-code-fence "aa`````bb") "``````")))
