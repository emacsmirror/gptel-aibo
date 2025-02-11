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

(ert-deftest test-gptel-aibo--working-buffer-info ()
  (with-temp-buffer
    (insert "hello")
    (let ((content "Content:
```fundamental
hello
```
")
          (fragment-before-line "Fragment before the cursor:\n")
          (fragment-after-line "Fragment after the cursor:\n"))

      (let* ((gptel-aibo-max-buffer-size 10240)
             (info (gptel-aibo--working-buffer-info)))
        (should (string-match-p (regexp-quote content) info))
        (should (string-match-p (regexp-quote fragment-before-line) info))
        (should (string-match-p (regexp-quote fragment-after-line) info)))

      (let* ((gptel-aibo-max-buffer-size 2)
             (info (gptel-aibo--working-buffer-info)))
        (should-not (string-match-p (regexp-quote content) info))
        (should (string-match-p (regexp-quote fragment-before-line) info))
        (should (string-match-p (regexp-quote fragment-after-line) info))))))

(ert-deftest test-gptel-aibo--project-buffers-info ()
  (with-temp-directory
   dir
   (with-multi-temp-buffers-attatch
    ((buf1 (expand-file-name "file1.txt" dir))
     (buf2 (expand-file-name "file2.txt" dir)))
    (with-current-buffer buf2
      (insert "hello"))
    (let ((content "Content:
```fundamental
hello
```
"))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _)
                   (list 'transient default-directory))))

        (let* ((gptel-aibo-max-buffer-size 10240)
               (info (gptel-aibo--project-buffers-info buf1)))
          (should (string-match-p (regexp-quote content) info)))

        (let* ((gptel-aibo-max-buffer-size 2)
               (info (gptel-aibo--project-buffers-info buf1)))
          ;; no outline, empty info
          (should (equal info "")))

        (with-current-buffer buf2
          (setq imenu-create-index-function
                (lambda ()
                  (list
                   (cons "f" 1)
                   ))))
        (let* ((gptel-aibo-max-buffer-size 2)
               (info (gptel-aibo--project-buffers-info buf1))
               (outline "Outline:
- f

"))
          (should (string-match-p (regexp-quote outline) info))))))))

(ert-deftest test-gptel-aibo-max-buffer-count ()
  (with-temp-directory
   dir
   (with-multi-temp-buffers-attatch
    ((buf1 (expand-file-name "file1.txt" dir))
     (buf2 (expand-file-name "file2.txt" dir))
     (buf3 (expand-file-name "file3.txt" dir)))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _)
                 (list 'transient default-directory))))

      (let* ((gptel-aibo-max-buffer-count 2)
             (info (gptel-aibo--project-buffers-info buf1)))
        (should (equal
                 (cl-count-if
                  (lambda (line) (string-prefix-p "Filepath: " line))
                  (split-string info "\n"))
                 2)))

      (let* ((gptel-aibo-max-buffer-count 1)
             (info (gptel-aibo--project-buffers-info buf1)))
        (should (equal
                 (cl-count-if
                  (lambda (line) (string-prefix-p "Filepath: " line))
                  (split-string info "\n"))
                 1)))))))

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

(ert-deftest test-gptel-aibo--make-code-block ()
  (should (equal (gptel-aibo--make-code-block "aa`bb")
                 "```\naa`bb\n```"))
  (should (equal (gptel-aibo--make-code-block "aa```bb")
                 "````\naa```bb\n````")))

(ert-deftest test-gptel-aibo--make-code-fence ()
  (should (equal (gptel-aibo--make-code-fence "aa`bb") "```"))
  (should (equal (gptel-aibo--make-code-fence "aa```bb") "````"))
  (should (equal (gptel-aibo--make-code-fence "aa`````bb") "``````")))
