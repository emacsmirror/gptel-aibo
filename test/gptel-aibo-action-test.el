(require 'ert)
(require 'gptel-aibo-action)

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.

The temporary directory will be deleted on exit/error."
  `(let ((,dir (make-temp-file "gptel-aibo-test-" t)))
    (unwind-protect
        (progn ,@body)
      (delete-directory ,dir t))))

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

