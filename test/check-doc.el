;;
;; This file should be load in the project root directory
;;

(setq gptel-aibo-checkdoc-error nil)

(defun checkdoc-in-batch (file)
  (message "Run checkdoc %s..." file)
  (letrec ((err nil)
           (checkdoc-create-error-function
            (lambda (text start end &optional unfixable)
              (let ((errmsg
                     (format "%s:%s %s"
                             (file-name-nondirectory file)
                             (count-lines (point-min) (or start (point-min)))
                             text)))
                (push errmsg gptel-aibo-checkdoc-error)
                (message "%s" errmsg)))))
    (checkdoc-file file)
    (not err)))

(let ((files (directory-files "." nil "\\.el$")))
  (dolist (file files)
    (unless
        (let ((gptel-aibo)
              (sentence-end-double-space nil))
          (checkdoc-in-batch (expand-file-name file)))
      (setq gptel-aibo-checkdoc-error t))))

(when gptel-aibo-checkdoc-error
  (message "\nCheckdoc FAILED")
  (dolist (errmsg (nreverse gptel-aibo-checkdoc-error))
    (message "%s" errmsg))
  (kill-emacs 1))
