;;
;; This file should be load in the project root directory
;;

(add-to-list 'load-path ".")

(load "test/gptel-aibo-context-test.el")
(load "test/gptel-aibo-action-test.el")

(ert-run-tests-batch-and-exit)
