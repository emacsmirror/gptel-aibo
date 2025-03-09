(require 'ert)
(require 'gptel-aibo-summon)

(ert-deftest test-gptel-aibo--summon-parse-response ()
  "Test gptel-aibo--summon-parse-response"

  (let* ((response "hello")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) 'error)))

  (let* ((response "
*SEARCH*
```
hello
```
")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) 'error)))

  (let* ((response "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```
remain")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) (cons "hello" "world")))
    (should (equal (cadr result) nil)))

  (let* ((response "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

### Nearby Modification

bad")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) (cons "hello" "world")))
    (should (equal (cadr result) nil)))

  (let* ((response "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

### Nearby Modification

*SEARCH*
```
good
```
*REPLACE*
```
morning
```

remain")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) (cons "hello" "world")))
    (should (equal (cadr result)
                   '(("good" . "morning"))))))

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
                 (point) response)))
