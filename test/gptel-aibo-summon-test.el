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
                 (current-buffer) (point) response)))
