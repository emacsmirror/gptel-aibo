# gptel-aibo

**gptel-aibo** is an AI writing assistant system built on top of **gptel**.
It helps users create and manage content in Emacs, including code, documentation,
and even novels. Essentially, it sends the content (or a portion of it) that
you're currently working on as you talk to the LLM, Once a response is received,
you can apply it using the command `gptai-apply-last-suggestions`
(bound to `C-c !`).

The term "aibo," meaning "partner," is currently ambiguous—it could refer to
gptel’s partner, or the user’s.

## Installation
This package requires [gptel](https://github.com/karthink/gptel) and access to any
LLM through it. See its README for details on setting up your LLM environment.

Since this package is developed based on the Doom Emacs `gptel` package, it uses
a very recent version of `gptel`. If you're not using Doom Emacs, it's recommended
to install `gptel` using `straight.el` or the built-in `package-vc-install`.

### Doom Emacs
In your `packages.el`, add this line:
```elisp
(package! gptel-aibo :recipe (:host github :repo "dolmens/gptel-aibo"))
```
**Note**: In the `gptel-aibo` interface, it overrides the `flycheck` keybinding
`C-c !` (you generally won't need to use `flycheck` in the `gptel-aibo` interface,
especially with the shortcut key). Therefore, if you want to configure further
using `use-package!`, make sure `gptel-aibo` is loaded after `flycheck`.
```elisp
(use-package! gptel-aibo
  :after (gptel flycheck))
```

## Usage
With the `gptai` interactive command, you can open or select an existing
`gptel-aibo` console, which is a `markdown` page with the `gptai` minor mode
enabled, an extension of `gptel-mode`.

To get started, open your file, move the cursor to the section you're working on,
and then switch to the gptai console. There, you can talk to the LLM and refer to
"this function", "this class", "this file", etc. `gptel-aibo` will automatically
send the content you're currently working on.

Once a response is received, you can apply it using the command 
`gptai-apply-last-suggestions` (bound to `C-c !`).

### Completion at point
`gptel-aibo` provides a quick interaction command, `gptai-complete-at-point`, 
which automatically inserts relevant content at the current position based on
the context.
For example, after writing a function comment, you can use this single command
to generate the corresponding code. Use TAB or Enter to accept, and other keys
to discard.

Currently, this command doesn't have a default keybinding, as it's difficult to
find a suitable key that won't upset anyone. In my case, I have bound it to
`Command i`.
```elisp
(use-package! gptel-aibo
  :after (gptel flycheck)
  :config
  (map! "s-i" #'gptai-complete-at-point))
```

## Samples
### Query the logic, request additional cases
![Starship segment](assets/aibo-segment-case-800-10.gif)

### Refactor a sample project
![Sample project refactor](assets/aibo-refactor-sample-800-10.gif)

## Customization
- `gptai--max-buffer-size`

  The size limit for the working buffer (the buffer you are currently working on)
  that is automatically sent to the LLM.
  
  If the buffer exceeds this size, only a portion of the content around the cursor
  (typically a function or class) will be sent. 
  
  The default value is 16000.

- `gptai--max-project-buffer-size`
  
  The size limit for other buffers within the same project as your working buffer
  that are automatically sent to the LLM.
  
  If a buffer exceeds this size, only the
  outline will be sent.
  
  The default value is 16000.

- `gptai--max-project-buffer-count`
  
  The maximum number of buffers within the same project as your working buffer
  that are automatically sent to the LLM.
  
  The default value is 2.

## security issues
1. The file path and content of the current working buffer, as well as buffers
from the same project, may be sent to the LLM.
2. Only projects are allowed to create and delete files under the project root.
A confirmation prompt will be shown when attempting to delete.
3. Modifications to the buffer will be saved. While you can revert changes using
Emacs’ undo system, it’s best to place your project under version control.
Additionally, files will be deleted upon confirmation and cannot be undone by
emacs itself.
