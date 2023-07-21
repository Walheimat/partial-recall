# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Addeed

- Custom variable `partial-recall-log-prefix` to do just that. The
  default is "PR".
- `partial-recall-filter` to filter out certain buffers/files. This
  currently only includes the file `COMMIT_EDITMSG`.

### Removed

- `partial-recall-reinforce` was removed from the public API as
  there's no scenario where this would be useful. In the command map,
  `partial-recall-remember` now uses the letter `r`.

### Changed

- `partial-recall--{log,debug}` now use `partial-recall--repr` to
  print moments and memories and prepends `partial-recall-log-prefix`.
- `partial-recall-reclaim` now switches to the reclaimed buffer.

### Fixed

- `partial-recall-menu` now has a minimum length for tab and buffer
  columns.
- `pop-to-buffer` is now also advised so that `find-file` is covered
  when file already has a buffer. The `find-file` hook was removed.
- `partial-recall-switch-buffer` no longer prompts with the current
  buffer as initial input.

## [v0.6.1]

### Fixed

- `partial-recall--clean-up-buffer` now calls `quit-window` instead of
  `delete-window` which could mean deleting the only window.

## [v0.6.0]

### Added

- `partial-recall--repr` to print a readable representation of a
  memory or moment instance.
- Custom variable `partial-recall-handle-delay` that determines the
  amount of seconds to wait until a buffer is handled. The default it
  3 seconds. `partial-recall--handle-buffer` will now check whether
  the scheduled buffer remains visible before handling it. This makes
  sure that only buffers are handled that weren't just switched to by
  accident. If the minibuffer was entered before handling, the check
  is made against the previously selected window's buffer.
- Custom variable `partial-recall-record-triggers`. This a list of
  commands that will trigger recording the previously selected window.
  It contains a single command by default, `consult-buffer` (because
  of its previewing).
- `partial-recall-remember` as a user-facing command. It was added
  because the user can now end up with a buffer that's not in memory.
  It provides a list of unmapped file buffers providing the current
  buffer as initial buffer. The selected buffer is switched to. If
  called with a prefix argument, all buffers can be remembered.
- `partial-recall--forget` will now delete any window displaying the
  buffer to forget.
- Custom variables `partial-recall-log` and
  `partial-recall-log-level`.
- `partial-recall--debug` to log debug messages.
- The docstrings of actions have been expanded upon.

### Changed

- `partial-recall-menu` now displays timestamps that are older than 12
  hours by printing a date.
- `partial-recall-menu` now displays the update count and whether it
  is implanted using block elements. The implanted state is indicated
  by face color. The actual values are displayed in the columns help
  echo.
- `partial-recall-{max-age, reclaim-min-age}` have been increased to
  10 and 30 minutes respectively.
- The list of mapped file buffers can now include those stored in the
  subconscious by passing the appropriate argument.
- Implanted moments can no longer be reclaimed.

### Fixed

- `partial-recall-menu-execute` no longer deletes entries which led to
  buffers to be forgotten to be ignored.
- Forgetting moments now suppresses them.
- All implanted moments that are on the verge of being forgotten are
  now reinforced when remembering a new moment.
- `partial-recall-toggle-logging` in favor of using custom variables.

## [v0.5.0]

### Added

- A separate memory called `partial-recall--subconscious` exists now
  as a sink of moments forgotten from other memories. If a moment is
  removed from the subconscious, its buffer is killed if
  `partial-recall-repress` is t. When remembering, moments are
  "lifted" from the subconscious if they exist in it.
- Lifting can also be performed using `partial-recall-lift`.
- Subconscious moments can be included and interacted with in
  `partial-recall-menu` if it is called with a prefix argument.
- Moments that have been updated past a certain limit are now
  auto-implanted, if `partial-recall-auto-implant` is `t` and their
  update count has exceeded `partial-recall-auto-implant-threshold`.

### Changed

- `partial-recall--reclaim` no longer recreates a moment, instead it
  is swapped from the previous memory to reality.
- `partial-recall--consult-buffer-source` was moved to new subpackage
  `partial-recall-extensions`.
- The column width is now adapted based on the longest buffer and tab
  name.
- Code was refactored again into more sensible headings.
- Completion for reality buffers now sets the current buffer as
  initial input if possible.

## [v0.4.0]

### Added

- New sub-package `partial-recall-menu` that acts similar to
  `list-buffers`. You can forget, reclaim, implant and excise (see
  below), and visit buffers from there.
- Function `partial-recall--tab` to get the tab associated
  with a memory (see below).
- Functions `--reinforce` and `--reclaim` now log their actions if new
  variable `partial-recall--log` is `t`.
- Memories that have grown while remembering may shrink again after
  forgetting.
- Moments can now be implanted using `partial-recall-implant`. This
  will make sure they are updated instead when would otherwise be
  removed.

### Changed

- The default buffer limit was halved (20 -> 10).
- All user-facing commands now use appropriate completion instead of
  applying only to the current buffer.
- Memories are now created with the key added to the tab for
  cross-reference (see above).
- `partial-recall--reinforce` now only reinforces when the visited
  buffer would be subject to removal. Instead of destroying the
  moment, it is simply re-inserted while growing the ring if needed.
  Afterwards timestamp and update count (see below) are increased.
- `partial-recall--moment` structs now have field `update-count` (for
  debug purposes).
- User-facing `partial-recall-reinforce` will now force `--reinforce`.

## [v0.3.0]

### Added

- `partial-recall--handle-buffer` now calls new
  `partial-recall--recollect` instead of `--reclaim`. This will either
  call `--reclaim` if the buffer is not part of the "reality" or new
  function `--reinforce` that will recreate the moment. This makes
  sure that often-visited buffers aren't forgotten just because they
  were visited early on.

### Changed

- Switched to using `dinghy`.

## [v0.2.0]

### Added

- `partial-recall-steal` to take another memory's buffer.
- Mode key-map.
- `partial-recall--handle-buffer` now calls new
  `partial-recall--recollect` instead of `--reclaim`. This will either
  call `--reclaim` if the buffer is not part of the "reality" or new
  function `--reinforce` that will recreate the moment. This makes
  sure that often-visited buffers aren't forgotten just because they
  were visited early on.

### Changed

- The defaults for custom variables have been vastly increased.
- Switched to using `dinghy`.

### Fixed

- `partial-recall` will fix up the original tab of the current frame
  after a delay.

## [v0.1.0]

Initial release of the package that was spliced out of my own
configuration.
