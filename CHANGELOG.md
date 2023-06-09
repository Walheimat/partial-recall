# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `partial-recall--repr` to print a readable representation of a
  memory or moment instance.
- Custom variable `partial-recall-handle-delay` that determines the
  amount of seconds to wait until a buffer is handled. The default it
  3 seconds. `partial-recall--handle-buffer` will now check whether
  the buffer of the selected window is the scheduled buffer before
  handling it. This makes sure that only buffers are handled that
  weren't just switch to by accident. If the minibuffer was entered
  before handling, the check is made against the previously selected
  window's buffer.
- `partial-recall-remember` as a user-facing command. It was added
  because the user can now end up with a buffer that's not in memory.
  It provides a list of unmapped file buffers providing the current
  buffer as initial buffer. The selected buffer is switched to.
- `partial-recall--forget` will now delete any window displaying the
  buffer to forget.
- `partial-recall--debug` to log debug messages.

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
