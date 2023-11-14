# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- The menu now works across frames.
- Clean-up now works across frames.
- Visibility check now works across frames.
- Claiming from other frames now quits the window on the other frame.

### Changed

- The menu how always uses `display-buffer-use-some-window`.

### Fixed

- Explainer for trait `buffer-file-name` now mentions the actual
  issue.
- Calling `pop-to-buffer` with NORECORD flag is now honored.
- Memories are only created for non-nil keys.
- Hash table tests are now done using `equal` to avoid duplicate
  subconsciousnesses.

## [v0.8.3]

### Added

- Custom variable `partial-recall-memorable-traits` that holds a list
  of functions to call during `partial-recall--flush` to save moment
  from being flushed.
- Function `partial-recall--gracedp` used as the default only such
  trait above. The function has the previous implementation as well as
  a check if the moment is below `partial-recall-reclaim-min-age`.
- New command `partial-recall-forget-some` that works like
  `kill-some-buffers` but instead prompts to forget moments.
- Functions in `partial-recall-meaningful-traits` can now have
  property `partial-recall-non-meaningful-explainer`: a string
  explaining why the trait was infringed. This is used for new command
  `partial-recall-explain-omission` and in the mode-line lighter.

### Changed

- Custom variable `partial-recall-traits` was renamed to
  `partial-recall-meaningful-traits`.
- Filtering based on `partial-recall-filter` is now a trait of
  `partial-recall-meaningful-traits`.
- Lighter now uses "!" to indicate buffer that is not meanignful and
  "?" to indicate a meaningful but not yet remembered buffer.

### Fixed

- Concentration only breaks once.

## [v0.8.2]

### Added

- Variable `partial-recall-auto-switch` can now be set to symbol
  `prompt` to prompt the user whether a switch should happen.
- The face used to highlight the `partial-recall-log-prefix` can now
  be customized.
- The lighter now shows additional information and provides quick
  access to implanting moments and the entire command map. It is
  constructed from new custom variable `partial-recall-lighter` that
  itself comprises several mode line constructs. Variable
  `partial-recall-mode-lighter` is replaced by
  `partial-recall-lighter-prefix`.
- Key `s` in `partial-recall-menu` buffers is now bound to toggling
  the inclusion of the subconscious.

## [v0.8.1]

### Changed

- Slot `update-count` has been renamed to `focus` (as well as various
  private functions).
- Concentration now instead increases the moment's focus.
- `partial-recall-auto-implant-threshold` has been increased to
  account for the change in how concentration works.
- The `focus` no longer is increased if the moment is already
  implanted.
- Trait `partial-recall--not-in-view-mode-p` to not consider buffers
  that were opened in `view-mode` meaningful.

### Removed

- `partial-recall-min-focus` has been removed (now folded into
  `partial-recall-auto-implant-threshold`.

### Fixed

- `partial-recall--suppress` no longer inserts moments that point to
  an already suppressed buffer.
- If a killed buffer was the target of the last focus, the focus is
  cleared.

## [v0.8.0]

### Added

- Enabling `partial-recall` now starts a timer that will check the
  current moment every minute. If the same moment is encountered
  enough times, it is made permanent. The required repeats are
  governed by variable `partial-recall-min-focus`.
- `git-rebase-todo` files are ignored by default.
- Menu uses new indicator for moments that are implanted but were
  never updated.
- Commands `partial-recall-next` and `partial-recall-previous` to
  quickly switch buffers bound to `n` and `p`.
- New keymap to easily repeat new navigation commands.
- In `partial-recall-menu` key `u` is now bound to new command
  `partial-recall-menu-unmark.`

### Changed

- By default the reclaim minimum age is now *below* the maximum age.
- !! `partial-recall-buffer-limit` was renamed to
  `partial-recall-memory-size`.

## [v0.7.1]

### Changed

- Several performance improvements.

### Fixed

- A menu opened with `include-subsconscious` now also honors this when
  reverting the buffer.

## [v0.7.0]

### Added

- New command `partial-recall-meld` to combine two memories into one
  and optionally closing the source memory. Bound to `u` in the
  command map.
- New custom variable `partial-recall-traits`. This is a list of
  functions used to determine whether a buffer is meaningful, i.e.
  whether it should be scheduled. By default it only includes
  `buffer-file-name`. The introduction of this variable means that
  functionality is no longer tied to file buffers if so desired.
- Command `partial-recall-buffer-specs` that gives information as to a
  buffer's characteristics.
- Command `partial-recall-memory-specs` that does the same thing for
  memories.
- Hooks `partial-recall-permanence-change-hook`,
  `partial-recall-probe-hook` and `partial-recall-after-insert-hook`.
- Command `partial-recall-flush` to remove all moments that are
  neither permanent nor were ever updated (excluding the currently
  visited buffer).

### Changed

- Implanting moments no longer increases their update count. In fact,
  excising will reset it.
- Forgetting buffers now probes the memory.
- Insertion is now done using `partial-recall--insert`.

### Fixed

- Probing memories now only extends if there was no resizing.

### Removed

- Functions `partial-recall--reality-buffer-p` and
  `partial-recall--reality-owns-buffer-p` in favor of using
  `partial-recall--memory-buffer-p`.

## [v0.6.3]

### Added

- Custom variable `partial-recall-auto-switch`. When switching to a
  buffer that is mapped in another memory and wouldn't be reclaimed, a
  switch to its memory's tab is performed.

### Changed

- `partial-reall-{menu,lift}` are now bound to `m` and `l`.

### Fixed

- `partial-recall-remember` no longer provides the current buffer as
  initial input unless it is meaningful or it was called with prefix
  argument.
- Completions now provide initial input whenever it makes sense.
- `partial-recall-lift` now switches to the lifted buffer.
- When API commands switch to a buffer, they are no longer scheduled.
- Lifting moments no longer increases their update count.

## [v0.6.2]

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
