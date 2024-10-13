# Changelog

## [0.13.1](https://github.com/Walheimat/partial-recall/compare/v0.13.0...v0.13.1) (2024-10-13)

### Bug Fixes

* **pop-to-logs:** set read-only, use view-buffer-other-window ([dc293e3](https://github.com/Walheimat/partial-recall/commit/dc293e349396229e002c909a8cb65d5edd2b557d))

### Changes

* **concentration,implanting:** rework focusing ([2bff462](https://github.com/Walheimat/partial-recall/commit/2bff4624ba2e4f925dc2638667b9f821dc41846a))
* **logging:** call set-buffer-modified-p after insert ([bf09a58](https://github.com/Walheimat/partial-recall/commit/bf09a58ded93353cf3f23a25e8aa5211fef80803))
* **logging:** disable echoing logs by default, log by default ([bc93408](https://github.com/Walheimat/partial-recall/commit/bc9340862114f19f605b2df47e924993b9077017))
* **logging:** include "log" in buffer name ([c46ecc8](https://github.com/Walheimat/partial-recall/commit/c46ecc83e318fac58d8f17dcbf6a1cb70158ab17))

## [0.13.0](https://github.com/Walheimat/partial-recall/compare/v0.12.0...v0.13.0) (2024-09-07)

### Features

* **remember:** remember multiple buffers ([29c001b](https://github.com/Walheimat/partial-recall/commit/29c001bdca3f4213bd5db2df85291b4ad67bc4b7))

### Bug Fixes

* **banish:** bind to z in command map ([5898e3d](https://github.com/Walheimat/partial-recall/commit/5898e3d31507d8a376f13f00af55387990d021a0))
* **x,consult:** remove autoload cookie ([483f20f](https://github.com/Walheimat/partial-recall/commit/483f20f712585d62f025b83a58873884480fe096))

### Changes

* **consult:** allow customizing narrow key ([5f58ffd](https://github.com/Walheimat/partial-recall/commit/5f58ffd5ba97ded385e833cb021ac4a9d5ea0134))
* **logging:** don't log to message buffer ([cd34819](https://github.com/Walheimat/partial-recall/commit/cd34819c3d0e318eccb6cfe21dbadda821b6b09f))
* **logging:** log to buffer, command to pop to it ([1a90f7b](https://github.com/Walheimat/partial-recall/commit/1a90f7b4930365bf0e116380e71b3be26d20221c))
* **menu:** use pop-to-buffer to display, make configurable ([3e87f04](https://github.com/Walheimat/partial-recall/commit/3e87f0456f1ac086d8056dbd6427f2e7a2f03b01))

## [0.12.0](https://github.com/Walheimat/partial-recall/compare/v0.11.0...v0.12.0) (2024-08-07)

### Features

* **hygiene:** add new behavior ([4238dfe](https://github.com/Walheimat/partial-recall/commit/4238dfe78ddc96377f4c32b030a0fe563355454a))
* **hygiene:** nag about full memories ([05b174b](https://github.com/Walheimat/partial-recall/commit/05b174b0e60cc5e8173226ecc0c2724b5cb057bd))

### Bug Fixes

* **flush:** don't probe memory unless moments were suppressed ([b9aecee](https://github.com/Walheimat/partial-recall/commit/b9aeceedc0e91e4cfca8c2f1828994658ca95f82))

### Changes

* **flush:** parameter to not flush visible moments ([8ace07c](https://github.com/Walheimat/partial-recall/commit/8ace07c795a965e9d0cc80329ec61e22ad916bf8))
* **flush:** return count of flushed moments ([95a3e1b](https://github.com/Walheimat/partial-recall/commit/95a3e1bffa72e001d0096397682a65068da6c410))
* **hygiene:** allow customizing function used to message ([555b3bb](https://github.com/Walheimat/partial-recall/commit/555b3bb8536ad38800cdb8ea1934212d2173f9ec))
* **hygiene:** call flush with ignore-visible ([5ad6240](https://github.com/Walheimat/partial-recall/commit/5ad6240e1c691ff5ed79569a2edb2b287cc66f03))
* **hygiene:** don't select window while minibuffer is active ([614e9ed](https://github.com/Walheimat/partial-recall/commit/614e9ed39d4363c0c0362a94052e16b11b5cd569))
* **hygiene:** remove nagging function in favor of message ([fbd85a9](https://github.com/Walheimat/partial-recall/commit/fbd85a9442181a46b538264492d9d3e5d10fdd0c))
* **hygiene:** use partial-recall-log for nagging ([c69b798](https://github.com/Walheimat/partial-recall/commit/c69b798e07a97166aa86910a24a8f470df4e529d))
* **logging:** only log visibility while handling ([fd0a464](https://github.com/Walheimat/partial-recall/commit/fd0a464f4fbdda7a9f8dceacd60d15e48d32dfd6))
* **logging:** state reason for reinserting moments ([8765220](https://github.com/Walheimat/partial-recall/commit/876522090b8ed3df7c0af1487f435de45c2a2f5e))

## [0.11.0](https://github.com/Walheimat/partial-recall/compare/v0.10.1...v0.11.0) (2024-06-29)

### Features

* **forgetting:** allow banishing buffers ([fdec137](https://github.com/Walheimat/partial-recall/commit/fdec1375cadb76c27dca10d968e9eb68a7ef7bc2))
* **menu,core:** spin out buffers into a new memory ([3ab6903](https://github.com/Walheimat/partial-recall/commit/3ab6903b757dee868bace3ebe325a7dd4e1283f7))
* **reactions:** support tab-bar-undo-close-tab ([813e9d9](https://github.com/Walheimat/partial-recall/commit/813e9d995b077e533e8e202780477c851233f5d2))
* rework how the subconscious works ([af5361c](https://github.com/Walheimat/partial-recall/commit/af5361c31f490e1f8f964e76e7515e2adddc8222))

### Bug Fixes

* **schedule:** allow passing buffer name ([c408dab](https://github.com/Walheimat/partial-recall/commit/c408daba5ac99f9c3018b41c3e4b4ded3cc94bd8))
* **traits:** consider view-mode being called after switching to buffer ([f8cecaa](https://github.com/Walheimat/partial-recall/commit/f8cecaaccfa2543ac348d7b0d4254860b3c55bab))

### Changes

* **explain-omission:** reference manual inclusion ([e4868be](https://github.com/Walheimat/partial-recall/commit/e4868be35b832d9d930523f6a29dc2c112af0c0d))
* **lighter:** indicate if memory near capacity with contrast ([122a8cc](https://github.com/Walheimat/partial-recall/commit/122a8cc358b5abe9b08167253eeb899e549bbf8d))

## [0.10.1](https://github.com/Walheimat/partial-recall/compare/v0.10.0...v0.10.1) (2024-02-18)


### Bug Fixes

* **cask:** add new package ([5bd4498](https://github.com/Walheimat/partial-recall/commit/5bd44982291884e3d7edc1e35af10fa12882c5a7))
* **concentration:** rebuild previous change check ([1294475](https://github.com/Walheimat/partial-recall/commit/129447524589aa770394b1fb302efff24818c828))
* **plasticity:** add autoload cookie for mode ([b46f92e](https://github.com/Walheimat/partial-recall/commit/b46f92e5e30148d671bc2d2b2c31198592698b3c))


### Changes

* **concentration:** faint focus ([c2c745b](https://github.com/Walheimat/partial-recall/commit/c2c745bba87c39808ce49841acc1830f576a67f6))
* **plasticity:** also extend if youngest is very young ([94f0abf](https://github.com/Walheimat/partial-recall/commit/94f0abf74379d47d442bdd147cc20f5d673d120f))

## [0.10.0](https://github.com/Walheimat/partial-recall/compare/v0.9.3...v0.10.0) (2024-02-08)


### Features

* **history:** add event structure, history methods ([21526a4](https://github.com/Walheimat/partial-recall/commit/21526a455242c14d769e9b3e63352c153e4aa182))
* **history:** add partial-recall-retrieve ([75dab0f](https://github.com/Walheimat/partial-recall/commit/75dab0f4a5bcafc9886992f46f9bbee70b81e69c))
* **structures:** add history slots ([ad5d714](https://github.com/Walheimat/partial-recall/commit/ad5d7148d811d7dd157fa6f963568baf9e89a7e5))
* **subconscious:** increased subconscious ([e761851](https://github.com/Walheimat/partial-recall/commit/e76185139e802028f77435947bf7cf6db3e05227))


### Bug Fixes

* **completion:** signal user-error when completing null ([65cb082](https://github.com/Walheimat/partial-recall/commit/65cb08293a29c7ea30916556290d94dc2ede4078))

## [0.9.3](https://github.com/Walheimat/partial-recall/compare/v0.9.2...v0.9.3) (2024-01-28)


### Bug Fixes

* **concentration:** defer when there is no reality ([01bae54](https://github.com/Walheimat/partial-recall/commit/01bae54d7fdcfe2fa0e5e3f74f76a7083c76ed65))


### Features

* **ci:** add release job ([db188ff](https://github.com/Walheimat/partial-recall/commit/db188fff2e9f1189d9db84e17f41076d439bd1e0))
* **faces:** make emphasis inherit from font-lock-type-face ([1e950ab](https://github.com/Walheimat/partial-recall/commit/1e950ab0c65561c0f7f3795843dd406b2642694f))

## [0.9.2]

### Added

- Convenience function `partial-recall-current-moment`.
- Lighter now shows focus using `partial-recall-graph` if fleeting.
- Concentration is now deferred until a meaningful buffer can be its
  target.
- Lighter indicates foreign moments.
- The menu now also indicates intermediate moments using a contrasting
  face.
- The menu now shows whether a buffer is modified.

### Changed

- Moments are now auto-implanted if their focus is equal or greater
  than the required focus.

### Fixed

- Concentration breaks on foreign moments.

## [0.9.1]

### Added

- Display of additional moment and memory info can now be toggled by
  customizing `partial-recall-lighter-show-info`.
- The documentation of the core functions has been updated and
  extended.
- `partial-recall-menu` now highlights buffers that are at the brink
  of being forgotten.
- The lighter now shows the memory size when it hasn't grown past its
  original size in the help echo.
- The lighter now shows the focus of a moment as a percentage
  (provided `partial-recall-auto-implant` is set).

### Changed

- Many private functions (and underlying structures) were renamed.

### Removed

- Variable `partial-recall-lighter` is no longer customizable. See
  above.

## [v0.9.0]

### Added

- Command `partial-recall-switch-to-buffer-other-window`, bound to `4`
  in the command map.
- Switching tabs now restarts concentration.

### Changed

- The lighter now shows the memory level using a box.
- Logging variables have been merged into `partial-recall-log` that
  can be either `nil`, `1`, or `0`.
- Auto-implanting variables have been merged into
  `partial-recall-auto-implant`, that is either `nil` or the focus
  value.
- Thresholds were reduced to single
  `partial-recall-intermediate-term`. It can be set to an integer or
  nil to disable its use. Moment that fall below or exceed this
  threshold are handled differently (switched to or reclaimed).
- Hooks `partial-recall-{probe,permanence-change,after-insert}-hook`
  now use `run-hook-with-args` and pass sensible arguments to their
  functions (the memory, the moment and permanence and the moment
  respectively).

## [v0.8.6]

### Added

- Jumping to a register with a window configuration is now handled for
  auto-switching.
- Changing window configurations with `winner` may switch memories.
- Command `partial-recall-reject` to push moments from the current
  memory to another.
- `partial-recall-intensities` governing by how much a moment's focus
  increases. Swapping now also has an effect on focus. The user may
  also give `suppress` an intensity.

### Changed

- Buffer source `partial-recall-x-consult-buffer-source` now sets
  `:hidden t` to avoid duplicate results during `consult-buffer`. It
  uses `u` for narrowing now.
- Command `partial-recall-switch-to-buffer` no longer automatically
  "neglects" the buffer; it only does so when called with a prefix
  argument.
- Value of `partial-recall-auto-implant-threshold` was doubled to
  account for actions giving more focus.

### Fixed

- All completion for buffers now uses `read-buffer` and sets
  completion table (meaning sorting works and you don't need to set up
  `marginalia` for this package).

## [v0.8.5]

### Changed

- `partial-recall-menu` no longer uses `read-symbol-shorthands`.
- `partial-recall-extensions` was renamed to `partial-recall-x`. All
  provided functions and variables also use the `partial-recall-x-`
  prefix.

### Fixed

- Buffers not having a name no longer breaks `partial-recall-menu`.
- `partial-recall--forget` now checks
  `partial-recall--mapped-buffer-p` instead of
  `partial-recall--meaningful-buffer-p` as the latter may fluctuate
  and it was possible to add meaningless buffers without the ability
  to remove them.

## [v0.8.4]

### Added

- The menu now works across frames.
- Clean-up now works across frames.
- Visibility check now works across frames.
- Claiming from other frames now quits the window on the other frame.

### Changed

- The menu how always uses `display-buffer-use-some-window`.
- The thresholds for `partial-recall-max-age` (30=>20) and
  `partial-recall-reclaim-min-age` (15=>10) were reduced.

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
