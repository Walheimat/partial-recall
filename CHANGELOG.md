# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- `partial-recall--reinforce` now only reinforces when the visited
  buffer moves close to the end of the ring. Instead of destroying the
  moment, it is simply re-inserted while growing the ring if needed.
  Afterwards timestamp and update count (see below) are increased.
- `partial-recall--moment` structs now have field `update-count` (for
  future debug purposes).

## [v0.3.0]

### Added

- `partial-recall--handle-buffer` now calls new
  `partial-recall--recollect` instead of `--reclaim`. This will either
  call `--reclaim` if the buffer is not part of the "reality" or new
  function `--reinfroce` that will recreate the moment. This makes
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
  function `--reinfroce` that will recreate the moment. This makes
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
