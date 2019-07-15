# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- Include Layers manipulation code.
- Include new "Tabar" layer
- Add support for LL:LayeredFramesToBack
- Add support for action keywords - RANDOM, LOOP, END ...
- Add support for trigger frame
- Data validation - ini vs. rle data.
- Ini data/property editors.
- Better multi-language support - replace TLang with proper GNU gettext.
- Import bmps and/or ini data.
- Investigate .gif and .dae import - if needed.
- Export/Import sprite-sheets if needed - for colour manipulation.

## 2019-07-15
### Fixed
- A bit of saving - so enabled that again - needs heavy test run

## 2019-07-11
### Added
- 64-bit support on decode - so pre-builds on Linux and Win64 - released
- Preliminary layer manipulation support
### Removed
- Decode pointer math - since whole file is read
### Disabled
- Initial save code disable since the encoding does not yet match the old 100%

## 2019-06-30
### Added
- Initial commit of preliminary build - missing save code done in experimental build
- Added assets, license and documentation