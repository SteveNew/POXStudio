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

## 2020-02-25
### Added
- Added support for reading/deflating .AG files (Hero X), into users AppData temp directory as .pox files and read from there. Linked Layered resources is not handle correct since original relative paths are not resolved - yet?
- Honoring the INI datas TransparentColor and EditorImage values.
- Transparent color can be changed.
### Changed
- German translation gone for now.
- Zoom/Scaling is now 1-2-4-8. 

## 2019-12-22
### Added
- Added some draft INI data documentation

## 2019-07-22
### Fixed
- A bit more of saving - have done a 30 min. play with a complete set of recreated chapter 1 POXs
### Added
- pf555 decoding support - since a few files has this - will save only as pf565
- Static resources image width mod 4 rule added on load
### Removed
- POX files are not "RLE-optimized" for now - so still contains unneeded 02 00 00 00 00 and 03

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