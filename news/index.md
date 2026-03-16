# Changelog

## ILSAmerge 1.4.0

CRAN release: 2026-03-11

- Added support for LANA 2023.
- Added maxtime argument to
  [`ILSAready()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md).
- Added support for ICCS 2022.

#### Remaining issues

- No unzip of PISA and TALIS downloads.

## ILSAmerge 1.3.8

CRAN release: 2025-04-10

- Solved an issue with
  [`mistoNAs()`](https://dopatendo.github.io/ILSAmerge/reference/mistoNAs.md)
  when haven was not loaded.
- Solved an issue for weird behavior of
  [`mistoNAs()`](https://dopatendo.github.io/ILSAmerge/reference/mistoNAs.md)
  with ICILS 2013.
- Solved an issue with
  [`ILSAready()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md)
  when filetype is not declared.
- Fixed a bug in
  [`untibble()`](https://dopatendo.github.io/ILSAmerge/reference/untibble.md).
- Fixed a bug in
  [`addSchools()`](https://dopatendo.github.io/ILSAmerge/reference/addSchools.md).

## ILSAmerge 1.3.6

CRAN release: 2025-02-12

- Added support for TIMSS 2023.
- Added support for ICILS 2020 Teacher Panel.
- Added support for ICILS 2023.
- Fixed bug in
  [`justload()`](https://dopatendo.github.io/ILSAmerge/reference/justload.md).
- Fixed bug in
  [`readILSA()`](https://dopatendo.github.io/ILSAmerge/reference/readILSA.md).

## ILSAmerge 1.3.5

CRAN release: 2025-01-08

- Fixed an issue for saving ‘SPSS’ files.
- Added new functions for tibble management:
  [`asthistibble()`](https://dopatendo.github.io/ILSAmerge/reference/asthistibble.md),
  [`get.atr()`](https://dopatendo.github.io/ILSAmerge/reference/get.atr.md),
  [`get.nas()`](https://dopatendo.github.io/ILSAmerge/reference/get.atr.md),
  [`get.varlab()`](https://dopatendo.github.io/ILSAmerge/reference/get.atr.md),
  [`mistoNAs()`](https://dopatendo.github.io/ILSAmerge/reference/mistoNAs.md),
  [`untibble()`](https://dopatendo.github.io/ILSAmerge/reference/untibble.md),
  and
  [`whichcol()`](https://dopatendo.github.io/ILSAmerge/reference/whichcol.md).

## ILSAmerge 1.3.0

CRAN release: 2024-11-26

- Added
  [`ILSAready()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md)
  and
  [`ILSAreadylocal()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md)
  as wrappers for downloading, merging, combining respondents, and
  renaming.

## ILSAmerge 1.2.5

CRAN release: 2024-11-21

- Added
  [`combineStudents()`](https://dopatendo.github.io/ILSAmerge/reference/combineStudents.md)
  for combining achievement and background questionnaires.
- Added
  [`addSchools()`](https://dopatendo.github.io/ILSAmerge/reference/addSchools.md)
  for adding school data.
- Added labels to countries and creates variables with country codes and
  country names.

## ILSAmerge 1.0.5

- Added
  [`ILSArename()`](https://dopatendo.github.io/ILSAmerge/reference/ILSArename.md)
  for creating comprehensible file names.
- Fixed a problem caused by PIRLS 2021 having an empty dataset.
- Fixed a problem with merging dates for TIMSS 2011.
- Fixed a problem originated from PIRLS 2001 with its multiple
  capitalization styles.

## ILSAmerge 1.0.1

- Added extensive new documentation.
- Added
  [`availableILSA()`](https://dopatendo.github.io/ILSAmerge/reference/availableILSA.md)
  to check which ILSA can be downloaded.

## ILSAmerge 1.0.0

CRAN release: 2024-11-08

- CRAN submission.
- Added PISA and TALIS to
  [`ILSAdownload()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAdownload.md).
- [`ILSAdownload()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAdownload.md)
  connects to GitHub to check for most recent links.

## ILSAmerge 0.8.4

- Fixed internal argument checks.
- Added
  [`ILSAdownload()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAdownload.md)
  for downloading ILSA ‘SPSS’ datasets.

## ILSAmerge 0.8.1

- Added
  [`justload()`](https://dopatendo.github.io/ILSAmerge/reference/justload.md)
  to load files in a list but not merge them or save them.

## ILSAmerge 0.8.0

- Initial version
