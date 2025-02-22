# ILSAmerge 1.3.7
- Solved an issue with `mistoNAs()` when haven was not loaded.
- Solved an issue for weird behavior of `mistoNAs()` with ICILS 2013.
- Solved an issue with `ILSAready()` when filetype is not declared.


# ILSAmerge 1.3.6
- Added support for TIMSS 2023.
- Added support for ICILS 2020 Teacher Panel.
- Added support for ICILS 2023.
- Fixed bug in `justload()`.
- Fixed bug in `readILSA()`.

### Remaining issues
- No unzip of PISA and TALIS downloads.


# ILSAmerge 1.3.5
- Fixed an issue for saving 'SPSS' files.
- Added new functions for tibble management: `asthistibble()`,
`get.atr()`, `get.nas()`, `get.varlab()`, `mistoNAs()`, `untibble()`, and `whichcol()`.



# ILSAmerge 1.3.0
- Added `ILSAready()` and `ILSAreadylocal()` as wrappers for downloading, 
merging, combining respondents, and renaming.


# ILSAmerge 1.2.5
- Added `combineStudents()` for combining achievement and  background questionnaires.
- Added `addSchools()` for adding school data.
- Added labels to countries and creates variables with country codes and country names.


# ILSAmerge 1.0.5
- Added `ILSArename()` for creating comprehensible file names.
- Fixed a problem caused by PIRLS 2021 having an empty dataset.
- Fixed a problem with merging dates for TIMSS 2011. 
- Fixed a problem originated from PIRLS 2001 with its multiple capitalization styles. 


# ILSAmerge 1.0.1
- Added extensive new documentation.
- Added `availableILSA()` to check which ILSA can be downloaded.

# ILSAmerge 1.0.0
- CRAN submission.
- Added PISA and TALIS to `ILSAdownload()`.
- `ILSAdownload()` connects to GitHub to check for most recent links.

# ILSAmerge 0.8.4
- Fixed internal argument checks.
- Added `ILSAdownload()` for downloading ILSA 'SPSS' datasets.

# ILSAmerge 0.8.1
 - Added `justload()` to load files in a list but not merge them or save them.
 
# ILSAmerge 0.8.0
 - Initial version

