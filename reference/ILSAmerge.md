# Merge ILSA data

Merges 'SPSS' data from different International Large-Scale Assessments
(ILSA). This function has been tested to behave correctly for: 'TIMSS',
'TIMSS Advanced', 'PIRLS', 'ICCS', 'ICILS', 'LANA', CIVED', 'REDS',
'RLII', and 'SITES' (2006).

## Usage

``` r
ILSAmerge(
  inputdir = getwd(),
  outputdir = getwd(),
  population = NULL,
  filetype = c("rds", "zsav", "sav"),
  MBlimit = NULL,
  MBlistlimit = 200,
  SPSSlimit = 50,
  quiet = FALSE
)
```

## Arguments

- inputdir:

  a string indicating the path where ILSA 'SPSS' files are stored.

- outputdir:

  the directory where the merged data will be saved.

- population:

  a character vector indicating which files should be merged. If `NULL`
  (the default), all files will be merged. For more information on
  available populations, run
  [`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md)
  first.

- filetype:

  a string indicating the type of file to be saved, it can be `"rds"`,
  `"zsav"`, or `"sav"`.

- MBlimit:

  a numerical value indicating the allowed limit of the combined storage
  of the files of one type (see
  [`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md)).
  For type files that go over the limit, files will not be merged in R,
  but an 'SPSS' syntax will be produced via
  [`spss.syntax()`](https://dopatendo.github.io/ILSAmerge/reference/spss.syntax.md).
  If set to `NULL`, no limit will be used and all files will be merged
  within R. If speed is a problem, we recommend that this number should
  not be over `200` and merge the rest in 'SPSS'. Beware that some ILSA
  will have files with different columns and this could cause some
  'SPSS' syntaxes to fail. If this happens, merge through `R`.

- MBlistlimit:

  a numerical value indicating the allowed limit of the combined storage
  of the files of one type for merging through a list. Values over the
  limit will be merged through a matrix, which will be slower but uses
  less memory. Default is `200`.

- SPSSlimit:

  a numerical value indicating the limit of files per command of 'SPSS',
  typically `50`.

- quiet:

  a logical value indicating if progress status should be shown. Default
  is `FALSE`.

## Value

Saves merged ILSA data or `.sps` syntax for merging ILSA data.

## Details

For files merged within R it will also add country information where
needed. Country information will be retrieved from 'GitHub' if possible.
If not, it will use the package internal data.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Path where merged files will be saved
dir.create(file.path(tempdir(),"ILSAmerge"))
output <- file.path(tempdir(),"ILSAmerge")

# Merging 'REDS' 2021, as .rds file
ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
#> 12 files detected. Merging into 3 files.
#> Merging BCGV1. Type 1 of 3.
#> Merging dataset 1 of 4.
#> Merging dataset 2 of 4.
#> Merging dataset 3 of 4.
#> Merging dataset 4 of 4.
#> Merging BCGV1 took 0 seconds or 0 minutes.
#> Merging BSGV1. Type 2 of 3.
#> Merging dataset 1 of 4.
#> Merging dataset 2 of 4.
#> Merging dataset 3 of 4.
#> Merging dataset 4 of 4.
#> Merging BSGV1 took 0 seconds or 0 minutes.
#> Merging BTGV1. Type 3 of 3.
#> Merging dataset 1 of 4.
#> Merging dataset 2 of 4.
#> Merging dataset 3 of 4.
#> Merging dataset 4 of 4.
#> Merging BTGV1 took 0 seconds or 0 minutes.
#> Merging took 0 seconds or 0.01 minutes.
```
