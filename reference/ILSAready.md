# Download and prepare ILSA data

Downloads ILSA data, merges it, combines students and adds school
information. This function is a wrapper for
[`ILSAdownload`](https://dopatendo.github.io/ILSAmerge/reference/ILSAdownload.md),
[`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md),
[`ILSArename`](https://dopatendo.github.io/ILSAmerge/reference/ILSArename.md),
[`combineStudents`](https://dopatendo.github.io/ILSAmerge/reference/combineStudents.md),
and
[`addSchools`](https://dopatendo.github.io/ILSAmerge/reference/addSchools.md).
To see which ILSA are available for this function use
[`availableILSA`](https://dopatendo.github.io/ILSAmerge/reference/availableILSA.md).
If data is already downloaded you can use `ILSAreadylocal`.

## Usage

``` r
ILSAready(
  study,
  year,
  outputdir = getwd(),
  filetype = c("rds", "zsav", "sav"),
  MBlistlimit = 200,
  quiet = FALSE,
  agreeLicense = FALSE,
  maxtime = 999
)

ILSAreadylocal(
  inputdir = getwd(),
  outputdir = getwd(),
  filetype = c("rds", "zsav", "sav"),
  quiet = FALSE,
  MBlistlimit = 200
)
```

## Arguments

- study:

  a string indicating the name of the study. For available studies check
  the description of this function.

- year:

  a numeric value indicating the year of the study.

- outputdir:

  the directory where the merged data will be saved.

- filetype:

  a string indicating the type of file to be saved, it can be `"rds"`,
  `"zsav"`, or `"sav"`.

- MBlistlimit:

  a numerical value indicating the allowed limit of the combined storage
  of the files of one type for merging through a list. Values over the
  limit will be merged through a matrix, which will be slower but uses
  less memory. Default is `200`.

- quiet:

  a logical value indicating if progress status should be shown. Default
  is `FALSE`.

- agreeLicense:

  a logical value indicating if you agree with the Disclaimer and
  License Agreement file from www.iea.nl. If `FALSE`, you will be
  prompted to agree with it or else data will not be downloaded. Default
  is `FALSE`.

- maxtime:

  a numeric value indicating the maximum time allowed for downloading a
  file. Default is `999`.

- inputdir:

  a string indicating the path where ILSA 'SPSS' files are stored.

## Value

Saves merged and renamed ILSA data.

## Examples

``` r
dir.create(file.path(tempdir(),"timssadv"),showWarnings = FALSE)
output <- file.path(tempdir(),"timssadv")

input <- system.file("extdata/timssadv", package = "ILSAmerge")

ILSAreadylocal(inputdir = input, outputdir = output, filetype = "zsav")
#> 15 files detected. Merging into 3 files.
#> Merging MCGM1. Type 1 of 3.
#> Merging dataset 1 of 5.
#> Merging dataset 2 of 5.
#> Merging dataset 3 of 5.
#> Merging dataset 4 of 5.
#> Merging dataset 5 of 5.
#> Merging MCGM1 took 0 seconds or 0 minutes.
#> Merging MSAM1. Type 2 of 3.
#> Merging dataset 1 of 5.
#> Merging dataset 2 of 5.
#> Merging dataset 3 of 5.
#> Merging dataset 4 of 5.
#> Merging dataset 5 of 5.
#> Merging MSAM1 took 0 seconds or 0 minutes.
#> Merging MSGM1. Type 3 of 3.
#> Merging dataset 1 of 5.
#> Merging dataset 2 of 5.
#> Merging dataset 3 of 5.
#> Merging dataset 4 of 5.
#> Merging dataset 5 of 5.
#> Merging MSGM1 took 0 seconds or 0 minutes.
#> Merging took 0 seconds or 0.01 minutes.
#> 2  ILSAmerge()/ILSArename() student file(s) found. Combining into 1 file(s).
#> Combining student 1 of 1.
#> Combining students took 0 seconds or 0 minutes.
#> 2  ILSAmerge()/ILSArename() file(s) found. Adding schools to 1 file(s).
#> Combined students found for MSAM1.
#> Adding schools to MSAM1, file 1 of 1.
#> Adding schools took 0 seconds or 0 minutes.
#> 3  ILSAmerge() file(s) found.
#> 3  ILSAmerge() file(s) renamed.
#> Preparing data took 1 seconds or 0.01 minutes.
```
