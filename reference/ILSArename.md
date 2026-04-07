# Rename [`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md) files

Renames files produced by
[`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
from name codes to comprehensible names including the study name, year
and respondent. This function has been tested to behave correctly for:
'TIMSS', 'TIMSS Advanced', 'PIRLS', 'ICCS', 'ICILS', 'LANA', 'CIVED',
'REDS', 'RLII', and 'SITES' (2006).

## Usage

``` r
ILSArename(
  inputdir = getwd(),
  codeTOname = TRUE,
  overwrite = TRUE,
  quiet = FALSE
)
```

## Arguments

- inputdir:

  a string indicating the path where ILSA 'SPSS' files are stored.

- codeTOname:

  a logical value indicating if files should be renamed from codes to
  names (`TRUE`) or from names to codes (`FALSE`). Default is `TRUE`.

- overwrite:

  a logical value indicating if files should be overwritten. If `FALSE`,
  files will be copied with the new names. Default is `TRUE`.

- quiet:

  a logical value indicating if progress status should be shown. Default
  is `FALSE`.

## Value

Renames or copies files produced by
[`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md).

## Examples

``` r
# Merge files
dir.create(file.path(tempdir(),"REDS2021"),showWarnings = FALSE)
ILSAmerge(inputdir = system.file("extdata/reds", package = "ILSAmerge"), 
outputdir = file.path(tempdir(),"REDS2021"))
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
#> Merging took 0 seconds or 0 minutes.

# Show files with raw names
list.files(file.path(tempdir(),"REDS2021"))
#> [1] "BCGV1.rds" "BSGV1.rds" "BTGV1.rds"

# Rename files
ILSArename(inputdir = file.path(tempdir(),"REDS2021"))
#> 3  ILSAmerge() file(s) found.
#> 3  ILSAmerge() file(s) renamed.

# Show files new names 
list.files(file.path(tempdir(),"REDS2021"))
#> [1] "REDS_2021_school.rds"  "REDS_2021_student.rds" "REDS_2021_teacher.rds"
```
