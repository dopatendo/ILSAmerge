# Add school data

Add school data to student and teacher files merged by
[`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md).
It will run
[`combineStudents`](https://dopatendo.github.io/ILSAmerge/reference/combineStudents.md)
internally. To see which ILSA are available for adding school data use
[`availableILSA`](https://dopatendo.github.io/ILSAmerge/reference/availableILSA.md).

## Usage

``` r
addSchools(inputdir = getwd(), outputdir = getwd(), quiet = FALSE)
```

## Arguments

- inputdir:

  a string indicating the path where
  [`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
  files are stored.

- outputdir:

  a string indicating where the combined data will be saved.

- quiet:

  a logical value indicating if progress status should be shown. Default
  is `FALSE`.

## Value

Saves combined student data and teacher data with added school data.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/timssadv", package = "ILSAmerge")

# Path where merged files will be saved
dir.create(file.path(tempdir(),"addSchools"))
output <- file.path(tempdir(),"addSchools")

# Merging 'TIMSS' Advanced 1995, as .rds file
ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
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

# Check file names
list.files(output,pattern = ".rds")
#> [1] "MCGM1.rds" "MSAM1.rds" "MSGM1.rds"

# Add school data
addSchools(inputdir = output, outputdir = output)
#> 2  ILSAmerge()/ILSArename() file(s) found. Adding schools to 1 file(s).
#> Combining students for MSAM1.
#> Adding schools to MSAM1, file 1 of 1.
#> Adding schools took 0 seconds or 0 minutes.

# Check file names
list.files(output,pattern = ".rds")
#> [1] "MCGM1.rds"                                 
#> [2] "MSAM1.rds"                                 
#> [3] "MSGM1.rds"                                 
#> [4] "TIMSSADVANCED_Math_1995_student&school.rds"
```
