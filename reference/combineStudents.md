# Combine student data

Combines achievement and background student data merged by
[`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md).
To see which ILSA are available for combining use
[`availableILSA`](https://dopatendo.github.io/ILSAmerge/reference/availableILSA.md).

## Usage

``` r
combineStudents(inputdir = getwd(), outputdir = getwd(), quiet = FALSE)
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

Saves combined student data produced by
[`ILSAmerge`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md).

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/timssadv", package = "ILSAmerge")

# Path where merged files will be saved
dir.create(file.path(tempdir(),"combineStudents"))
output <- file.path(tempdir(),"combineStudents")

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
#> Merging took 0 seconds or 0 minutes.

# Rename files
ILSArename(output)
#> 3  ILSAmerge() file(s) found.
#> 3  ILSAmerge() file(s) renamed.

# Check file names
list.files(output,pattern = ".rds")
#> [1] "TIMSSADVANCED_Math_1995_school.rds"             
#> [2] "TIMSSADVANCED_Math_1995_student_achievement.rds"
#> [3] "TIMSSADVANCED_Math_1995_student_background.rds" 

# Combine student files
combineStudents(inputdir = output, outputdir = output)
#> 2  ILSAmerge()/ILSArename() student file(s) found. Combining into 1 file(s).
#> Combining student 1 of 1.
#> Combining students took 0 seconds or 0 minutes.

# Check file names
list.files(output,pattern = ".rds")
#> [1] "TIMSSADVANCED_Math_1995_school.rds"             
#> [2] "TIMSSADVANCED_Math_1995_student.rds"            
#> [3] "TIMSSADVANCED_Math_1995_student_achievement.rds"
#> [4] "TIMSSADVANCED_Math_1995_student_background.rds" 
```
