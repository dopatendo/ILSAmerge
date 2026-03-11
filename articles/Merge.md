# Merge ILSA files

## Downloaded data

As an example, we can use the data of ‘REDS’ 2021 provided by the
package.

In the case of ‘REDS’ 2021, we have 12 files and 3 populations:

``` r
ILSAfile.info(inputdir = system.file("extdata/reds", package = "ILSAmerge"))
##   Population Files  MB
## 1      BCGV1     4 0.3
## 2      BSGV1     4 0.4
## 3      BTGV1     4 0.3
```

With
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
we can combine all the files into the number of populations we have.

The default type will be an “rds”, but also we can make “zsav” and “sav”
files:

``` r
ILSAmerge(inputdir = system.file("extdata/reds", package = "ILSAmerge"), 
          outputdir = tempdir(), 
          filetype = c("rds", "zsav", "sav"))
```

## Adjust merge options

Depending on your goals or the capabilities of your computer, you may
consider adjusting some options of
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md):

- `population`: If you need to merge only some of the populations from
  the path, you could include them here as a character vector. It should
  be in the same format as the output of
  [`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md).
- `MBlimit`: If set to `NULL`, the default, all files will be merged
  within R. If set a numeric value that will establish a limit (from the
  output of
  [`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md))
  of which files are going to be merged by R, the populations that go
  over this limit will not be merged by R, but instead an SPSS syntax
  will be produced. Later, you can use this syntax to merge the files.
- `MBlistlimit`: In
  [`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
  files are merged in two ways: using a list produced by an lapply
  function, or an empty matrix filled up by subscripts. The first method
  is almost always faster, but it can use a lot of memory. This argument
  establishes a limit for merging by list and not by matrix. We
  recommend not setting this higher than 200.
- `SPSSlimit`: If SPSS syntaxes are going to be produced it is important
  to take into account the SPSS requirements for handling files. This
  could vary by SPSS version, but normally it is a total of 50 files per
  command.

## Progress and time for merging

When
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
runs, information about the progress of the merging and the running time
will be produced:

``` r
ILSAmerge(inputdir = system.file("extdata/reds", package = "ILSAmerge"), 
          outputdir = tempdir(), 
          filetype = c("rds", "zsav", "sav"))
## 12 files detected. Merging into 3 files.
## Merging BCGV1. Type 1 of 3.
## Merging dataset 1 of 4.
## Merging dataset 2 of 4.
## Merging dataset 3 of 4.
## Merging dataset 4 of 4.
## Merging BCGV1 took 0 seconds or 0.01 minutes.
## Merging BSGV1. Type 2 of 3.
## Merging dataset 1 of 4.
## Merging dataset 2 of 4.
## Merging dataset 3 of 4.
## Merging dataset 4 of 4.
## Merging BSGV1 took 0 seconds or 0 minutes.
## Merging BTGV1. Type 3 of 3.
## Merging dataset 1 of 4.
## Merging dataset 2 of 4.
## Merging dataset 3 of 4.
## Merging dataset 4 of 4.
## Merging BTGV1 took 0 seconds or 0 minutes.
## Merging took 1 seconds or 0.01 minutes.
```

## Merging between respondents

After merging countries it is also possible to merge datasets of the
same or different respondents.

Two functions are available to merge respondents:
[`combineStudents()`](https://dopatendo.github.io/ILSAmerge/reference/combineStudents.md),
and `addSchools`.

### Combine students

[`combineStudents()`](https://dopatendo.github.io/ILSAmerge/reference/combineStudents.md)
combines multiple student files, e.g., achievement and background
questionnaires, using ‘TIMSS’ Advanced 1995:

``` r
# Path were raw 'SPSS' files are
input <- system.file("extdata/timssadv", package = "ILSAmerge")

# Path were merged files will be saved
dir.create(file.path(tempdir(),"combineStudents"))
output <- file.path(tempdir(),"combineStudents")

# Merging 'TIMSS' Advanced 1995, as .rds file
ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = TRUE)

# Rename files
ILSArename(output)
## 3  ILSAmerge() file(s) found.
## 3  ILSAmerge() file(s) renamed.

# Check file names
list.files(output,pattern = ".rds")
## [1] "TIMSSADVANCED_Math_1995_school.rds"             
## [2] "TIMSSADVANCED_Math_1995_student_achievement.rds"
## [3] "TIMSSADVANCED_Math_1995_student_background.rds"

# Combine student files
combineStudents(inputdir = output, outputdir = output)
## 2  ILSAmerge()/ILSArename() student file(s) found. Combining into 1 file(s).
## Combining student 1 of 1.
## Combining students took 0 seconds or 0 minutes.

# Check file names
list.files(output,pattern = ".rds")
## [1] "TIMSSADVANCED_Math_1995_school.rds"             
## [2] "TIMSSADVANCED_Math_1995_student_achievement.rds"
## [3] "TIMSSADVANCED_Math_1995_student_background.rds" 
## [4] "TIMSSADVANCED_Math_1995_student.rds"
```

As we can see, a new file was created combining the achievement and the
student questionnaire.

### Combine schools

[`addSchools()`](https://dopatendo.github.io/ILSAmerge/reference/addSchools.md)
adds school data to student and teacher files, using ‘TIMSS’ Advanced
1995:

``` r
# Check file names
list.files(output,pattern = ".rds")
## [1] "TIMSSADVANCED_Math_1995_school.rds"             
## [2] "TIMSSADVANCED_Math_1995_student_achievement.rds"
## [3] "TIMSSADVANCED_Math_1995_student_background.rds" 
## [4] "TIMSSADVANCED_Math_1995_student.rds"

# Add school data
addSchools(inputdir = output, outputdir = output)
## 2  ILSAmerge()/ILSArename() file(s) found. Adding schools to 1 file(s).
## Combined students found for MSAM1.
## Adding schools to MSAM1, file 1 of 1.
## Adding schools took 0 seconds or 0 minutes.

# Check file names
list.files(output,pattern = ".rds")
## [1] "TIMSSADVANCED_Math_1995_school.rds"             
## [2] "TIMSSADVANCED_Math_1995_student_achievement.rds"
## [3] "TIMSSADVANCED_Math_1995_student_background.rds" 
## [4] "TIMSSADVANCED_Math_1995_student.rds"            
## [5] "TIMSSADVANCED_Math_1995_student&school.rds"
```

As we can see, a new file was created combining the information of
students and schools.
