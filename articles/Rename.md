# Rename ILSAmerge files

Quiet often ILSA file names come in codes representing different
respondents, and cycles. To make this codes comprehensible, we can use
[`ILSArename()`](https://dopatendo.github.io/ILSAmerge/reference/ILSArename.md)
to give new names to merged files.

First, we need to use
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
to merge the files:

``` r
dir.create(file.path(tempdir(),"REDS2021"),showWarnings = FALSE)

ILSAmerge(inputdir = system.file("extdata/reds", package = "ILSAmerge"), 
          outputdir = file.path(tempdir(),"REDS2021"), 
          filetype = c("rds", "zsav", "sav"),quiet = TRUE)
```

This will create 3 files with codes for respondent and cycle:

``` r
list.files(file.path(tempdir(),"REDS2021"))
## [1] "BCGV1.rds" "BSGV1.rds" "BTGV1.rds"
```

With
[`ILSArename()`](https://dopatendo.github.io/ILSAmerge/reference/ILSArename.md)
we can convert these names:

``` r
ILSArename(inputdir = file.path(tempdir(),"REDS2021"))
## 3  ILSAmerge() file(s) found.
## 3  ILSAmerge() file(s) renamed.
list.files(file.path(tempdir(),"REDS2021"))
## [1] "REDS_2021_school.rds"  "REDS_2021_student.rds" "REDS_2021_teacher.rds"
```
