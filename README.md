
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ILSAmerge

It is common that data from International Large-Scale Assessments
(ILSA), like TIMSS, TIMSS Advanced, PIRLS, ICCS, ICILS, CIVED, REDS,
RLII, and SITES, come in an unaggregated format.

That means that data is published in hundreds of files, each of one
represents a combination of countries and participants (students,
teachers, principals, etc.)

It is also common that researches would need to merge this data to
include all countries into a single file.

The goal of ILSAmerge is to make this process as simple and as
straightforward as possible.

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/dopatendo/ILSAmerge)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Installation

You can install the development version of `ILSAmerge` using:

``` r
remotes::install_github("dopatendo/ILSAmerge")
```

## Identifying files - `ILSAfiles.info()`

For example, if we download the data of TIMSS 1995 G4 from its original
source, we could find the SPSS data files in a path like this:

``` r
input <- "C:/TIMSS1995_IDB_SPSS_G4/Data"
```

Where we could a large number of files:

``` r
length(list.files(input))
#> [1] 130
list.files(input)
#>   [1] "ACGAUSm1.sav" "ACGAUTm1.sav" "ACGCANm1.sav" "ACGCYPm1.sav" "ACGCZEm1.sav"
#>   [6] "ACGENGm1.sav" "ACGGRCm1.sav" "ACGHKGm1.sav" "ACGHUNm1.sav" "ACGIRLm1.sav"
#>  [11] "ACGIRNm1.sav" "ACGISLm1.sav" "ACGISRm1.sav" "ACGJPNm1.sav" "ACGKORm1.sav"
#>  [16] "ACGKWTm1.sav" "ACGLVAm1.sav" "ACGNLDm1.sav" "ACGNORm1.sav" "ACGNZLm1.sav"
#>  [21] "ACGPRTm1.sav" "ACGSCOm1.sav" "ACGSGPm1.sav" "ACGSVNm1.sav" "ACGTHAm1.sav"
#>  [26] "ACGUSAm1.sav" "ASAAUSm1.sav" "ASAAUTm1.sav" "ASACANm1.sav" "ASACYPm1.sav"
#>  [31] "ASACZEm1.sav" "ASAENGm1.sav" "ASAGRCm1.sav" "ASAHKGm1.sav" "ASAHUNm1.sav"
#>  [36] "ASAIRLm1.sav" "ASAIRNm1.sav" "ASAISLm1.sav" "ASAISRm1.sav" "ASAJPNm1.sav"
#>  [41] "ASAKORm1.sav" "ASAKWTm1.sav" "ASALVAm1.sav" "ASANLDm1.sav" "ASANORm1.sav"
#>  [46] "ASANZLm1.sav" "ASAPRTm1.sav" "ASASCOm1.sav" "ASASGPm1.sav" "ASASVNm1.sav"
#>  [51] "ASATHAm1.sav" "ASAUSAm1.sav" "ASGAUSm1.sav" "ASGAUTm1.sav" "ASGCANm1.sav"
#>  [56] "ASGCYPm1.sav" "ASGCZEm1.sav" "ASGENGm1.sav" "ASGGRCm1.sav" "ASGHKGm1.sav"
#>  [61] "ASGHUNm1.sav" "ASGIRLm1.sav" "ASGIRNm1.sav" "ASGISLm1.sav" "ASGISRm1.sav"
#>  [66] "ASGJPNm1.sav" "ASGKORm1.sav" "ASGKWTm1.sav" "ASGLVAm1.sav" "ASGNLDm1.sav"
#>  [71] "ASGNORm1.sav" "ASGNZLm1.sav" "ASGPRTm1.sav" "ASGSCOm1.sav" "ASGSGPm1.sav"
#>  [76] "ASGSVNm1.sav" "ASGTHAm1.sav" "ASGUSAm1.sav" "ASTAUSm1.sav" "ASTAUTm1.sav"
#>  [81] "ASTCANm1.sav" "ASTCYPm1.sav" "ASTCZEm1.sav" "ASTENGm1.sav" "ASTGRCm1.sav"
#>  [86] "ASTHKGm1.sav" "ASTHUNm1.sav" "ASTIRLm1.sav" "ASTIRNm1.sav" "ASTISLm1.sav"
#>  [91] "ASTISRm1.sav" "ASTJPNm1.sav" "ASTKORm1.sav" "ASTKWTm1.sav" "ASTLVAm1.sav"
#>  [96] "ASTNLDm1.sav" "ASTNORm1.sav" "ASTNZLm1.sav" "ASTPRTm1.sav" "ASTSCOm1.sav"
#> [101] "ASTSGPm1.sav" "ASTSVNm1.sav" "ASTTHAm1.sav" "ASTUSAm1.sav" "ATGAUSm1.sav"
#> [106] "ATGAUTm1.sav" "ATGCANm1.sav" "ATGCYPm1.sav" "ATGCZEm1.sav" "ATGENGm1.sav"
#> [111] "ATGGRCm1.sav" "ATGHKGm1.sav" "ATGHUNm1.sav" "ATGIRLm1.sav" "ATGIRNm1.sav"
#> [116] "ATGISLm1.sav" "ATGISRm1.sav" "ATGJPNm1.sav" "ATGKORm1.sav" "ATGKWTm1.sav"
#> [121] "ATGLVAm1.sav" "ATGNLDm1.sav" "ATGNORm1.sav" "ATGNZLm1.sav" "ATGPRTm1.sav"
#> [126] "ATGSCOm1.sav" "ATGSGPm1.sav" "ATGSVNm1.sav" "ATGTHAm1.sav" "ATGUSAm1.sav"
```

To summarize this information, we can use `ILSAfile.info()`:

``` r
library(ILSAmerge)

ILSAfile.info(inputdir = input)
#>   Population Files    MB
#> 1      ACGm1    26   3.9
#> 2      ASAm1    26 106.4
#> 3      ASGm1    26 159.5
#> 4      ASTm1    26 127.0
#> 5      ATGm1    26  11.7
```

Where we can see how many file types or populations exist in the path,
how many files per population, and the combined storage of all these
file.

## Merging files - `ILSAmerge()`

Using the same path we can automatically convert these 130 files into
just 5, one for each population.

For that we only need to include a new path, where the merged files will
be saved:

``` r
output <- "C:/TIMSS1995_IDB_SPSS_G4/MERGED"
```

Then, we can simply run `ILSAmerge()` and all files will be merged. The
default type will be an “rds”, but also we can make “zsav” and “sav”
files:

``` r
ILSAmerge(inputdir = input, outputdir = output, filetype = c("rds", "zsav", "sav"))
```

## Adjusting merge options

Depending on your goals or the capabilities of your computer, you may
consider adjusting some options of `ILSAmerge()`:

- `population`: If you need to merge only some of the populations from
  the path, you could include them here as a character vector. It should
  be in the same format as the output of `ILSAfile.info`.
- `MBlimit`: If set to `NULL`, the default, all files will be merged
  within R. If set a numeric value that will establish a limit (from the
  output of `ILSAfile.info`) of which files are going to be merged by R,
  the populations that go over this limit will not be merged by R, but
  instead an SPSS syntax will be produced. Later, you can use this
  syntax to merge the files.
- `MBlistlimit`: In `ILSAmerge()` files are merged in two ways: using a
  list produced by an lapply function, or an empty matrix filled up by
  subscripts. The first method is almost always faster, but it can use a
  lot of memory. This argument establishes a limit for merging by list
  and not by matrix. We recommend not setting this higher than 200.
- `SPSSlimit`: If SPSS syntaxes are going to be produced it is important
  to take into account the SPSS requirements for handling files. This
  could vary by SPSS version, but normally it is a total of 50 files per
  command.

## Progress and time for merging

When `ILSAmerge()` runs, information about the progress of the merging
and the running time will be produced:

``` r
ILSAmerge(inputdir = input, outputdir = output, filetype = c("rds", "zsav", "sav"))
#> 130 files detected. Merging into 5 files.
#> Merging ACGm1. Type 1 of 5.
#> Merging dataset 1 of 26.
#> Merging dataset 2 of 26.
#> Merging dataset 3 of 26.
#> Merging dataset 4 of 26.
#> Merging dataset 5 of 26.
#> Merging dataset 6 of 26.
#> Merging dataset 7 of 26.
#> Merging dataset 8 of 26.
#> Merging dataset 9 of 26.
#> Merging dataset 10 of 26.
#> Merging dataset 11 of 26.
#> Merging dataset 12 of 26.
#> Merging dataset 13 of 26.
#> Merging dataset 14 of 26.
#> Merging dataset 15 of 26.
#> Merging dataset 16 of 26.
#> Merging dataset 17 of 26.
#> Merging dataset 18 of 26.
#> Merging dataset 19 of 26.
#> Merging dataset 20 of 26.
#> Merging dataset 21 of 26.
#> Merging dataset 22 of 26.
#> Merging dataset 23 of 26.
#> Merging dataset 24 of 26.
#> Merging dataset 25 of 26.
#> Merging dataset 26 of 26.
#> Merging ACGm1 took 1 seconds or 0.02 minutes.
#> Merging ASAm1. Type 2 of 5.
#> Merging dataset 1 of 26.
#> Merging dataset 2 of 26.
#> Merging dataset 3 of 26.
#> Merging dataset 4 of 26.
#> Merging dataset 5 of 26.
#> Merging dataset 6 of 26.
#> Merging dataset 7 of 26.
#> Merging dataset 8 of 26.
#> Merging dataset 9 of 26.
#> Merging dataset 10 of 26.
#> Merging dataset 11 of 26.
#> Merging dataset 12 of 26.
#> Merging dataset 13 of 26.
#> Merging dataset 14 of 26.
#> Merging dataset 15 of 26.
#> Merging dataset 16 of 26.
#> Merging dataset 17 of 26.
#> Merging dataset 18 of 26.
#> Merging dataset 19 of 26.
#> Merging dataset 20 of 26.
#> Merging dataset 21 of 26.
#> Merging dataset 22 of 26.
#> Merging dataset 23 of 26.
#> Merging dataset 24 of 26.
#> Merging dataset 25 of 26.
#> Merging dataset 26 of 26.
#> Merging ASAm1 took 29 seconds or 0.48 minutes.
#> Merging ASGm1. Type 3 of 5.
#> Merging dataset 1 of 26.
#> Merging dataset 2 of 26.
#> Merging dataset 3 of 26.
#> Merging dataset 4 of 26.
#> Merging dataset 5 of 26.
#> Merging dataset 6 of 26.
#> Merging dataset 7 of 26.
#> Merging dataset 8 of 26.
#> Merging dataset 9 of 26.
#> Merging dataset 10 of 26.
#> Merging dataset 11 of 26.
#> Merging dataset 12 of 26.
#> Merging dataset 13 of 26.
#> Merging dataset 14 of 26.
#> Merging dataset 15 of 26.
#> Merging dataset 16 of 26.
#> Merging dataset 17 of 26.
#> Merging dataset 18 of 26.
#> Merging dataset 19 of 26.
#> Merging dataset 20 of 26.
#> Merging dataset 21 of 26.
#> Merging dataset 22 of 26.
#> Merging dataset 23 of 26.
#> Merging dataset 24 of 26.
#> Merging dataset 25 of 26.
#> Merging dataset 26 of 26.
#> Merging ASGm1 took 24 seconds or 0.4 minutes.
#> Merging ASTm1. Type 4 of 5.
#> Merging dataset 1 of 26.
#> Merging dataset 2 of 26.
#> Merging dataset 3 of 26.
#> Merging dataset 4 of 26.
#> Merging dataset 5 of 26.
#> Merging dataset 6 of 26.
#> Merging dataset 7 of 26.
#> Merging dataset 8 of 26.
#> Merging dataset 9 of 26.
#> Merging dataset 10 of 26.
#> Merging dataset 11 of 26.
#> Merging dataset 12 of 26.
#> Merging dataset 13 of 26.
#> Merging dataset 14 of 26.
#> Merging dataset 15 of 26.
#> Merging dataset 16 of 26.
#> Merging dataset 17 of 26.
#> Merging dataset 18 of 26.
#> Merging dataset 19 of 26.
#> Merging dataset 20 of 26.
#> Merging dataset 21 of 26.
#> Merging dataset 22 of 26.
#> Merging dataset 23 of 26.
#> Merging dataset 24 of 26.
#> Merging dataset 25 of 26.
#> Merging dataset 26 of 26.
#> Merging ASTm1 took 13 seconds or 0.21 minutes.
#> Merging ATGm1. Type 5 of 5.
#> Merging dataset 1 of 26.
#> Merging dataset 2 of 26.
#> Merging dataset 3 of 26.
#> Merging dataset 4 of 26.
#> Merging dataset 5 of 26.
#> Merging dataset 6 of 26.
#> Merging dataset 7 of 26.
#> Merging dataset 8 of 26.
#> Merging dataset 9 of 26.
#> Merging dataset 10 of 26.
#> Merging dataset 11 of 26.
#> Merging dataset 12 of 26.
#> Merging dataset 13 of 26.
#> Merging dataset 14 of 26.
#> Merging dataset 15 of 26.
#> Merging dataset 16 of 26.
#> Merging dataset 17 of 26.
#> Merging dataset 18 of 26.
#> Merging dataset 19 of 26.
#> Merging dataset 20 of 26.
#> Merging dataset 21 of 26.
#> Merging dataset 22 of 26.
#> Merging dataset 23 of 26.
#> Merging dataset 24 of 26.
#> Merging dataset 25 of 26.
#> Merging dataset 26 of 26.
#> Merging ATGm1 took 4 seconds or 0.07 minutes.
#> Merging took 71 seconds or 1.19 minutes.
```

## Loading data without merging - `justload()`

If we need to load the data but not save it, we can use `justload()`. By
default, it will load all datasets of an specific population:

``` r
list <- justload(inputdir = input, population = "ACGm1")
class(list)
#> [1] "list"
sapply(list,dim)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]  179  135  395  187  188  134  176  124  150   165   180   147    87   142
#> [2,]  273  273  273  273  273  273  273  273  273   273   273   273   273   273
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,]   150   150   126   135   140   149   157   154   191   122   154   189
#> [2,]   273   273   273   273   273   273   273   273   273   273   273   273
```

Nevertheless, it is also possible to load all the datasets of a single
population without rows, i.e., just the attributes of the columns. This
could be helpful for running some consistency checks between files:

``` r
list <- justload(inputdir = input, population = "ACGm1", justattributes = TRUE)
class(list)
#> [1] "list"
sapply(list,dim)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0
#> [2,]  273  273  273  273  273  273  273  273  273   273   273   273   273   273
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,]     0     0     0     0     0     0     0     0     0     0     0     0
#> [2,]   273   273   273   273   273   273   273   273   273   273   273   273

list[[1]]
#> # A tibble: 0 × 273
#> # ℹ 273 variables: VERSION <dbl>, IDCNTRY <dbl>, IDPOP <dbl+lbl>,
#> #   IDSTRAT <dbl+lbl>, IDSCHOOL <dbl>, ILREPLAC <dbl+lbl>, ITREPLAC <dbl+lbl>,
#> #   IDGRADER <dbl+lbl>, ITPART <dbl+lbl>, ACBGCOMM <dbl+lbl>,
#> #   ACBGGRPK <dbl+lbl>, ACBGGRK <dbl+lbl>, ACBGGR1 <dbl+lbl>,
#> #   ACBGGR2 <dbl+lbl>, ACBGGR3 <dbl+lbl>, ACBGGR4 <dbl+lbl>, ACBGGR5 <dbl+lbl>,
#> #   ACBGGR6 <dbl+lbl>, ACBGGR7 <dbl+lbl>, ACBGGR8 <dbl+lbl>, ACBGGR9 <dbl+lbl>,
#> #   ACBGGR10 <dbl+lbl>, ACBGGR11 <dbl+lbl>, ACBGGR12 <dbl+lbl>, …
```
