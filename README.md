
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ILSAmerge <img src="man/figures/logo.png" align="right" height="127" alt="" />

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

[![CRAN
status](https://www.r-pkg.org/badges/version/ILSAmerge)](https://CRAN.R-project.org/package=ILSAmerge)
![GitHub R package
version](https://img.shields.io/github/r-package/v/dopatendo/ILSAmerge)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![Static
Badge](https://img.shields.io/badge/dependencies-haven-brightgreen)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.ILSAmerge-green.svg)](https://doi.org/10.32614/CRAN.package.ILSAmerge)
<!-- ![![](http://cranlogs.r-pkg.org/badges/grand-total/ILSAmerge?color=blue)](https://cran.r-project.org/package=ILSAmerge)-->
<!-- badges: end -->

## Installation

You can install the stable version of `ILSAmerge` directly from CRAN:

``` r
install.packages("ILSAmerge")
```

Or, if you wish to install the development version of `ILSAmerge`:

``` r
remotes::install_github("dopatendo/ILSAmerge")
```

## Downloading files - `ILSAdownload()`

We can download SPSS files of supported ILSA using `ILSAdownload()`. If
we do, a license agreement will be downloaded, and the user will be
prompted to agree with these terms, otherwise data will not be
downloaded. We can also decide if we want to unzip them:

``` r
output <- tempdir()
ILSAdownload(study = "RLII", year = 1991, outputdir = output, unzip = TRUE,agreeLicense = TRUE)
#> By accessing the Data Repository, IDB Analyzer and Data visualizer, you indicate that you agree to the terms and conditions associated with their use. Please read the Disclaimer and License Agreement for full details.
#> 1 files found for RLII 1991.
#> Visit https://www.iea.nl/data-tools/repository/readingliteracy to know how to use and cite these datasets.
```

## Identifying files - `ILSAfiles.info()`

For example, if we download the data of RLII 1991 from its original
source, we could find the SPSS data files in a path like this:

``` r

input <- file.path(output,"RLII1991_IDB_SPSS/Data")
```

Where we could a large number of files:

``` r
length(list.files(input))
#> [1] 9
list.files(input)
#> [1] "ASCGRCt1.sav" "ASCHUNt1.sav" "ASCISLt1.sav" "ASCITAt1.sav" "ASCNZLt1.sav"
#> [6] "ASCSGPt1.sav" "ASCSVNt1.sav" "ASCSWEt1.sav" "ASCUSAt1.sav"
```

To summarize this information, we can use `ILSAfile.info()`:

``` r

ILSAfile.info(inputdir = input)
#>   Population Files   MB
#> 1      ASCt1     9 19.9
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
dir.create(file.path(tempdir(),"RLII1991_IDB_SPSS/MERGED"),showWarnings = FALSE)
output <- file.path(tempdir(),"RLII1991_IDB_SPSS/MERGED")
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
#> 9 files detected. Merging into 1 files.
#> Merging ASCt1. Type 1 of 1.
#> Merging dataset 1 of 9.
#> Merging dataset 2 of 9.
#> Merging dataset 3 of 9.
#> Merging dataset 4 of 9.
#> Merging dataset 5 of 9.
#> Merging dataset 6 of 9.
#> Merging dataset 7 of 9.
#> Merging dataset 8 of 9.
#> Merging dataset 9 of 9.
#> Merging ASCt1 took 2 seconds or 0.04 minutes.
#> Merging took 2 seconds or 0.04 minutes.
```

## Loading data without merging - `justload()`

If we need to load the data but not save it, we can use `justload()`. By
default, it will load all datasets of an specific population:

``` r
list <- justload(inputdir = input, population = "ASCt1")
class(list)
#> [1] "list"
sapply(list,dim)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,] 3516 3009 3961 2221 3016 7326 3297 4301 6433
#> [2,]  247  247  247  247  247  247  247  247  247
```

Nevertheless, it is also possible to load all the datasets of a single
population without rows, i.e., just the attributes of the columns. This
could be helpful for running some consistency checks between files:

``` r
list <- justload(inputdir = input, population = "ASCt1", justattributes = TRUE)
class(list)
#> [1] "list"
sapply(list,dim)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,]    0    0    0    0    0    0    0    0    0
#> [2,]  247  247  247  247  247  247  247  247  247

list[[1]]
#> # A tibble: 0 × 247
#> # ℹ 247 variables: IDCNTRY <dbl>, IDSCHOOL <dbl>, IDCLASS <dbl>, IDGRADE <dbl>,
#> #   IDSTUD <dbl>, ITBIRTHM <dbl+lbl>, ITBIRTHY <dbl+lbl>, ITSEX <dbl+lbl>,
#> #   ITADMINI <dbl+lbl>, ITDATEM <dbl+lbl>, ITDATEY <dbl+lbl>, ITLANG <dbl+lbl>,
#> #   IDPOP <dbl+lbl>, IDGRADER <dbl+lbl>, INRLS91 <dbl+lbl>, ASAGEY <dbl+lbl>,
#> #   ASAGEM <dbl+lbl>, ASSEX <dbl+lbl>, ASUSLAN <dbl+lbl>, ASMEAL1 <dbl+lbl>,
#> #   ASMEAL2 <dbl+lbl>, ASMEAL3 <dbl+lbl>, ASNEWS <dbl+lbl>, ASTV <dbl+lbl>,
#> #   ASBOOKS <dbl+lbl>, ASHOM01 <dbl+lbl>, ASHOM02 <dbl+lbl>, …
```
