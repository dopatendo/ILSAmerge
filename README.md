
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
[![](https://img.shields.io/github/r-package/v/dopatendo/ILSAmerge)](https://github.com/dopatendo/ILSAmerge)
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

## Downloading files

For downloading ILSA data, we can use `ILSAdownload()`. For an example
see `vignette("Download")`.

## Identify ILSA files

For identifying populations within a study, we can use
`ILSAfiles.info()`. For an example see `vignette("IndentifyILSAfiles")`.

## Load data without merging

If we need to load ILSA data but not save it, we can use `justload()`.
For an example see `vignette("Load")`.

## Merge files

For merging ILSA data, we can use `ILSAmerge()`. For an example see
`vignette("Merge")`.

## Rename files

For rename ILSA data, we can use `ILSArename()`. For an example see
`vignette("Rename")`.

## Download, merge and combine in one step

We can also combine all the steps for preparing ILSA data (downloading, merging,
combining respondents, and renaming) using `ILSAready()`.
