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

For downloading ILSA data, we can use
[`ILSAdownload()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAdownload.md).
For an example see
[`vignette("Download")`](https://dopatendo.github.io/ILSAmerge/articles/Download.md).

## Identify ILSA files

For identifying populations within a study, we can use
`ILSAfiles.info()`. For an example see `vignette("IndentifyILSAfiles")`.

## Load data without merging

If we need to load ILSA data but not save it, we can use
[`justload()`](https://dopatendo.github.io/ILSAmerge/reference/justload.md).
For an example see
[`vignette("Load")`](https://dopatendo.github.io/ILSAmerge/articles/Load.md).

## Merge files

For merging ILSA data, we can use
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md).
For an example see
[`vignette("Merge")`](https://dopatendo.github.io/ILSAmerge/articles/Merge.md).

## Rename files

For rename ILSA data, we can use
[`ILSArename()`](https://dopatendo.github.io/ILSAmerge/reference/ILSArename.md).
For an example see
[`vignette("Rename")`](https://dopatendo.github.io/ILSAmerge/articles/Rename.md).

## Download, merge and combine in one step

We can also combine all the steps for preparing ILSA data (downloading,
merging, combining respondents, and renaming) using
[`ILSAready()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md).
