# ILSA data files information

Aggregates International Large-Scale Assessments (ILSA) data files
information by population.

## Usage

``` r
ILSAfile.info(inputdir = getwd())
```

## Arguments

- inputdir:

  a string indicating the path where ILSA 'SPSS' files are stored.

## Value

A data frame with the number of files and MBs per population.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Get file information
ILSAfile.info(inputdir = input)
#>   Population Files  MB
#> 1      BCGV1     4 0.3
#> 2      BSGV1     4 0.4
#> 3      BTGV1     4 0.3
```
