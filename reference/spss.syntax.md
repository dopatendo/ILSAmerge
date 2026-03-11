# 'SPSS' merge syntax

Produces and saves an 'SPSS' merge syntax given a list of files.

## Usage

``` r
spss.syntax(filelist, name, outputdir = getwd(), zsav = TRUE, SPSSlimit = 50)
```

## Arguments

- filelist:

  a character vector with the list of files to be merged.

- name:

  a string with the name of the merged file (without any extension).

- outputdir:

  the directory where the `.sps` file and the merged file will be saved.

- zsav:

  a logical value indicating if the the merged file should be compressed
  with zsav. Default is `TRUE`.

- SPSSlimit:

  a numerical value indicating the limit of files per command of 'SPSS',
  typically `50`.

## Value

Saves an `.sps` file with the 'SPSS' syntax for merging the desired
files.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Path where merged files will be saved
dir.create(file.path(tempdir(),"spsssyntax"))
output <- file.path(tempdir(),"spsssyntax")

# List of BCGV1 files to be merged
files <- list.files(path = input, pattern = "BCG.+V1|bcg.+v1")

# Create 'SPSS' syntax
spss.syntax(filelist = files, name = "BCGV1", outputdir = output, zsav = TRUE)
```
