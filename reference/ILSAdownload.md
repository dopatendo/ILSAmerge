# Download ILSA data

Downloads 'SPSS' data from different International Large-Scale
Assessments (ILSA). This functions supports the following ILSA: 'TIMSS',
'TIMSS Advanced', 'PIRLS', 'ICCS', 'ICILS', 'CIVED', 'REDS', 'RLII', and
'SITES.' Depending on the study, you will need to decide which data to
download, and read and accept its terms and conditions to proceed with
the download.

## Usage

``` r
ILSAdownload(
  study,
  year,
  outputdir = getwd(),
  unzip = FALSE,
  maxtime = 999,
  quiet = FALSE,
  agreeLicense = FALSE
)
```

## Arguments

- study:

  a string indicating the name of the study. For available studies check
  the description of this function.

- year:

  a numeric value indicating the year of the study.

- outputdir:

  the directory where data will be downloaded.

- unzip:

  a logical value indicating if files should be unzipped. Default is
  `FALSE`.

- maxtime:

  a numeric value indicating the maximum time allowed for downloading a
  file. Default is `999`.

- quiet:

  a logical value indicating if progress status should be shown. Default
  is `FALSE`.

- agreeLicense:

  a logical value indicating if you agree with the Disclaimer and
  License Agreement file from www.iea.nl. If `FALSE`, you will be
  prompted to agree with it or else data will not be downloaded. Default
  is `FALSE`.

## Value

Saves 'SPSS' ILSA data locally.

## Examples

``` r
# For example, to download 'RLII' 1991 data:

# Path where files will be saved
dir.create(file.path(tempdir(),"ILSAdownload"))
output <- file.path(tempdir(),"ILSAdownload")


# Downloading 'RLII' 1991 and unzipping files
ILSAdownload(study = "RLII", year = 1991, outputdir = output, unzip = TRUE, agreeLicense = TRUE)
#> By accessing the Data Repository, IDB Analyzer and Data visualizer, you indicate that you agree to the terms and conditions associated with their use. Please read the Disclaimer and License Agreement for full details.
#> 1 files found for RLII 1991.
#> Visit https://www.iea.nl/data-tools/repository/readingliteracy to know how to use and cite these datasets.

```
