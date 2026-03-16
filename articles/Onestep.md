# One-step solution

We can also combine all the steps for preparing ILSA data (downloading,
merging, combining respondents, and renaming) with the function
[`ILSAready()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md):

``` r
dir.create(file.path(tempdir(),"rlii1991"),showWarnings = FALSE)
output <- file.path(tempdir(),"rlii1991")
ILSAready(study = "RLII", year = 1991,outputdir = output, agreeLicense = TRUE)
## By accessing the Data Repository, IDB Analyzer and Data visualizer, you indicate that you agree to the terms and conditions associated with their use. Please read the Disclaimer and License Agreement for full details.
## 1 files found for RLII 1991.
## Visit https://www.iea.nl/data-tools/repository/readingliteracy to know how to use and cite these datasets.9 files detected. Merging into 1 files.
## Merging ASCT1. Type 1 of 1.
## Merging dataset 1 of 9.
## Merging dataset 2 of 9.
## Merging dataset 3 of 9.
## Merging dataset 4 of 9.
## Merging dataset 5 of 9.
## Merging dataset 6 of 9.
## Merging dataset 7 of 9.
## Merging dataset 8 of 9.
## Merging dataset 9 of 9.
## Merging ASCT1 took 3 seconds or 0.05 minutes.
## Merging took 3 seconds or 0.05 minutes.
## 1  ILSAmerge() file(s) found.
## 1  ILSAmerge() file(s) renamed.
## Downloading and preparing data took 6 seconds or 0.1 minutes.
```

Also if data is already downloaded we can use
[`ILSAreadylocal()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAready.md):

``` r
dir.create(file.path(tempdir(),"timssadv"),showWarnings = FALSE)
output <- file.path(tempdir(),"timssadv")

input <- system.file("extdata/timssadv", package = "ILSAmerge")

ILSAreadylocal(inputdir = input, outputdir = output)
## 15 files detected. Merging into 3 files.
## Merging MCGM1. Type 1 of 3.
## Merging dataset 1 of 5.
## Merging dataset 2 of 5.
## Merging dataset 3 of 5.
## Merging dataset 4 of 5.
## Merging dataset 5 of 5.
## Merging MCGM1 took 0 seconds or 0 minutes.
## Merging MSAM1. Type 2 of 3.
## Merging dataset 1 of 5.
## Merging dataset 2 of 5.
## Merging dataset 3 of 5.
## Merging dataset 4 of 5.
## Merging dataset 5 of 5.
## Merging MSAM1 took 0 seconds or 0 minutes.
## Merging MSGM1. Type 3 of 3.
## Merging dataset 1 of 5.
## Merging dataset 2 of 5.
## Merging dataset 3 of 5.
## Merging dataset 4 of 5.
## Merging dataset 5 of 5.
## Merging MSGM1 took 0 seconds or 0 minutes.
## Merging took 0 seconds or 0 minutes.
## 2  ILSAmerge()/ILSArename() student file(s) found. Combining into 1 file(s).
## Combining student 1 of 1.
## Combining students took 0 seconds or 0 minutes.
## 2  ILSAmerge()/ILSArename() file(s) found. Adding schools to 1 file(s).
## Combined students found for MSAM1.
## Adding schools to MSAM1, file 1 of 1.
## Adding schools took 0 seconds or 0 minutes.
## 3  ILSAmerge() file(s) found.
## 3  ILSAmerge() file(s) renamed.
## Preparing data took 0 seconds or 0.01 minutes.
```
