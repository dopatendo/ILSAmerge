# Download ILSA files

## Available datasets

Currently [`ILSAmerge`](https://cran.r-project.org/package=ILSAmerge) is
able to download several ILSA datasets, including: ‘PISA’, ‘TALIS’,
‘TIMSS’, ‘TIMSS Advanced’, ‘PIRLS’, ‘ICCS’, ‘ICILS’, ‘CIVED’, ‘REDS’,
‘RLII’, and ‘SITES.’

The links of these datasets are allocated in
[GitHub](https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAlinks.csv),
so links can be corrected if they are down.

With
[`availableILSA()`](https://dopatendo.github.io/ILSAmerge/reference/availableILSA.md),
we can check which datasets are available:

``` r
availableILSA()
## CIVED: 1999.
## ICCS: 2009, 2016, 2022.
## ICILS: 2013, 2018, 2020, 2023.
## LANA: 2023.
## PIRLS: 2001, 2006, 2011, 2016, 2021.
## REDS: 2021.
## RLII: 1991, 2001.
## SITES: 1998, 2006.
## TALIS: 2008, 2013, 2018.
## TIMSS: 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023.
## TIMSSADVANCED: 1995, 2008, 2015.
```

## Download the datasets for a particular study

Depending on the study datasets are uploaded in 1 or more .zip files.
That is why
[`ILSAdownload()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAdownload.md)
will handle by itself how many and which .zip files to download.

As an example, we let us say we want to download the data of ‘RLII’
1991. Then, at least we need to opt for three arguments; `study`,
`year`, and `outputdir`:

``` r
ILSAdownload(study = "RLII", year = 1991, outputdir = tempdir())
```

    ## By accessing the Data Repository, IDB Analyzer and Data visualizer, you indicate that you agree to the terms and conditions associated with their use. Please read the Disclaimer and License Agreement for full details.
    ## License Agreement downloaded.
    ## Do you agree with these terms and conditions?
    ## Please answer TRUE or FALSE.

As we can see, some studies will require that you agree with their
license agreements. If that is the case, you will be required to answer
if you agree or not. In order to continue, you will need to agree. If
you know that you will agree with the study terms, then you can also
avoid this prompt via `agreeLicense = TRUE`:

``` r
ILSAdownload(study = "RLII", year = 1991, outputdir = tempdir(),
             agreeLicense = TRUE)
## By accessing the Data Repository, IDB Analyzer and Data visualizer, you indicate that you agree to the terms and conditions associated with their use. Please read the Disclaimer and License Agreement for full details.
## 1 files found for RLII 1991.
## Visit https://www.iea.nl/data-tools/repository/readingliteracy to know how to use and cite these datasets.
```

## Unzip

This function can also unzip downloaded files using `unzip = TRUE`:

``` r
ILSAdownload(study = "RLII", year = 1991, outputdir = tempdir(),
             unzip = TRUE)
```

## Avoid printing

It is possible to avoid any printing by `quiet = TRUE`, but you will
still be prompted to agree. To avoid both prints you can use both
arguments:

``` r
ILSAdownload(study = "RLII", year = 1991, outputdir = tempdir(),
             quiet = TRUE, agreeLicense = TRUE)
```

## Download time

Timeout times for downloading are controlled by `maxtime`, which you can
set for a desired number of seconds:

``` r
ILSAdownload(study = "RLII", year = 1991, outputdir = tempdir(),
             maxtime = 60)
```
