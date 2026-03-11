# Identify ILSA files

It is common that ILSA files come an unaggregated format, i.e.,
separated by country. If that is the case
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
is able to identify which files should be merged together.

If we want know how many files were downloaded and into which many files
they can be merged, we can use
[`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md)
targeting the folder with the unzipped data:

``` r

ILSAfile.info(inputdir = system.file("extdata/reds", package = "ILSAmerge"))
##   Population Files  MB
## 1      BCGV1     4 0.3
## 2      BSGV1     4 0.4
## 3      BTGV1     4 0.3
```

As we can see, for ‘REDS’ 2021 we have 3 file ‘types’ or populations:
`BCGV1` (schools), `BSGV1` (students), and `BTGV1` (teachers). This will
become important when we need to work with only one population.
