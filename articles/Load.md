# Load ILSA files

## Check what can be loaded

Using
[`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md)
we can identify different populations within an ILSA.

Moreover, we can also use this information for loading an specific
population into the memory. Using the ‘REDS’ 2021 example data provided
by this package:

``` r

ILSAfile.info(inputdir = system.file("extdata/reds", package = "ILSAmerge"))
##   Population Files  MB
## 1      BCGV1     4 0.3
## 2      BSGV1     4 0.4
## 3      BTGV1     4 0.3
```

## Load all information

We can decide to load a population, like `BCGV1`, using
[`justload()`](https://dopatendo.github.io/ILSAmerge/reference/justload.md):

``` r
loaded <- justload(inputdir = system.file("extdata/reds", package = "ILSAmerge"),
         population = "BCGV1")
```

This will result in a list of elements loaded by `haven`, in our
example, a list of 11 elements:

``` r
class(loaded)
## [1] "list"

length(loaded)
## [1] 4

loaded[[1]]
## # A tibble: 10 × 314
##    IDCNTRY             IDSCHOOL ITLANGC      IP1G00A   IP1G00B      IP1G00C     
##    <dbl+lbl>           <dbl+lb> <dbl+lbl>    <dbl+lbl> <dbl+lbl>    <dbl+lbl>   
##  1 784 [United Arab E… 1001     53 [Arabic]  2 [Mid]   3 [March]    9 (NA) [Omi…
##  2 784 [United Arab E… 1002     53 [Arabic]  1 [Early] 2 [February] 3 [Late]    
##  3 784 [United Arab E… 1003     53 [Arabic]  1 [Early] 2 [February] 3 [Late]    
##  4 784 [United Arab E… 1004      1 [English] 3 [Late]  2 [February] 3 [Late]    
##  5 784 [United Arab E… 1005     53 [Arabic]  1 [Early] 3 [March]    3 [Late]    
##  6 784 [United Arab E… 1006     53 [Arabic]  2 [Mid]   2 [February] 2 [Mid]     
##  7 784 [United Arab E… 1007     53 [Arabic]  1 [Early] 3 [March]    3 [Late]    
##  8 784 [United Arab E… 1008     53 [Arabic]  1 [Early] 3 [March]    2 [Mid]     
##  9 784 [United Arab E… 1009     53 [Arabic]  2 [Mid]   3 [March]    1 [Early]   
## 10 784 [United Arab E… 1010     53 [Arabic]  2 [Mid]   6 [June]     3 [Late]    
## # ℹ 308 more variables: IP1G00D <dbl+lbl>, IP1GIAA <dbl+lbl>,
## #   IP1GIAB <dbl+lbl>, IP1GIAC <dbl+lbl>, IP1GIAD <dbl+lbl>, IP1GIAE <dbl+lbl>,
## #   IP1GIAF <dbl+lbl>, IP1GIAG <dbl+lbl>, IP1GIBA <dbl+lbl>, IP1GIBB <dbl+lbl>,
## #   IP1GIBC <dbl+lbl>, IP1GIBD <dbl+lbl>, IP1G01A <dbl+lbl>, IP1G01B <dbl+lbl>,
## #   IP1G01C1 <dbl+lbl>, IP1G01C2 <dbl+lbl>, IP2G01A1 <dbl+lbl>,
## #   IP1G01AA <dbl+lbl>, IP1G02A <dbl+lbl>, IP1G02B <dbl+lbl>,
## #   IP1G02C <dbl+lbl>, IP1G02D <dbl+lbl>, IP1G02E <dbl+lbl>, …
```

## Load only column information

For some purposes, we might need to load only the column information,
most probably to check if the attributes are correct. We can do this by
setting `justattributes = TRUE`:

``` r
loaded <- justload(inputdir = system.file("extdata/reds", package = "ILSAmerge"),
         population = "BCGV1", justattributes = TRUE)
```

This will load all 11 datasets with 0 rows each:

``` r
length(loaded)
## [1] 4

loaded[[1]]
## # A tibble: 0 × 314
## # ℹ 314 variables: IDCNTRY <dbl+lbl>, IDSCHOOL <dbl+lbl>, ITLANGC <dbl+lbl>,
## #   IP1G00A <dbl+lbl>, IP1G00B <dbl+lbl>, IP1G00C <dbl+lbl>, IP1G00D <dbl+lbl>,
## #   IP1GIAA <dbl+lbl>, IP1GIAB <dbl+lbl>, IP1GIAC <dbl+lbl>, IP1GIAD <dbl+lbl>,
## #   IP1GIAE <dbl+lbl>, IP1GIAF <dbl+lbl>, IP1GIAG <dbl+lbl>, IP1GIBA <dbl+lbl>,
## #   IP1GIBB <dbl+lbl>, IP1GIBC <dbl+lbl>, IP1GIBD <dbl+lbl>, IP1G01A <dbl+lbl>,
## #   IP1G01B <dbl+lbl>, IP1G01C1 <dbl+lbl>, IP1G01C2 <dbl+lbl>,
## #   IP2G01A1 <dbl+lbl>, IP1G01AA <dbl+lbl>, IP1G02A <dbl+lbl>, …
```
