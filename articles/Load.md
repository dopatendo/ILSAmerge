# Load and prepare ILSA files

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

## Read a single file

We can also read any SPSS file or any file produced by
[`ILSAmerge()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAmerge.md)
by using
[`readILSA()`](https://dopatendo.github.io/ILSAmerge/reference/readILSA.md).
For example, to read the student questionnaire of Burkina Faso in REDS
2021:

``` r

dirdat <- system.file("extdata/reds", package = "ILSAmerge")

bfa <- readILSA(file.path(dirdat,"bsgbfav1.sav"))

bfa
## # A tibble: 10 × 334
##    IDCNTRY            IDSCHOOL IDSTUD SEX        ASDAGE  ITLANGS    IS1G01      
##    <dbl+lbl>          <dbl+lb> <dbl+> <dbl+lbl>  <dbl+l> <dbl+lbl>  <dbl+lbl>   
##  1 854 [Burkina Faso] 1001     1.00e7 1 [Female] 13.8    2 [French] 0 [I did no…
##  2 854 [Burkina Faso] 1001     1.00e7 1 [Female] 18.3    2 [French] 0 [I did no…
##  3 854 [Burkina Faso] 1001     1.00e7 1 [Female] 14.2    2 [French] 0 [I did no…
##  4 854 [Burkina Faso] 1001     1.00e7 1 [Female] 13.8    2 [French] 0 [I did no…
##  5 854 [Burkina Faso] 1001     1.00e7 1 [Female] 15.3    2 [French] 0 [I did no…
##  6 854 [Burkina Faso] 1001     1.00e7 1 [Female] 15      2 [French] 0 [I did no…
##  7 854 [Burkina Faso] 1001     1.00e7 1 [Female] 13.9    2 [French] 0 [I did no…
##  8 854 [Burkina Faso] 1001     1.00e7 2 [Male]   12.4    2 [French] 0 [I did no…
##  9 854 [Burkina Faso] 1001     1.00e7 2 [Male]   16.3    2 [French] 0 [I did no…
## 10 854 [Burkina Faso] 1001     1.00e7 2 [Male]   14.1    2 [French] 0 [I did no…
## # ℹ 327 more variables: IS1G02A <dbl+lbl>, IS1G02B <dbl+lbl>,
## #   IS1G02C <dbl+lbl>, IS1G03 <dbl+lbl>, IS1G04A <dbl+lbl>, IS1G04B <dbl+lbl>,
## #   IS1G04C <dbl+lbl>, IS1G04D <dbl+lbl>, IS1G04E <dbl+lbl>, IS2G04F <dbl+lbl>,
## #   IS2G04G <dbl+lbl>, IS1G05A <dbl+lbl>, IS1G05B <dbl+lbl>, IS1G05C <dbl+lbl>,
## #   IS1G06 <dbl+lbl>, IS1G07A <dbl+lbl>, IS1G07B <dbl+lbl>, IS1G07C <dbl+lbl>,
## #   IS1G07D <dbl+lbl>, IS1G07E <dbl+lbl>, IS1G07F <dbl+lbl>, IS1G07G <dbl+lbl>,
## #   IS1G07H <dbl+lbl>, IS1G07I <dbl+lbl>, IS2G07J <dbl+lbl>, …
```

Meanwhile, for reading a merged ILSA file, we run also
[`readILSA()`](https://dopatendo.github.io/ILSAmerge/reference/readILSA.md)
but on the merged file:

``` r
# merge data
ILSAmerge(inputdir = dirdat,outputdir = tempdir(),filetype = "sav")
## 12 files detected. Merging into 3 files.
## Merging BCGV1. Type 1 of 3.
## Merging dataset 1 of 4.
## Merging dataset 2 of 4.
## Merging dataset 3 of 4.
## Merging dataset 4 of 4.
## Merging BCGV1 took 0 seconds or 0 minutes.
## Merging BSGV1. Type 2 of 3.
## Merging dataset 1 of 4.
## Merging dataset 2 of 4.
## Merging dataset 3 of 4.
## Merging dataset 4 of 4.
## Merging BSGV1 took 0 seconds or 0 minutes.
## Merging BTGV1. Type 3 of 3.
## Merging dataset 1 of 4.
## Merging dataset 2 of 4.
## Merging dataset 3 of 4.
## Merging dataset 4 of 4.
## Merging BTGV1 took 0 seconds or 0 minutes.
## Merging took 0 seconds or 0 minutes.
# rename merged files
ILSArename(inputdir = tempdir())
## 3  ILSAmerge() file(s) found.
## 3  ILSAmerge() file(s) renamed.

stu <- readILSA(file.path(tempdir(),"REDS_2021_student.sav"))

stu
## # A tibble: 40 × 336
##    CNTRY IDCNTRY_STR   IDCNTRY   IDSCHOOL IDSTUD SEX       ASDAGE   ITLANGS    
##    <chr> <chr>         <dbl+lbl> <dbl+lb> <dbl+> <dbl+lbl> <dbl+lb> <dbl+lbl>  
##  1 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13       53 [Arabic]
##  2 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  12.7     53 [Arabic]
##  3 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.4     53 [Arabic]
##  4 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.8     53 [Arabic]
##  5 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.4     53 [Arabic]
##  6 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.4     53 [Arabic]
##  7 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.2     53 [Arabic]
##  8 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.8     53 [Arabic]
##  9 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  13.8     53 [Arabic]
## 10 ARE   United Arab … 784 [Uni… 1001     1.00e7 2 [Male]  12.2     53 [Arabic]
## # ℹ 30 more rows
## # ℹ 328 more variables: IS1G01 <dbl+lbl>, IS1G02A <dbl+lbl>, IS1G02B <dbl+lbl>,
## #   IS1G02C <dbl+lbl>, IS1G03 <dbl+lbl>, IS1G04A <dbl+lbl>, IS1G04B <dbl+lbl>,
## #   IS1G04C <dbl+lbl>, IS1G04D <dbl+lbl>, IS1G04E <dbl+lbl>, IS2G04F <dbl+lbl>,
## #   IS2G04G <dbl+lbl>, IS1G05A <dbl+lbl>, IS1G05B <dbl+lbl>, IS1G05C <dbl+lbl>,
## #   IS1G06 <dbl+lbl>, IS1G07A <dbl+lbl>, IS1G07B <dbl+lbl>, IS1G07C <dbl+lbl>,
## #   IS1G07D <dbl+lbl>, IS1G07E <dbl+lbl>, IS1G07F <dbl+lbl>, …
```

## Remove tibble attributes

As we can see, `bfa`, and `stu` are both tibbles produced by package
`haven`. Nonetheless, this type of objects can produce some errors while
using other packages for analysis. Therefore, we should always create
another object that is a simple data frame so we can treat directly with
the numeric data, always trying also to remove all labeled missing
values. For this, we use
[`untibble()`](https://dopatendo.github.io/ILSAmerge/reference/untibble.md):

``` r

bfa2 <- untibble(tibble = bfa, mistoNAs = TRUE)

stu2 <- untibble(tibble = stu, mistoNAs = TRUE)

class(bfa)
## [1] "tbl_df"     "tbl"        "data.frame"
class(bfa2)
## [1] "data.frame"

class(stu)
## [1] "tbl_df"     "tbl"        "data.frame"
class(stu2)
## [1] "data.frame"
```
