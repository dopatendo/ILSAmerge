# Read ILSA data

Reads files created with ILSAmerge().

## Usage

``` r
readILSA(file, mistoNAs = FALSE, untibble = FALSE)
```

## Arguments

- file:

  a path to an '.rds', '.sav', or '.zsav' file.

- mistoNAs:

  a logical value indicating if missing values should be converted into
  NAs. Default is `FALSE`.

- untibble:

  a logical value indicating if data should be converted into a plain
  data frame with no column attributes.

## Value

A tibble or a data frame.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Path where merged files will be saved
unlink(file.path(tempdir(),"ILSAmerge"),recursive = TRUE)
dir.create(file.path(tempdir(),"ILSAmerge"))
output <- file.path(tempdir(),"ILSAmerge")

# Merging 'REDS' 2021, as .rds file
ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
#> 12 files detected. Merging into 3 files.
#> Merging BCGV1. Type 1 of 3.
#> Merging dataset 1 of 4.
#> Merging dataset 2 of 4.
#> Merging dataset 3 of 4.
#> Merging dataset 4 of 4.
#> Merging BCGV1 took 0 seconds or 0 minutes.
#> Merging BSGV1. Type 2 of 3.
#> Merging dataset 1 of 4.
#> Merging dataset 2 of 4.
#> Merging dataset 3 of 4.
#> Merging dataset 4 of 4.
#> Merging BSGV1 took 0 seconds or 0 minutes.
#> Merging BTGV1. Type 3 of 3.
#> Merging dataset 1 of 4.
#> Merging dataset 2 of 4.
#> Merging dataset 3 of 4.
#> Merging dataset 4 of 4.
#> Merging BTGV1 took 0 seconds or 0 minutes.
#> Merging took 0 seconds or 0 minutes.

# Read student file
readILSA(file = file.path(output,"BSGV1.rds"))
#> # A tibble: 40 × 336
#>    CNTRY IDCNTRY_STR          IDCNTRY         IDSCHOOL IDSTUD SEX       ASDAGE  
#>    <chr> <chr>                <dbl+lbl>       <dbl+lb> <dbl+> <dbl+lbl> <dbl+lb>
#>  1 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13      
#>  2 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  12.7    
#>  3 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.4    
#>  4 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.8    
#>  5 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.4    
#>  6 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.4    
#>  7 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.2    
#>  8 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.8    
#>  9 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  13.8    
#> 10 ARE   United Arab Emirates 784 [United Ar… 1001     1.00e7 2 [Male]  12.2    
#> # ℹ 30 more rows
#> # ℹ 329 more variables: ITLANGS <dbl+lbl>, IS1G01 <dbl+lbl>, IS1G02A <dbl+lbl>,
#> #   IS1G02B <dbl+lbl>, IS1G02C <dbl+lbl>, IS1G03 <dbl+lbl>, IS1G04A <dbl+lbl>,
#> #   IS1G04B <dbl+lbl>, IS1G04C <dbl+lbl>, IS1G04D <dbl+lbl>, IS1G04E <dbl+lbl>,
#> #   IS2G04F <dbl+lbl>, IS2G04G <dbl+lbl>, IS1G05A <dbl+lbl>, IS1G05B <dbl+lbl>,
#> #   IS1G05C <dbl+lbl>, IS1G06 <dbl+lbl>, IS1G07A <dbl+lbl>, IS1G07B <dbl+lbl>,
#> #   IS1G07C <dbl+lbl>, IS1G07D <dbl+lbl>, IS1G07E <dbl+lbl>, …
```
