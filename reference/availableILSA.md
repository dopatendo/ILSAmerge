# Check available ILSA data

Checks which 'SPSS' data from different International Large-Scale
Assessments (ILSA). are available.

## Usage

``` r
availableILSA(
  print = TRUE,
  FOR = c("download", "combine.students", "add.schools", "ILSAready")
)
```

## Arguments

- print:

  a logical value indicating if results should be printed or not.

- FOR:

  a string indicating the availability of ILSA data for different
  purposes. Valid strings are `"download"`, `"combine.students"`,
  `add.schools`, and `"ILSAready"`.

## Value

A list with the names of the ILSA and the available years.

## Examples

``` r
availableILSA(print = TRUE)
#> CIVED: 1999.
#> ICCS: 2009, 2016, 2022.
#> ICILS: 2013, 2018, 2020, 2023.
#> LANA: 2023.
#> PIRLS: 2001, 2006, 2011, 2016, 2021.
#> REDS: 2021.
#> RLII: 1991, 2001.
#> SITES: 1998, 2006.
#> TALIS: 2008, 2013, 2018.
#> TIMSS: 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023.
#> TIMSSADVANCED: 1995, 2008, 2015.
```
