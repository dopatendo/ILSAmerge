# Loading ILSA data into a list

Load 'SPSS' data from different International Large-Scale Assessments
(ILSA), including: 'TIMSS', 'TIMSS Advanced', 'PIRLS', 'ICCS', 'ICILS',
'CIVED', 'REDS', 'RLII', and 'SITES' (2006) into a list.

## Usage

``` r
justload(
  inputdir = getwd(),
  population,
  justattributes = FALSE,
  addcountries = FALSE
)
```

## Arguments

- inputdir:

  a string indicating the path where ILSA 'SPSS' files are stored.

- population:

  a character value indicating which files should be loaded. For more
  information on available populations, run
  [`ILSAfile.info()`](https://dopatendo.github.io/ILSAmerge/reference/ILSAfile.info.md)
  first.

- justattributes:

  a logical value indicating if 0 rows should be loaded. This can be
  used when we just need to check column attributes. Default is `FALSE`.

- addcountries:

  a logical value indicating if country information should be added to
  the elements of the list. This means adding the variable `CNTRY` where
  needed and adding labels for `IDCNTRY` where needed. If `FALSE` (the
  default), data will be loaded as is. Country information will be
  retrieved from 'GitHub' if possible. If not, it will use the package
  internal data.

## Value

A list of tibbles.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Load only attributes
emptylist <- justload(inputdir = input, population = "BCGV1", justattributes = TRUE)

# Load complete data
fulllist <- justload(inputdir = input, population = "BCGV1", justattributes = FALSE)

# Load complete data and add country labels
withcou <- justload(inputdir = input, population = "BCGV1", addcountries = TRUE)
```
