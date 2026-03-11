# Untibble

Converts a tibble into a plain data frame with no column attributes.

## Usage

``` r
untibble(tibble, mistoNAs = FALSE)
```

## Arguments

- tibble:

  a tibble object or a list of tibbles.

- mistoNAs:

  a logical value indicating if missing values should be converted into
  NAs. Default is `FALSE`.

## Value

A tibble.

## Examples

``` r
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Load complete data
fulllist <- justload(inputdir = input, population = "BCGV1", justattributes = FALSE)

# Untibble first element
unt1 <- untibble(fulllist[[1]])

# Untibble all list
unt2 <- untibble(fulllist)

```
