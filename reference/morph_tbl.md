# Generating a Morphological Delimitation Table

`morph_tbl()` returns species partition hypothesis estimated from a
prior taxonomic identifications supplied by the user.

## Usage

``` r
morph_tbl(labels, sppVector, delimname = "morph")
```

## Arguments

- labels:

  Vector of unique sequence ID labels.

- sppVector:

  Vector of corresponding morphological species delimitation groups.

- delimname:

  Character. String to rename the delimitation method in the table.
  Default to 'morph'.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`morph_tbl()` uses information in a species name vector to label each
unique sample with a number corresponding to this name.

## Author

Rupert A. Collins

## Examples

``` r

# create a tibble
morph_df <- morph_tbl(
  labels = geophagus_info$gbAccession,
  sppVector = geophagus_info$scientificName
)

# check
morph_df
#> # A tibble: 354 × 2
#>    labels     morph
#>    <chr>      <int>
#>  1 MZ504486.1     1
#>  2 MZ504531.1     1
#>  3 MZ504560.1     1
#>  4 MZ504525.1     1
#>  5 MZ504604.1     1
#>  6 MZ504501.1     1
#>  7 MZ504559.1     1
#>  8 MZ504504.1     1
#>  9 MZ504512.1     1
#> 10 MZ504546.1     1
#> # ℹ 344 more rows
```
