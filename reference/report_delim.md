# Report Unique Species Partitions

`report_delim()` reports the number of unique species partitions in
`delim`.

## Usage

``` r
report_delim(delim, verbose = TRUE)
```

## Arguments

- delim:

  Output from any `*_tbl()` (e.g. [gmyc_tbl](gmyc_tbl.md)),
  [delim_join](delim_join.md) or [delim_consensus](delim_consensus.md).

- verbose:

  Logical. If TRUE, returns a message and a tabulated summary of
  `delim`.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)\].

## Details

For each column in `delim`, `report_delim()` will calculate the number
of unique partitions and print them to `Console`, using
[n_distinct](https://dplyr.tidyverse.org/reference/n_distinct.html) with
`na.rm = TRUE` whether `delim` is a single-method output from `*_tbl()`
or a multi-method output from [delim_join](delim_join.md) or
[delim_consensus](delim_consensus.md). This is to prevent any columns
with NA values from being interpreted as species partitions.

## Author

Rupert A. Collins, Pedro S. Bittencourt

## Examples

``` r

# report geophagus delimitations
report_delim(geophagus_delims)
#> ℹ Joined delimitations have a total of 43 unique species partitions.
#> ℹ Check below the number of species partitions per method:
#> 
#> 
#> |method | partitions|
#> |:------|----------:|
#> |gmyc   |         21|
#> |locmin |         21|
#> |abgd   |         19|
#> |bgmyc  |         18|
#> |ptp    |         17|
#> |morph  |         16|
#> |asap   |         14|
#> |mptp   |         11|
#> # A tibble: 137 × 9
#>    labels     abgd  asap  bgmyc gmyc  locmin morph mptp  ptp  
#>    <chr>      <chr> <chr> <chr> <chr> <chr>  <chr> <chr> <chr>
#>  1 MZ504301.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  2 MZ504318.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  3 MZ504341.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  4 MZ504337.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  5 MZ504342.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  6 MZ504304.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  7 MZ504332.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  8 MZ504343.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  9 MZ504315.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#> 10 MZ504345.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#> # ℹ 127 more rows
```
