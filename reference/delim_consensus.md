# Estimate a Majority-Vote Consensus

`delim_consensus()` estimates a majority-vote consensus over the output
of [delim_join](delim_join.md) in a row-wise manner.

## Usage

``` r
delim_consensus(delim, n_match = NULL)
```

## Arguments

- delim:

  Output from [delim_join](delim_join.md).

- n_match:

  An integer. Threshold for Majority-Vote calculations. If not
  specified, returns a warning and the threshold will be defined as
  `ceiling(ncol(delim[, -1])/2)`.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`delim_consensus()` iterates row-by-row, counting the number of matching
species partition names across all species delimitations methods in
[delim_join](delim_join.md) output. If the sum of identical partition
names is greater or equal `n_match`, the consensus column will be filled
with its partition name. Otherwise, consensus column will be filled with
[NA](https://rdrr.io/r/base/NA.html).

## Author

Pedro S. Bittencourt

## Examples

``` r

# estimate a majority vote consensus
delim_consensus <- delim_consensus(geophagus_delims, n_match= 5)

# check
delim_consensus
#> # A tibble: 137 × 10
#>    labels     abgd  asap  bgmyc gmyc  locmin morph mptp  ptp   consensus
#>    <chr>      <chr> <chr> <chr> <chr> <chr>  <chr> <chr> <chr> <chr>    
#>  1 MZ504301.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  2 MZ504318.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  3 MZ504341.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  4 MZ504337.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  5 MZ504342.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  6 MZ504304.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  7 MZ504332.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  8 MZ504343.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#>  9 MZ504315.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#> 10 MZ504345.1 sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1   sp1      
#> # ℹ 127 more rows

```
