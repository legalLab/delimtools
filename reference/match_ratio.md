# Compute Agreement Between Alternative Species Delimitation Partitions

`match_ratio()` uses the Match Ratio statistic of Ahrens et al. (2014)
to compute agreement between all pairs of species delimitation
partitions in [delim_join](delim_join.md) output.

## Usage

``` r
match_ratio(delim, sorted = TRUE)
```

## Source

Ahrens D., Fujisawa T., Krammer H. J., Eberle J., Fabrizi S., Vogler A.
P. 2016. Rarity and Incomplete Sampling in DNA-Based Species
Delimitation. *Systematic Biology* 65 (3): 478-494.
[doi:10.1093/sysbio/syw002](https://doi.org/10.1093/sysbio/syw002)

## Arguments

- delim:

  Output from [delim_join](delim_join.md).

- sorted:

  If output should be sorted by match ratio values. Default to TRUE.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`match_ratio()` iterates between all species delimitation partitions in
[delim_join](delim_join.md) output and returns a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing the following columns:

- `pairs` pairs of species delimitation methods analyzed.

- `delim_1` number of species partitions in method 1.

- `delim_2` number of species partitions in method 2.

- `n_match` number of identical species partitions in methods 1 and 2.

- `match_ratio` match ratio statistic, where 0 indicates no agreement
  between pairs of species delimitation partitions and 1 indicates
  complete agreement between them.

## Author

Pedro S. Bittencourt

## Examples

``` r

# estimate match ratio statistics
match_ratio(geophagus_delims)
#> # A tibble: 28 × 5
#>    pairs        delim_1 delim_2 n_match match_ratio
#>    <chr>          <int>   <int>   <int>       <dbl>
#>  1 abgd-locmin       19      21      17        0.85
#>  2 bgmyc-gmyc        18      21      15        0.77
#>  3 abgd-bgmyc        19      18      14        0.76
#>  4 bgmyc-ptp         18      17      13        0.74
#>  5 abgd-ptp          19      17      13        0.72
#>  6 asap-ptp          14      17      11        0.71
#>  7 gmyc-locmin       21      21      15        0.71
#>  8 gmyc-ptp          21      17      13        0.68
#>  9 asap-morph        14      16      10        0.67
#> 10 bgmyc-locmin      18      21      13        0.67
#> # ℹ 18 more rows
```
