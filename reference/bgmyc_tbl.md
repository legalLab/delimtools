# Turns bGMYC Results Into a Tibble

`bgmyc_tbl()` processes output from [`bgmyc()`](bgmyc.md) (class
`"bgmyc_fit"`) or
[bgmyc.singlephy](https://rdrr.io/pkg/bGMYC/man/bgmyc.singlephy.html)
(class `"singlebgmyc"`) into an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

For `"bgmyc_fit"` objects the partition is derived from the stored
posterior co-occurrence matrix (`$probmat`) by grouping tip pairs whose
probability of co-occurrence exceeds `ppcutoff` (union-find). This lets
you explore different thresholds without re-running the MCMC.

For legacy `"singlebgmyc"` objects the original bGMYC functions
[spec.probmat](https://rdrr.io/pkg/bGMYC/man/spec.probmat.html) and
[bgmyc.point](https://rdrr.io/pkg/bGMYC/man/bgmyc.point.html) are called
(requires the bGMYC package to be installed).

## Usage

``` r
bgmyc_tbl(bgmyc_res, ppcutoff = 0.05, delimname = "bgmyc")
```

## Source

Reid N.M., Carstens B.C. 2012. Phylogenetic estimation error can
decrease the accuracy of species delimitation: a Bayesian implementation
of the general mixed Yule-coalescent model. BMC Evolutionary Biology 12
(196).

## Arguments

- bgmyc_res:

  Output from [`bgmyc()`](bgmyc.md) or
  [bgmyc.singlephy](https://rdrr.io/pkg/bGMYC/man/bgmyc.singlephy.html).

- ppcutoff:

  Posterior co-occurrence probability threshold. Tip pairs with
  posterior probability of being conspecific \\\geq\\ `ppcutoff` are
  merged into the same species. Default 0.05.

- delimname:

  Character. Column name for the species index in the returned tibble.
  Default `"bgmyc"`.

## Value

An object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html) with
columns `labels` (tip labels, in tree-tip order) and the species index
named by `delimname`.

## See also

[`bgmyc()`](bgmyc.md), [`gmyc_tbl()`](gmyc_tbl.md)

## Author

Noah M. Reid (original bGMYC); Pedro S. Bittencourt (delimtools
wrapper).

## Examples

``` r

# \donttest{
# bgmyc (no external package required)
result <- try( bgmyc(ape::as.phylo(geophagus_beast),
  mcmc = 11000, burnin = 1000, thinning = 100,
  start = c(1, 0.5, 30), quiet = TRUE
))
bgmyc_df <- try( bgmyc_tbl(result, ppcutoff = 0.05) )
try(bgmyc_df)
#> # A tibble: 137 × 2
#>    labels     bgmyc
#>    <chr>      <int>
#>  1 GU701784.1     1
#>  2 GU701785.1     1
#>  3 JN988869.1     1
#>  4 MH780911.1     1
#>  5 MZ050845.1     2
#>  6 MZ051032.1     2
#>  7 MZ051272.1     3
#>  8 MZ051516.1     3
#>  9 MZ051549.1     3
#> 10 MZ051706.1     2
#> # ℹ 127 more rows

# legacy bGMYC object
bgmyc_res <- try( bGMYC::bgmyc.singlephy(ape::as.phylo(geophagus_beast),
  mcmc = 11000, burnin = 1000, thinning = 100,
  t1 = 2, t2 = ape::Ntip(geophagus_beast),
  start = c(1, 0.5, 50)
))
#> You are running bGMYC on a single phylogenetic tree.
#> This tree contains  137  tips.
#> The Yule process rate change parameter has a uniform prior ranging from  0  to  2 .
#> The coalescent process rate change parameter has a uniform prior ranging from  0  to  2 .
#> The threshold parameter, which is equal to the number of species, has a uniform prior ranging from  2  to  137 . The upper bound of this prior should not be more than the number of tips in your trees.
#> The MCMC will start with the Yule parameter set to  1 .
#> The MCMC will start with the coalescent parameter set to  0.5 .
#> The MCMC will start with the threshold parameter set to  50 . If this number is greater than the number of tips in your tree, an error will result.
#> Given your settings for mcmc, burnin and thinning, your analysis will result in  100  samples being retained.
#> 10 % 
#> 20 % 
#> 30 % 
#> 40 % 
#> 50 % 
#> 60 % 
#> 80 % 
#> 90 % 
#> 100 % 
#> acceptance rates 
#>  py pc th 
#>  0.5471818 0.5499091 0.2346364 
bgmyc_df2 <- try( bgmyc_tbl(bgmyc_res, ppcutoff = 0.05) )
try(bgmyc_df2)
#> # A tibble: 137 × 2
#>    labels     bgmyc
#>    <chr>      <int>
#>  1 GU701784.1     1
#>  2 GU701785.1     1
#>  3 JN988869.1     1
#>  4 MH780911.1     1
#>  5 OR732927.1     1
#>  6 OR732928.1     1
#>  7 MZ050845.1     2
#>  8 MZ051032.1     2
#>  9 MZ051706.1     2
#> 10 MZ051794.1     2
#> # ℹ 127 more rows
# }
```
