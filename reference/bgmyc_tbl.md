# Turns bGMYC Results Into a Tibble

`bgmyc_tbl()` processes output from [`bgmyc()`](bgmyc.md) (class
`"bgmyc_fit"`) or
[`bGMYC::bgmyc.singlephy()`](https://rdrr.io/pkg/bGMYC/man/bgmyc.singlephy.html)
(class `"singlebgmyc"`) into an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

For `"bgmyc_fit"` objects the partition is derived from the stored
posterior co-occurrence matrix (`$probmat`) by grouping tip pairs whose
probability of co-occurrence exceeds `ppcutoff` (union-find). This lets
you explore different thresholds without re-running the MCMC.

For legacy `"singlebgmyc"` objects the original bGMYC functions
`spec.probmat()` and `bgmyc.point()` are reimplemented natively, so the
bGMYC package is not required.

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
  [`bGMYC::bgmyc.singlephy()`](https://rdrr.io/pkg/bGMYC/man/bgmyc.singlephy.html).

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

Pedro S. Bittencourt

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

# legacy `singlebgmyc` object, as produced by the original bGMYC package
# (bGMYC itself is not required to build this example): `par` holds
# post-burnin (py, pc, t) draws, and `mrca[[t]]` lists the MRCA node
# offsets (APE node number - Ntip) merged into one species at threshold t.
tr <- ape::as.phylo(geophagus_beast)
mrca_node <- ape::getMRCA(tr, tr$tip.label[1:2]) - ape::Ntip(tr)
legacy_res <- structure(
  list(tree = tr, par = cbind(py = 1, pc = 1, t = 1L), mrca = list(mrca_node)),
  class = "singlebgmyc"
)
bgmyc_df2 <- try( bgmyc_tbl(legacy_res, ppcutoff = 0.5) )
try(bgmyc_df2)
#> # A tibble: 137 × 2
#>    labels     bgmyc
#>    <chr>      <int>
#>  1 GU701784.1     1
#>  2 GU701785.1     1
#>  3 JN988869.1     1
#>  4 MH780911.1     1
#>  5 MZ050845.1     2
#>  6 MZ051032.1     3
#>  7 MZ051272.1     4
#>  8 MZ051516.1     5
#>  9 MZ051549.1     6
#> 10 MZ051706.1     7
#> # ℹ 127 more rows
# }
```
