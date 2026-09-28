# Confidence Intervals for Species Delimitations Methods

These functions compute confidence intervals for various species
delimitation methods, including GMYC, bGMYC, Local Minima, and mPTP.

## Usage

``` r
gmyc_ci(tr, posterior, method = "single", interval = c(0, 5))

bgmyc_ci(
  tr,
  posterior,
  ppcutoff = 0.05,
  mcmc,
  burnin,
  thinning,
  py1 = 0,
  py2 = 2,
  pc1 = 0,
  pc2 = 2,
  t1 = 2,
  t2 = 51,
  scale = c(20, 10, 5),
  start = c(1, 0.5, 50)
)

locmin_ci(dna, block = 1, reps = 100, threshold = 0.01, haps = NULL, ...)

mptp_ci(
  infile,
  bootstraps,
  exe = NULL,
  outfolder = NULL,
  method = c("multi", "single"),
  minbrlen = 1e-04,
  webserver = NULL
)
```

## Arguments

- tr:

  A `phylo` object (ape); the maximum-likelihood (point-estimate) tree.

- posterior:

  Trees from posterior. An object of class
  [multiphylo](https://rdrr.io/pkg/ape/man/multiphylo.html).

- method:

  Which algorithm for Maximum Likelihood point-estimate to be used.
  Available options are:

  - single Single-rate PTP model. It assumes that every species evolved
    with the same rate.

  - multi Multi-rate mPTP model. It assumes that all species have
    different evolutionary rates.

- interval:

  Optimisation interval for the null model scaling exponent; see
  [gmyc](gmyc.md).

- ppcutoff:

  Posterior probability threshold for clustering samples into species
  partitions. See [`bgmyc_tbl()`](bgmyc_tbl.md) for details. Default to
  0.05.

- mcmc:

  Integer. Total MCMC steps (default 11 000).

- burnin:

  Integer. Steps discarded as burn-in (default 1 000).

- thinning:

  Integer. Thinning interval; every `thinning`-th post-burnin step is
  retained (default 100).

- py1, py2:

  Numeric. Prior bounds for the Yule rate-change exponent (default 0 and
  2).

- pc1, pc2:

  Numeric. Prior bounds for the coalescent rate-change exponent (default
  0 and 2).

- t1, t2:

  Integer. Prior bounds for the threshold parameter (number of species).
  Defaults: `t1 = 2`, `t2 = NULL` (auto: `Nnode - 1`).

- scale:

  Numeric vector of length 3. Proposal scale parameters for `py`, `pc`,
  and `t` (default `c(20, 10, 5)`).

- start:

  Numeric vector of length 3. Starting values for `py`, `pc`, and `t`
  (default `c(1, 0.5, 50)`).

- dna:

  an object of class [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html).

- block:

  integer. Number of columns to be resampled together. Default to 1.

- reps:

  Number of bootstrap replicates. Default to 100.

- threshold:

  Distance cutoff for clustering. Default of 0.01. See
  [localMinima](https://rdrr.io/pkg/spider/man/localMinima.html) for
  details.

- haps:

  Optional. A vector of haplotypes to keep into the
  [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

- ...:

  Further arguments to be passed to
  [dist.dna](https://rdrr.io/pkg/ape/man/dist.dna.html).

- infile:

  Path to tree file in Newick format, or an object of class `"mptp_ml"`
  or `"mptp_mcmc"` returned by [`mptp`](mptp.md) or
  [`mptp_mcmc`](mptp_mcmc.md). When an `mptp_ml` object is supplied the
  remaining arguments are ignored and the assignments are returned
  directly.

- bootstraps:

  Bootstrap trees. An object of class
  [multiphylo](https://rdrr.io/pkg/ape/man/multiphylo.html).

- exe:

  Path to an mPTP executable.

- outfolder:

  Path to output folder. Default to NULL. If not specified, a temporary
  location is used.

- minbrlen:

  Numeric. Branch lengths smaller or equal to the value provided are
  ignored from computations. Default to 0.0001. Use
  [min_brlen](min_brlen.md)for fine tuning.

- webserver:

  A .txt file containing mPTP results obtained from a webserver. Default
  to NULL.

## Value

A vector containing the number of species partitions in `tr`, `dna` or
`infile` followed by the number of partitions in `posterior`, `reps` or
`bootstraps`.

## Details

Both `gmyc_ci` and `bgmyc_ci` can take a very long time to proccess,
depending on how many posterior trees are provided. As an alternative,
these analyses can be sped up significantly by running in parallel using
[plan](https://future.futureverse.org/reference/plan.html).

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
# \donttest{

# gmyc confidence intervals

# compute values using multisession mode
{
  try( future::plan("multisession") )

  gmyc_res <- try( gmyc_ci(ape::as.phylo(geophagus_beast), geophagus_posterior) )

  # reset future parameters
  try( future::plan("sequential") )
}

# plot distribution
try(plot(density(gmyc_res)))


# tabulate
try( tibble::tibble(
  method = "gmyc",
  point_estimate = gmyc_res[1],
  CI_95 = as.integer(quantile(gmyc_res[-1], probs = c(0.025, 0.975))) |>
    stringr::str_flatten(collapse = "-"),
  CI_mean = as.integer(mean(gmyc_res[-1])),
  CI_median = as.integer(stats::median(gmyc_res[-1]))
)
)
#> # A tibble: 1 × 5
#>   method point_estimate CI_95 CI_mean CI_median
#>   <chr>           <int> <chr>   <int>     <int>
#> 1 gmyc               21 3-43       25        23
# }
```
