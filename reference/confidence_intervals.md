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

  An ultrametric, dichotomous tree object in ape format.

- posterior:

  Trees from posterior. An object of class
  [multiphylo](https://rdrr.io/pkg/ape/man/multiphylo.html).

- method:

  Method of analysis, either "single" for single-threshold version or
  "multiple" for multiple-threshold version.

- interval:

  Upper and lower limit of estimation of scaling parameters, e.g.
  c(0,10)

- ppcutoff:

  Posterior probability threshold for clustering samples into species
  partitions. See
  [bgmyc.point](https://rdrr.io/pkg/bGMYC/man/bgmyc.point.html) for
  details. Default to 0.05.

- mcmc:

  number of samples to take from the Markov Chain

- burnin:

  the number of samples to discard as burn-in

- thinning:

  the interval at which samples are retained from the Markov Chain

- py1:

  governs the prior on the Yule (speciation) rate change parameter.
  using the default prior distribution, this is the lower bound of a
  uniform distribution. this can be the most influential prior of the
  three. rate change is parameterized as n^py where n is the number of
  lineages in a waiting interval (see Pons et al. 2006). if there are 50
  sequences in an analysis and the Yule rate change parameter is 2, this
  allows for a potential 50-fold increase in speciation rate. this
  unrealistic parameter value can cause the threshold between Yule and
  Coalescent process to be difficult to distinguish. are more reasonable
  upper bound for the prior would probably be less than 1.5 (a potential
  7-fold increase). Or you could modify the prior function to use a
  different distribution entirely.

- py2:

  governs the prior on the Yule rate change parameter. using the default
  prior distribution, this is the upper bound of a uniform distribution.

- pc1:

  governs the prior on the coalescent rate change parameter. using the
  default prior distribution, this is the lower bound of a uniform
  distribution. rate change is parameterized as (n(n-1))^pc where n is
  the number of lineages in a waiting interval (see Pons et al. 2006).
  In principle pc can be interpreted as change in effective population
  size (pc\<1 decline, pc\>1 growth) but because identical haplotypes
  must be excluded from this analysis an accurate biological
  interpretation is not possible.

- pc2:

  governs the prior on the coalescent rate change parameter. using the
  default prior distribution, this is the upper bound of a uniform
  distribution.

- t1:

  governs the prior on the threshold parameter. the lower bound of a
  uniform distribution. the bounds of this uniform distribution should
  not be below 1 or greater than the number of unique haplotypes in the
  analysis.

- t2:

  governs the prior on the threshold parameter. the upper bound of a
  uniform distribution

- scale:

  a vector of scale parameters governing the proposal distributions for
  the markov chain. the first to are the Yule and coalescent rate change
  parameters. increasing them makes the proposals more conservative. the
  third is the threshold parameter. increasing it makes the proposals
  more liberal.

- start:

  a vector of starting parameters in the same order as the scale
  parameters, py, pc, t. t may need to be set so that it is not
  impossible given the dataset.

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
#> Registered S3 method overwritten by 'splits':
#>   method       from      
#>   summary.gmyc delimtools

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
