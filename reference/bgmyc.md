# Public API Run Bayesian GMYC species delimitation

Runs a Bayesian implementation of the General Mixed Yule-Coalescent
(bGMYC) model using the embedded C GMYC engine. Likelihood evaluations
are performed in C (via `C_gmyc_loglik`), eliminating the dependency on
the external bGMYC package.

## Usage

``` r
bgmyc(
  tree,
  mcmc = 11000L,
  burnin = 1000L,
  thinning = 100L,
  py1 = 0,
  py2 = 2,
  pc1 = 0,
  pc2 = 2,
  t1 = 2L,
  t2 = NULL,
  scale = c(20, 10, 5),
  start = c(1, 0.5, 50),
  ppcutoff = 0.05,
  quiet = FALSE
)
```

## Arguments

- tree:

  An ultrametric, fully binary
  [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html) object.

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

- ppcutoff:

  Numeric. Posterior co-occurrence probability threshold used to compute
  the point-estimate `$assignments`. Tip pairs with posterior
  probability of co-occurrence \\\geq\\ `ppcutoff` are merged into the
  same species (default 0.05). Can be recomputed later via
  [`bgmyc_tbl`](bgmyc_tbl.md).

- quiet:

  Logical. Suppress progress output (default `FALSE`).

## Value

An object of class `"bgmyc_fit"`, a named list with:

- `par`:

  Matrix of retained MCMC samples (rows = samples, columns `py`, `pc`,
  `t`, `loglik`).

- `assignments`:

  Data frame with columns `labels` (tip labels) and `bgmyc` (integer
  species index), giving the point-estimate partition at `ppcutoff`.

- `probmat`:

  Numeric matrix (`n_tips * n_tips`) of posterior co-occurrence
  probabilities. Use with [`bgmyc_tbl`](bgmyc_tbl.md) to recompute the
  partition at any cutoff.

- `accept_rates`:

  Named numeric vector of per-parameter acceptance rates (`py`, `pc`,
  `t`).

- `mcmc`, `burnin`, `thinning`:

  Integer scalars.

- `tree`:

  The input `phylo` object.

## Details

The Metropolis-within-Gibbs sampler follows Reid & Carstens (2012): a
gamma proposal for `py` and `pc`, and a Gaussian random walk for the
integer threshold `t`. The prior is uniform on
`[py1, py2] * [pc1, pc2] * {t1, ..., t2}`.

## References

Reid N.M., Carstens B.C. 2012. Phylogenetic estimation error can
decrease the accuracy of species delimitation: a Bayesian implementation
of the general mixed Yule-coalescent model. *BMC Evolutionary Biology*
12, 196.

## See also

[`bgmyc_tbl`](bgmyc_tbl.md), [`gmyc`](gmyc.md),
[`mptp_mcmc`](mptp_mcmc.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ape)
phy <- as.phylo(geophagus_beast)
result <- bgmyc(phy, mcmc = 11000, burnin = 1000, thinning = 100,
                start = c(1, 0.5, 30))
summary(result)
head(result$assignments)
bgmyc_tbl(result, ppcutoff = 0.95)
} # }
```
