# Run mPTP species delimitation (MCMC)

Calls the mPTP MCMC engine directly via
[`.Call()`](https://rdrr.io/r/base/CallExternal.html). Returns ML
species assignments plus per-node posterior support values.

## Usage

``` r
mptp_mcmc(
  tree,
  method = c("multi", "single"),
  mcmc_steps = 1000000L,
  mcmc_sample = 1000L,
  mcmc_burnin = 100000L,
  mcmc_runs = 1L,
  mcmc_credible = 0.95,
  mcmc_startnull = FALSE,
  mcmc_startrandom = FALSE,
  mcmc_startml = FALSE,
  outgroup = NULL,
  outgroup_crop = FALSE,
  pvalue = 0.001,
  minbr = 1e-04,
  seed = as.integer(Sys.time()),
  quiet = FALSE
)
```

## Arguments

- tree:

  A path to a Newick tree file (character) or a `phylo` object from ape.
  See [`mptp`](mptp.md) for details.

- method:

  Coalescent rate model: `"multi"` (default) or `"single"`.

- mcmc_steps:

  Integer. Total MCMC steps per run (default 1,000,000).

- mcmc_sample:

  Integer. Sampling frequency (default 1,000).

- mcmc_burnin:

  Integer. Steps discarded as burn-in (default 100,000).

- mcmc_runs:

  Integer. Number of independent runs (default 1).

- mcmc_credible:

  Numeric. Credible interval (default 0.95).

- mcmc_startnull:

  Logical. Start from the null (one-species) model.

- mcmc_startrandom:

  Logical. Start from a random delimitation.

- mcmc_startml:

  Logical. Start from the ML delimitation.

- outgroup:

  Character. Outgroup taxon name for rooting (optional).

- outgroup_crop:

  Logical. Remove outgroup after rooting (default `FALSE`).

- pvalue:

  Numeric. LRT significance threshold (default 0.001).

- minbr:

  Numeric. Minimum branch length threshold (default 0.0001).

- seed:

  Integer. Random seed (default: current time).

- quiet:

  Logical. Suppress C-level progress messages (default `FALSE`).

## Value

An object of classes `c("mptp_mcmc", "mptp_ml")`, a list with all fields
of `"mptp_ml"` plus:

- `support`:

  Data frame with columns `node_label` and `support` (0–1); posterior
  probability of each inner node being a coalescent root.

- `mcmc_steps`:

  Integer.

- `mcmc_sample`:

  Integer.

- `mcmc_burnin`:

  Integer.

- `mcmc_runs`:

  Integer.

- `seed`:

  Integer. Actual seed used.

## See also

[`mptp`](mptp.md), [`mptp_tbl`](mptp_tbl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mptp_mcmc(
  "tree.nwk",
  mcmc_steps    = 5000000L,
  mcmc_burnin   = 500000L,
  mcmc_runs     = 3L,
  seed          = 42L,
  outgroup      = "Outgroup1",
  outgroup_crop = TRUE
)
print(result)
head(result$support)
} # }
```
