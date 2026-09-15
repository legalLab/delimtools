# Run mPTP species delimitation (Maximum Likelihood)

Calls the mPTP C engine directly via
[`.Call()`](https://rdrr.io/r/base/CallExternal.html) — no external
process, no file parsing. Results are returned as a structured R object.

## Usage

``` r
mptp(
  tree,
  method = c("multi", "single"),
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

  A path to a Newick tree file (character) or an
  [`read.tree`](https://rdrr.io/pkg/ape/man/read.tree.html) `phylo`
  object. Rooted and unrooted trees are both accepted.

- method:

  Coalescent rate model: `"multi"` (default, recommended) or `"single"`.

- outgroup:

  Character. Name of the outgroup taxon used for rooting an unrooted
  tree (optional; if omitted, the longest tip branch is used).

- outgroup_crop:

  Logical. If `TRUE`, removes the outgroup after rooting (default
  `FALSE`).

- pvalue:

  Numeric. Significance threshold for the likelihood ratio test (default
  `0.001`).

- minbr:

  Numeric. Minimum branch length; edges shorter than this are excluded
  from the analysis (default `0.0001`).

- seed:

  Integer. Random seed (default: current time).

- quiet:

  Logical. Suppress C-level progress messages (default `FALSE`).

## Value

An object of class `"mptp_ml"`, a list with:

- `n_species`:

  Integer. Number of delimited species.

- `method`:

  Character. `"multi"` or `"single"`.

- `null_logl`:

  Numeric. Null-model log-likelihood.

- `best_logl`:

  Numeric. Best log-likelihood under mPTP.

- `pvalue`:

  Numeric. LRT p-value.

- `lrt_passed`:

  Logical. Whether the LRT rejected the null.

- `edge_count`:

  Integer. Edges longer than `minbr`.

- `total_edges`:

  Integer. Total edges in the tree.

- `assignments`:

  Data frame with columns `labels` and `mptp` (integer index; `ptp` when
  `method = "single"`).

## See also

[`mptp_mcmc`](mptp_mcmc.md), [`mptp_tbl`](mptp_tbl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mptp("tree.nwk", outgroup = "Outgroup1",
                       outgroup_crop = TRUE)
print(result)
head(result$assignments)

library(ape)
phy <- read.tree("tree.nwk")
result <- mptp(phy)
} # }
```
