# Run PTP species delimitation (Maximum Likelihood)

Alias of [`mptp`](mptp.md) with `method` defaulting to `"single"` (the
single-rate PTP model), rather than `mptp`'s own default of `"multi"`
(the multi-rate mPTP model).

## Usage

``` r
ptp(
  tree,
  method = c("single", "multi"),
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

See [`mptp`](mptp.md).

## See also

[`mptp`](mptp.md), [`ptp_tbl`](ptp_tbl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ptp("tree.nwk", outgroup = "Outgroup1",
               outgroup_crop = TRUE)
print(result)
head(result$assignments)

library(ape)
phy <- read.tree("tree.nwk")
result <- ptp(phy)
} # }
```
