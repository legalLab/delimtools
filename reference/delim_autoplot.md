# Plot Phylogenetic Trees With Species Delimitation Partitions

`delim_autoplot()` returns a phylogenetic tree plotted using `ggtree`
alongside with a customized tile plot using
[geom_tile](https://ggplot2.tidyverse.org/reference/geom_tile.html)
combined by
[wrap_plots](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Usage

``` r
delim_autoplot(
  delim,
  tr,
  consensus = TRUE,
  n_match = NULL,
  delim_order = NULL,
  tbl_labs = NULL,
  col_vec = NULL,
  hexpand = 0.1,
  widths = c(0.5, 0.2)
)
```

## Arguments

- delim:

  Output from [delim_join](delim_join.md).

- tr:

  A [treedata](https://rdrr.io/pkg/tidytree/man/treedata-class.html)
  object. Both phylogram and ultrametric trees are supported.

- consensus:

  Logical. Should the majority-vote consensus to be estimated?

- n_match:

  An Integer. If `consensus = TRUE`, threshold for majority-vote
  calculations. See [delim_consensus](delim_consensus.md) for details.

- delim_order:

  A character vector of species delimitation names ordered by user.
  Default to NULL.

- tbl_labs:

  A [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  of customized labels for tree plotting. The first column must match
  tip labels of the `tr` object, while the second column should have
  customized labels.

- col_vec:

  A color vector for species delimitation partitions. See
  [delim_brewer](delim_brewer.md) for customized color palette options.

- hexpand:

  Numeric. Expand xlim of tree by a ratio of x axis range. Useful if
  tiplabels become truncated when plotting. Default to `0.1`.

- widths:

  A numeric vector containing the relative widths of the tree and
  species delimitation bars. See
  [wrap_plots](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for details. Defaults to `c(0.5, 0.2)`.

## Value

A `patchwork` object.

## Details

`delim_autoplot()` is a wrapper for tree plotting with associated data
implemented using `ggtree`, `ggplot2`, and `patchwork`. If
`consensus = TRUE`, a consensus bar will be plotted next to the species
delimitation plot, summarizing partitions across samples. If no
consensus is reached, an "X" will be plotted instead.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
# view partitions using an ultrametric tree
p <- delim_autoplot(geophagus_delims, geophagus_beast)
#> Warning: ⚠ Argument `tbl_labs` not provided. Using tiplabels instead.
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the ggtree package.
#>   Please report the issue at <https://github.com/YuLab-SMU/ggtree/issues>.
#> Warning: ⚠ Argument `delim_order` not provided. Using default order from
#> `geophagus_delims`.
#> Warning: ⚠ Argument `col_vec` not provided. Customizing one using `delim_brewer()`.
#> ℹ Please use `delimtools::delim_brewer()` to create even better colour
#>   palettes!
#> Warning: ⚠ `n_match` was not found. Using `n_match= 4` instead.
p


# view partitions using a phylogram
p1 <- delim_autoplot(geophagus_delims, geophagus_raxml)
#> Warning: ⚠ Argument `tbl_labs` not provided. Using tiplabels instead.
#> Warning: ⚠ Argument `delim_order` not provided. Using default order from
#> `geophagus_delims`.
#> Warning: ⚠ Argument `col_vec` not provided. Customizing one using `delim_brewer()`.
#> ℹ Please use `delimtools::delim_brewer()` to create even better colour
#>   palettes!
#> Warning: ⚠ `n_match` was not found. Using `n_match= 4` instead.
```
