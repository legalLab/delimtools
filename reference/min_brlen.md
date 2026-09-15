# A function to report the smallest tip-to-tip distances in a phylogenetic tree

`min_brlen()` returns a table of smallest tip-to-tip distances in a
phylogenetic tree.

## Usage

``` r
min_brlen(tree, n = 5, verbose = TRUE)
```

## Arguments

- tree:

  A path to tree file in Newick format, or a phylogenetic tree object of
  class [phylo](https://rdrr.io/pkg/ape/man/read.tree.html).

- n:

  Number of distances to report (default = 5).

- verbose:

  Logical of whether to print the result to screen (default = TRUE).

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

## Details

`min_brlen()` tabulates the smallest tip-to-tip distances in a
phylogenetic tree using
[cophenetic.phylo](https://rdrr.io/pkg/ape/man/cophenetic.phylo.html)
and prints a table to screen. This is useful when excluding identical or
near-identical haplotypes using the '–minbr' parameter in mPTP.

## Author

Rupert A. Collins

## Examples

``` r

# estimate minimum branch length from raxml tree
min_brlen(ape::as.phylo(geophagus_raxml), n = 5)
#> 
#> 
#> ℹ Printing 5 smallest tip-to-tip distances in a tree with 137 tips ...
#> 
#> 
#> |dist     |  n|
#> |:--------|--:|
#> |0.000002 | 12|
#> |0.000003 |  2|
#> |0.000004 |  6|
#> |0.000005 |  2|
#> |0.001561 |  2|
```
