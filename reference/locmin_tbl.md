# Turns Local Minima Results into a Tibble

`locmin_tbl()` processes output from
[tclust](https://rdrr.io/pkg/spider/man/tclust.html) into an object of
class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Usage

``` r
locmin_tbl(distobj, threshold = 0.01, haps = NULL, delimname = "locmin")
```

## Source

Brown S.D.J., Collins R.A., Boyer S., Lefort M.-C., Malumbres-Olarte J.,
Vink C.J., Cruickshank, R.H. 2012. Spider: An R package for the analysis
of species identity and evolution, with particular reference to DNA
barcoding. Molecular Ecology Resources, 12: 562-565.

## Arguments

- distobj:

  A distance object (usually from
  [dist.dna](https://rdrr.io/pkg/ape/man/dist.dna.html)).

- threshold:

  Distance cutoff for clustering. Default of 0.01. See
  [localMinima](https://rdrr.io/pkg/spider/man/localMinima.html) for
  details.

- haps:

  Optional. A vector of haplotypes to keep into the
  [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

- delimname:

  Character. String to rename the delimitation method in the table.
  Default to 'locmin'.

## Value

An object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`spider` package uses
[localMinima](https://rdrr.io/pkg/spider/man/localMinima.html) to
determine possible thresholds for any distance matrix and
[tclust](https://rdrr.io/pkg/spider/man/tclust.html) to cluster samples
within a given `threshold` into species partitions. `locmin_tbl()` turns
these inputs into a tibble which matches the output from
[gmyc_tbl](gmyc_tbl.md) and [bgmyc_tbl](bgmyc_tbl.md).

## Author

Samuel Brown.

## Examples

``` r
# create a distance matrix
mat <- ape::dist.dna(geophagus, model = "raw", pairwise.deletion = TRUE)

# run Local Minima
locmin_res <- spider::localMinima(mat)
#> [1] 0.006828358 0.018994392 0.040791871 0.055999413 0.067151612 0.082105695

# create a tibble
locmin_df <- locmin_tbl(mat, 
                        threshold = locmin_res$localMinima[1], 
                        haps = ape::as.phylo(geophagus_beast)$tip.label)

# check
locmin_df 
#> # A tibble: 137 × 2
#>    labels     locmin
#>    <chr>       <int>
#>  1 MZ504301.1      1
#>  2 MZ504318.1      1
#>  3 MZ504341.1      1
#>  4 MZ504337.1      1
#>  5 MZ504342.1      1
#>  6 MZ504304.1      1
#>  7 MZ504332.1      1
#>  8 MZ504343.1      1
#>  9 MZ504315.1      1
#> 10 MZ504345.1      1
#> # ℹ 127 more rows
```
