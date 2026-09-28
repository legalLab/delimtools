# Turns GMYC Results Into a Tibble

`gmyc_tbl()` processes output from [gmyc()](gmyc.md) into an object of
class
[`tbl_df`](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Usage

``` r
gmyc_tbl(gmyc_res, delimname = "gmyc")
```

## Source

Pons J., Barraclough T. G., Gomez-Zurita J., Cardoso A., Duran D. P.,
Hazell S., Kamoun S., Sumlin W. D., Vogler A. P. 2006. Sequence-based
species delimitation for the DNA taxonomy of undescribed insects.
Systematic Biology. 55:595-609.

Monaghan M. T., Wild R., Elliot M., Fujisawa T., Balke M., Inward D. J.
G., Lees D. C., Ranaivosolo R., Eggleton P., Barraclough T. G., Vogler
A. P. 2009. Accelerated species inventory on Madagascar using
coalescent-based models of species delineation. Systematic Biology.
58:298-311.

Fujisawa T., Barraclough T. G. 2013. Delimiting Species Using
Single-Locus Data and the Generalized Mixed Yule Coalescent Approach: A
Revised Method and Evaluation on Simulated Data Sets. Systematic
Biology. 62(5):707–724.

## Arguments

- gmyc_res:

  Output from [gmyc()](gmyc.md).

- delimname:

  Character. String to rename the delimitation method in the table.
  Default to 'gmyc'.

## Value

An object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

[gmyc()](gmyc.md) optimizes genetic clusters, and
[spec.list()](spec.list.md) clusters samples into species partitions
from the result. `gmyc_tbl()` turns these results into a tibble which
matches the output from [bgmyc_tbl](bgmyc_tbl.md) and
[locmin_tbl](locmin_tbl.md).

## Author

Pedro S. Bittencourt, Tomas Hrbek

## Examples

``` r

# \donttest{
# run GMYC
gmyc_res <- try( gmyc(ape::as.phylo(geophagus_beast)) )

# create a tibble
gmyc_df <- try( gmyc_tbl(gmyc_res) )

# check
try(gmyc_df)
#> # A tibble: 137 × 2
#>    labels      gmyc
#>    <chr>      <int>
#>  1 GU701784.1     1
#>  2 GU701785.1     1
#>  3 MH780911.1     1
#>  4 OR732927.1     1
#>  5 JN988869.1     1
#>  6 OR732928.1     1
#>  7 MZ504387.1     2
#>  8 MZ504388.1     2
#>  9 MZ504369.1     2
#> 10 MZ504390.1     2
#> # ℹ 127 more rows
# }
```
