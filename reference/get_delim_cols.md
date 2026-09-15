# Extract Labels and Colors from Species Delimitation Partitions

`get_delim_cols()` returns a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
format containing extracted and processed data from
[delim_autoplot](delim_autoplot.md).

## Usage

``` r
get_delim_cols(p, delimname = NULL, hap_tbl = NULL)
```

## Arguments

- p:

  Output from [delim_autoplot](delim_autoplot.md).

- delimname:

  A character vector of species delimitation names (optional). If
  provided, the function filters the data to only include rows matching
  such terms. Default to NULL.

- hap_tbl:

  output from [haplotype_tbl](haplotype_tbl.md) (optional). If provided,
  the function will annotate color and fill data for collapsed
  haplotypes. Default to NULL.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`get_delim_cols()` is a convenience function to extract labels, species
partitions, color and fill data from the output of
[delim_autoplot](delim_autoplot.md) in a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
format. It is best used when combined with haplotype information from
[haplotype_tbl](haplotype_tbl.md) or when combined with other metadata,
such as GPS coordinates for map plotting.

## Author

Pedro S. Bittencourt.

## Examples

``` r

# plot using autoplot
p <- delim_autoplot(geophagus_delims, geophagus_beast)
#> Warning: ⚠ Argument `tbl_labs` not provided. Using tiplabels instead.
#> Warning: ⚠ Argument `delim_order` not provided. Using default order from
#> `geophagus_delims`.
#> Warning: ⚠ Argument `col_vec` not provided. Customizing one using `delim_brewer()`.
#> ℹ Please use `delimtools::delim_brewer()` to create even better colour
#>   palettes!
#> Warning: ⚠ `n_match` was not found. Using `n_match= 4` instead.

# view 
p


# get haplotypes
hap_tbl <- haplotype_tbl(geophagus)
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1

# extract colors for consensus
get_delim_cols(p, delimname= "consensus", hap_tbl= hap_tbl)
#> # A tibble: 354 × 6
#>    labels     method    spp   fill    colour  status   
#>    <chr>      <fct>     <fct> <chr>   <chr>   <chr>    
#>  1 MZ504432.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  2 MZ504431.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  3 MZ504433.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  4 MZ504415.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  5 MZ504442.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  6 MZ504420.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  7 MZ504424.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  8 MZ504400.1 consensus sp3   #D76C3E #D76C3E haplotype
#>  9 MZ504414.1 consensus sp3   #D76C3E #D76C3E haplotype
#> 10 MZ504394.1 consensus sp3   #D76C3E #D76C3E haplotype
#> # ℹ 344 more rows

```
