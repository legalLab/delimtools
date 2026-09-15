# Unite Haplotype Summaries with Species Delimitation Outputs

`hap_unite()` returns a single
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
combining all results from [haplotype_tbl](haplotype_tbl.md) or
[collapse_others](collapse_others.md) with results from
[delim_join](delim_join.md) or [delim_consensus](delim_consensus.md).

## Usage

``` r
hap_unite(hap_tbl, delim)
```

## Arguments

- hap_tbl:

  output from [haplotype_tbl](haplotype_tbl.md) or
  [collapse_others](collapse_others.md).

- delim:

  output from [delim_join](delim_join.md) or
  [delim_consensus](delim_consensus.md).

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

Many functions in this package relies on the usage of unique haplotypes
due to known issues when using identical or duplicated sequences for
species delimitation analysis. Thus, these outputs will very often refer
only to unique haplotypes within a given dataset, which can be
determined by using functions like [hap_collapse](hap_collapse.md).
Assuming that a duplicated or identical sequence should share the same
properties as the first sequence of the group has, `hap_unite()`
combines the output of [haplotype_tbl](haplotype_tbl.md) with the output
of [delim_join](delim_join.md). Alternativelly, one may use
[collapse_others](collapse_others.md) and
[delim_consensus](delim_consensus.md) as well. This output may be used
for downstream analysis or to determine in which cluster a given
sequence belongs.

## Author

Pedro S. Bittencourt

## Examples

``` r

# get haplotype table
hap_tbl <- haplotype_tbl(geophagus)
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1

# unite
hap_unite(hap_tbl, geophagus_delims)
#> # A tibble: 354 × 12
#>    labels     n_seqs collapsed  abgd  asap  bgmyc gmyc  locmin morph mptp  ptp  
#>    <chr>       <dbl> <chr>      <chr> <chr> <chr> <chr> <chr>  <chr> <chr> <chr>
#>  1 MZ504318.1     38 MZ504328.… sp1   sp20  sp1   sp1   sp1    sp37  sp38  sp1  
#>  2 MZ504540.1     20 MZ504505.… sp16  sp16  sp25  sp27  sp33   sp34  sp16  sp16 
#>  3 MZ504420.1     19 MZ504417.… sp3   sp23  sp3   sp32  sp3    sp3   sp23  sp23 
#>  4 MZ504488.1     16 MZ504538.… sp16  sp16  sp24  sp24  sp33   sp34  sp16  sp16 
#>  5 MZ504484.1     15 MZ504496.… sp16  sp16  sp24  sp24  sp33   sp34  sp16  sp16 
#>  6 MZ504462.1     14 MZ504479.… sp6   sp21  sp21  sp21  sp6    sp21  sp21  sp21 
#>  7 MZ504375.1     13 MZ504372.… sp2   sp23  sp26  sp26  sp2    sp26  sp23  sp23 
#>  8 MZ504535.1      8 MZ504515.… sp16  sp16  sp24  sp24  sp33   sp34  sp16  sp16 
#>  9 MZ504393.1      8 MZ504445.… sp3   sp23  sp3   sp31  sp3    sp3   sp23  sp23 
#> 10 MZ504400.1      6 MZ504404.… sp3   sp23  sp3   sp32  sp3    sp3   sp23  sp23 
#> # ℹ 344 more rows
#> # ℹ 1 more variable: status <chr>
```
