# Summarise Haplotype Metadata Down to One Row

`collapse_others()` returns a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
summarising all unique haplotype frequencies, duplicates and selected
metadata into a single row.

## Usage

``` r
collapse_others(data, hap_tbl, labels, cols)
```

## Arguments

- data:

  An object of class
  [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing sequence metadata.

- hap_tbl:

  Output from [haplotype_tbl](haplotype_tbl.md).

- labels:

  Column name which contains sequence names.

- cols:

  A character vector of variables to collapse.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`collapse_others()` is a helper function to summarise metadata along
with [haplotype_tbl](haplotype_tbl.md). For any given `cols`,
`collapse_others()` flattens its content by unique haplotypes and its
duplicates in `hap_tbl`.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
# summarise haplotypes
hap_tbl <- haplotype_tbl(geophagus)
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1

# summarise country
others_df <- collapse_others(geophagus_info, hap_tbl, "gbAccession", "country")
```
