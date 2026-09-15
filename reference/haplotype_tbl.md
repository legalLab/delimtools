# Summarise Haplotypes Down to One Row

`haplotype_tbl()` returns a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
summarising all unique haplotype frequencies and duplicates into a
single row.

## Usage

``` r
haplotype_tbl(dna, clean = TRUE, collapseSubstrings = TRUE, verbose = TRUE)
```

## Arguments

- dna:

  an object of class [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html).

- clean:

  logical. Whether to remove or not remove non ACTG bases from
  alignment.

- collapseSubstrings:

  logical. Whether to collapse or not collapse shorter but identical
  sequences.

- verbose:

  logical. Returns a warning if any sequence contains non ACTG bases.
  See [clean_dna](clean_dna.md) for details.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`haplotype_tbl()` uses a combination of [clean_dna](clean_dna.md) and
[hap_collapse](hap_collapse.md) to summarise haplotypes into a tibble.
Each row of the tibble has an unique haplotype, its frequency and all
its collapsed duplicates in a flattened string.

## Author

Rupert A. Collins, Pedro S. Bittencourt.

## Examples

``` r

# get haplotype table
haplotype_tbl(geophagus) 
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1
#> # A tibble: 137 × 3
#>    labels     n_seqs collapsed                                                  
#>    <chr>       <dbl> <chr>                                                      
#>  1 MZ504318.1     38 MZ504328.1, MZ504313.1, MZ504311.1, MZ504312.1, MZ504309.1…
#>  2 MZ504540.1     20 MZ504505.1, MZ504553.1, MZ504554.1, MZ504552.1, MZ504542.1…
#>  3 MZ504420.1     19 MZ504417.1, MZ504437.1, MZ504425.1, MZ504427.1, MZ504422.1…
#>  4 MZ504488.1     16 MZ504538.1, KU568830.1, JN026709.1, MZ504522.1, MZ504523.1…
#>  5 MZ504484.1     15 MZ504496.1, MZ504487.1, MZ504573.1, MZ504560.1, MZ504497.1…
#>  6 MZ504462.1     14 MZ504479.1, MZ504477.1, MZ504481.1, MZ504476.1, MZ504463.1…
#>  7 MZ504375.1     13 MZ504372.1, MZ504382.1, MZ504381.1, MZ504379.1, MZ504383.1…
#>  8 MZ504535.1      8 MZ504515.1, MZ504525.1, MZ504533.1, MZ504534.1, MZ504536.1…
#>  9 MZ504393.1      8 MZ504445.1, MZ504413.1, MZ504407.1, MZ504408.1, MZ504410.1…
#> 10 MZ504400.1      6 MZ504404.1, MZ504401.1, MZ504402.1, MZ504403.1, MZ504399.1 
#> # ℹ 127 more rows
```
