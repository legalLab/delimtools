# Removes Gaps, Ambiguities and Missing Data from DNA Sequences

`clean_dna()` removes all character not a valid ACTG base from a
[DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object.

## Usage

``` r
clean_dna(dna, verbose = TRUE)
```

## Arguments

- dna:

  an object of class [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html).

- verbose:

  logical. Returns a warning if any sequence contains non ACTG bases.

## Value

an object of class [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html).

## Details

`clean_dna()` detects and removes any non ACTG bases from alignment.
This includes: "N", "-", "?", "R", "Y", etc. If `verbose = TRUE`,
returns a warning if these characters are inside the sequences, i.e, are
not alignment padding chars at the ends.

## Author

Rupert A. Collins

## Examples

``` r
geo_clean <- clean_dna(geophagus)
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1
```
