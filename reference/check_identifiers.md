# Checks for Differences Between Identifiers in Metadata and DNA Sequence Files

`check_identifiers()` checks for differences between identifiers in
metadata and DNA sequence files.

## Usage

``` r
check_identifiers(dna, identifier, data)
```

## Arguments

- dna:

  a [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object.

- identifier:

  column in `data` which contains sequence identifiers.

- data:

  an object of class
  [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing sequence metadata.

## Value

Invisibly returns `TRUE` if all checks pass.

## Details

`check_identifiers()` is a helper function to check for inconsistencies
between identifiers in metadata and DNA sequence files. It performs
three checks, in order:

1.  every identifier in `dna` must be present in `data`; any missing
    identifiers are reported and abort the function.

2.  `dna` must not contain duplicated identifiers; any duplicates are
    reported and abort the function.

3.  `data` must not contain duplicated identifiers. Duplicated
    identifiers that also occur in `dna` are reported and abort the
    function, since they make matching sequences to metadata ambiguous.
    Duplicated identifiers that do not occur in `dna` are reported as a
    warning only, since they do not affect matching.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
check_identifiers(geophagus, "gbAccession", geophagus_info)
#> ✔ Identifiers passed all checks.
```
