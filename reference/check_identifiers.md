# Checks for Differences Between Identifiers in Metadata and DNA Sequence Files

`check_identifiers()` checks for differences between identifiers in
metadata and DNA sequence files.

## Usage

``` r
check_identifiers(data, identifier, dna)
```

## Arguments

- data:

  an object of class
  [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing sequence metadata.

- identifier:

  column in `data` which contains sequence identifiers.

- dna:

  a [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object.

## Value

A list containing erroneus identifiers between metadata and sequence
file.

## Details

`check_identifiers()` is a helper function to check for inconsistencies
between identifiers in metadata and DNA sequences files, such as
absence, mistyping, duplicated entries, or differences in size lengths.
If any of these problems are found, warnings will appear in `Console`
and corrections should be made to prevent unintended consequences later.
A list containing erroneous identifiers is returned invisibly.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
check_identifiers(geophagus_info, "gbAccession", geophagus)
#> Error in UseMethod("pull"): no applicable method for 'pull' applied to an object of class "DNAbin"
```
