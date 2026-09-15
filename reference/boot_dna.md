# Boostrapping DNA sequences

`boot_dna()` generates random bootstrap alignments for confidence
interval estimation using
[confidence_intervals](confidence_intervals.md). Thus, it is meant to be
an internal function of this package.

## Usage

``` r
boot_dna(dna, block = 1)
```

## Arguments

- dna:

  an object of class [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html).

- block:

  integer. Number of columns to be resampled together. Default to 1.

## Value

a [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object.

## Author

Pedro S. Bittencourt

## Examples

``` r
boot <- boot_dna(geophagus)
```
