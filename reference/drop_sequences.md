# Remove Sequences of a DNAbin list object

`drop_sequences()` removes sequences of a FASTA file by its names.

## Usage

``` r
drop_sequences(dna, identifier, drop = TRUE)
```

## Arguments

- dna:

  a [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) list object.

- identifier:

  a character vector containing sequence names.

- drop:

  Logical. If `TRUE`, sequence names in `identifier` will be dropped
  from `dna`. If `FALSE`, sequence names absent in `identifier` will be
  dropped instead.

## Value

an object of class [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html).

## Details

`drop_sequences()` relies on exact match between sequence names within a
fasta file and `identifier` argument.

## Author

Pedro S. Bittencourt

## Examples

``` r

# Create a vector of sequence names to drop or keep.
identifier <- names(geophagus)[1:3]

# Remove sequences listed in identifier
drop_sequences(geophagus, identifier, drop = TRUE)
#> 351 DNA sequences in binary format stored in a list.
#> 
#> All sequences of same length: 690 
#> 
#> Labels:
#> MZ504313.1
#> MZ504311.1
#> MZ504312.1
#> MZ504309.1
#> MZ504341.1
#> MZ504337.1
#> ...
#> 
#> Base composition:
#>     a     c     g     t 
#> 0.237 0.282 0.177 0.303 
#> (Total: 242.19 kb)

# Remove sequences not listed in identifier
drop_sequences(geophagus, identifier, drop = FALSE)
#> 3 DNA sequences in binary format stored in a list.
#> 
#> All sequences of same length: 690 
#> 
#> Labels:
#> MZ504301.1
#> MZ504328.1
#> MZ504318.1
#> 
#> Base composition:
#>     a     c     g     t 
#> 0.237 0.284 0.181 0.298 
#> (Total: 2.07 kb)
```
