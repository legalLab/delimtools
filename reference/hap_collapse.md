# Removes Duplicated Sequences from Alignment

`hap_collapse()` collapses haplotypes from a
[DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object, keeping unique
haplotypes only.

## Usage

``` r
hap_collapse(dna, clean = TRUE, collapseSubstrings = TRUE, verbose = TRUE)
```

## Arguments

- dna:

  A [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object.

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

A [DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object.

## Details

`hap_collapse()` collapses a
[DNAbin](https://rdrr.io/pkg/ape/man/DNAbin.html) object, keeping unique
haplotypes only. If `clean = TRUE`, the function will call
[clean_dna](clean_dna.md) to remove any non ACTG bases from alignment
prior to collapsing haplotypes. If `clean = FALSE`, the function will
treat data as it is, and will not remove any bases. If
`collapseSubstrings = TRUE`, the function will consider shorter but
identical sequences as the same haplotype and collapse them, returning
the longest sequence. If `collapseSubstrings = FALSE`, the function will
consider shorter but identical sequences as different haplotypes and
will keep them.

## Author

Rupert A. Collins

## Examples

``` r

# collapse into unique haplotypes, including shorter sequences
hap_collapse(geophagus, clean = TRUE, collapseSubstrings = TRUE)
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1
#> 137 DNA sequences in binary format stored in a list.
#> 
#> Mean sequence length: 643.007 
#>    Shortest sequence: 505 
#>     Longest sequence: 690 
#> 
#> Labels:
#> MZ504301.1
#> MZ504318.1
#> MZ504341.1
#> MZ504337.1
#> MZ504342.1
#> MZ504304.1
#> ...
#> 
#> Base composition:
#>     a     c     g     t 
#> 0.238 0.281 0.177 0.304 
#> (Total: 88.09 kb)

# collapse into unique haplotypes keeping shorter sequences
hap_collapse(geophagus, clean = TRUE, collapseSubstrings = FALSE)
#> Warning: ⚠ You have missing data "('N','-' '?')" or ambiguity inside your sequence, i.e.
#> not padding the ends, and this may have unintended consequences later, as they
#> have now been removed!
#> ℹ The names of the samples are below.
#> GU701784.1, GU701785.1
#> 246 DNA sequences in binary format stored in a list.
#> 
#> Mean sequence length: 639.764 
#>    Shortest sequence: 505 
#>     Longest sequence: 690 
#> 
#> Labels:
#> MZ504301.1
#> MZ504328.1
#> MZ504318.1
#> MZ504341.1
#> MZ504337.1
#> MZ504299.1
#> ...
#> 
#> Base composition:
#>     a     c     g     t 
#> 0.237 0.282 0.177 0.303 
#> (Total: 157.38 kb)
```
