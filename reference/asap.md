# ASAP – Assemble Species by Automatic Partitions

Delimits species from DNAbin sequences or a distance matrix using the
algorithm of Puillandre, Brouillet & Achaz (2021). The original C code
(asap_core.c, asap_common.c) is executed without modifications; only the
Python/graphical dependencies are replaced.

## Usage

``` r
asap(
  x,
  model = "simple",
  len_seq = NULL,
  replicates = 1000L,
  pvalue_threshold = 0.001,
  slope_weight = 0.1,
  score_weight = 0.5,
  pairwise.deletion = TRUE
)
```

## Arguments

- x:

  `DNAbin` (ape), `dist`, or a numeric distance matrix.

- model:

  Distance model when `x` is `DNAbin`. `"simple"` (default) uses
  Simple_Dist, identical to the ASAP original default. Any model from
  [`ape::dist.dna()`](https://rdrr.io/pkg/ape/man/dist.dna.html) is also
  accepted.

- len_seq:

  Sequence length for the coalescent simulations. Inferred automatically
  from `DNAbin`; use 600 (original default) when the input is a distance
  matrix.

- replicates:

  Coalescent replicates. Default: `1000`.

- pvalue_threshold:

  P-value threshold. Default: `0.001`.

- slope_weight:

  Slope window weight. Default: `0.1`.

- score_weight:

  Weight of the p-value rank in the ASAP-score. Default: `0.5`.

- pairwise.deletion:

  Passed to
  [`ape::dist.dna()`](https://rdrr.io/pkg/ape/man/dist.dna.html).
  Default: `TRUE`.

## Value

A list of class `"asap"` with:

- `partitions`:

  data.frame sorted by ascending ASAP-score.

- `best`:

  List with the best partition (rank 1).

- `dist_matrix`:

  Distance matrix used.

- `taxa`:

  Sequence names.

## AI Disclaimer

This function was written with assistance of AI coding agent (Claude
Code Sonnet 4.6). Correctness was validated by comparing against
original software, on a suite of real sequencing datasets, and manual
code review. All validation and output

## Source Code

N. Puillandre, A. Lambert, S. Brouillet, G. Achaz (ASAP C engine)

## References

Puillandre N, Brouillet S, Achaz G (2021). ASAP: assemble species by
automatic partitions. *Molecular Ecology Resources*, 21(2), 609-620.
[doi:10.1111/1755-0998.13281](https://doi.org/10.1111/1755-0998.13281)

## Author

Pedro S. Bittencourt

## Examples

``` r
if (FALSE) { # \dontrun{
library(ape)
seqs   <- read.dna("barcode.fasta", format = "fasta")
result <- asap(seqs)
print(result)
bp <- best_partition(result)
bp$n_groups   # number of species
bp$partition  # assignment for each sequence
} # }
```
