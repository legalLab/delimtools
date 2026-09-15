# Automatic Barcode Gap Discovery (ABGD)

Delimits species using the original algorithm of Puillandre et al.
(2012). All computation (distances, gap detection, partitioning and
recursion) is performed by the original C code of G. Achaz via
[`.Call()`](https://rdrr.io/r/base/CallExternal.html).

## Usage

``` r
abgd(
  file,
  model = "simple",
  prior_min = 0.001,
  prior_max = 0.1,
  prior_steps = 10L,
  min_slope_increase = 1.5,
  ts_tv = 2
)
```

## Arguments

- file:

  Path to an aligned FASTA file, a Phylip distance matrix, or a `DNAbin`
  object (ape). If a `DNAbin` object is provided, sequences are exported
  to a temporary file and processed by the original C code without
  intermediate conversion.

- model:

  Distance model:

  `"simple"`

  :   p-distance with Laplace correction – equivalent to `-d 3` in the
      original ABGD. **Use this to replicate `abgd -a -d 3`.**

  `"JC69"`

  :   Jukes-Cantor (original default, `-d 1`).

  `"K80"`

  :   Kimura 2-parameter (`-d 0`).

  `"TN93"`

  :   Tamura-Nei (`-d 2`, *not implemented*).

- prior_min:

  Minimum prior for intraspecific divergence (`-p`). Default: `0.001`.

- prior_max:

  Maximum prior for intraspecific divergence (`-P`). Default: `0.1`.

- prior_steps:

  Number of steps in \\\[p,P\]\\ (`-n`). Default: `10`.

- min_slope_increase:

  Minimum slope increase factor (`-X`). Default: `1.5`.

- ts_tv:

  Transition/transversion ratio for K80 (`-t`). Default: `2.0`.

## Value

A named list with the following elements:

- `initial`:

  A data.frame with one row per prior (initial partition): `prior`,
  `n_groups`, `partition`.

- `recursive`:

  A data.frame with one row per prior (recursive partition): `prior`,
  `n_groups`, `partition`.

- `dist_matrix`:

  Numeric distance matrix (nseq x nseq).

- `taxa`:

  Character vector of sequence names.

## AI Disclaimer

This function was written with assistance of AI coding agent (Claude
Code Sonnet 4.6). Correctness was validated by comparing against
original software, on a suite of real sequencing datasets, and manual
code review. All validation and output verification was made by the
authors.

## Source Code

N. Puillandre, A. Lambert, S. Brouillet, G. Achaz (ABGD C engine)

## References

Puillandre N, Lambert A, Brouillet S, Achaz G (2012). ABGD, Automatic
Barcode Gap Discovery for primary species delimitation. *Molecular
Ecology*, 21(8), 1864–1877.
[doi:10.1111/j.1365-294X.2011.05239.x](https://doi.org/10.1111/j.1365-294X.2011.05239.x)

## Author

Pedro S. Bittencourt

## Examples

``` r
if (FALSE) { # \dontrun{
# Equivalent to: abgd -a -d 3 geophagus.fasta
result <- abgd("geophagus.fasta", model = "simple")

result$initial[, c("prior", "n_groups")]
result$recursive[, c("prior", "n_groups")]
result$recursive$partition[[1]]   # partition for the first prior

# Input from a DNAbin object
library(ape)
seqs   <- read.dna("geophagus.fasta", format = "fasta")
result <- abgd(seqs, model = "simple")
} # }
```
