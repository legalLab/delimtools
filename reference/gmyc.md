# Run the GMYC Species Delimitation Analysis

Run the GMYC Species Delimitation Analysis

## Usage

``` r
gmyc(tree, method = "single", interval = c(0, 5), quiet = TRUE, tol = 1e-06)
```

## Arguments

- tree:

  A `"phylo"` object (ape).

- method:

  Analysis method: only `"single"` is implemented.

- interval:

  Optimisation interval for the null model scaling exponent (used only
  for the pure-R fallback; C engine always searches `[0, 5]`).

- quiet:

  Suppress per-threshold progress output (default `TRUE`).

- tol:

  Ultrametricity tolerance (default `1e-6`).

## Value

A named list of class `"gmyc"`, with the same structure as
[`splits::gmyc()`](https://rdrr.io/pkg/splits/man/gmyc.html): `method`,
`likelihood` (per-threshold vector), `parameters`, `entity`, `cluster`,
`MRCA`, `threshold.time`, and `tree`.

## References

Fujisawa & Barraclough (2013). *Syst. Biol.* 62(5), 707–724.
