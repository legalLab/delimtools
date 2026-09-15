# Get species list from a delimitation result

Returns a character vector of tip labels, grouped by inferred species.

## Usage

``` r
spec.list(x, ...)

# S3 method for class 'gmyc'
spec.list(x, second.peak = FALSE, ...)
```

## Arguments

- x:

  A fitted delimitation object (e.g. `gmyc`).

- ...:

  Additional arguments passed to methods.

- second.peak:

  Logical. If `TRUE`, use the second likelihood peak instead of the
  global maximum. Default: `FALSE`.

## Value

A named character vector mapping tips to species identifiers.
