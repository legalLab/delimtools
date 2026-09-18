# Customize Delimitation Colors

`delim_brewer()` returns a set of colors created by interpolating or
using color palettes from
[RColorBrewer](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html),
[viridisLite](https://sjmgarnier.github.io/viridisLite/reference/viridis.map.html)
or
[randomcoloR](https://rdrr.io/pkg/randomcoloR/man/distinctColorPalette.html).

## Usage

``` r
delim_brewer(delim, package = NULL, palette = NULL, seed = NULL)
```

## Arguments

- delim:

  Output from [delim_join](delim_join.md).

- package:

  Package which contains color palettes. Available options are
  "RColorBrewer", "viridisLite" or "randomcoloR".

- palette:

  A palette name.
  [brewer.pal](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)
  for RColorBrewer or
  [viridis](https://sjmgarnier.github.io/viridisLite/reference/viridis.map.html)
  for viridisLite options.

- seed:

  Integer. Number to initialize random number generator.

## Value

A `character` vector of hexadecimal color codes.

## Details

`delim_brewer()` interpolates over a color palette and returns a vector
of random colors whose length is equal to the sum of unique species
delimitation partitions in `delim`. For reproducibility, make sure to
provide a `seed`. If not provided,
[Sys.time](https://rdrr.io/r/base/Sys.time.html) will be used as seed
instead. One should also try different seeds to get best color
combinations for plotting.

## Author

Rupert A. Collins, Pedro S. Bittencourt

## Examples

``` r

# create a vector of colors
cols <- delim_brewer(geophagus_delims, package = "randomcoloR")
#> Warning: ⚠ Argument `seed` not found. Using `Sys.time()` as seed.
#> ℹ For reproducibility, you may want to set a custom `seed` instead. `seed` is
#>   printed below:
#> 2026-09-18 04:09:28.096367
```
