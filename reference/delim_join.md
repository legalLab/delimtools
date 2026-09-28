# Join Multiple Species Delimitation Methods Outputs

`delim_join()` returns a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html) of
species delimitation outputs whose partitions are consistent across
different methods.

## Usage

``` r
delim_join(delim, return = c("both", "df", "removed"))
```

## Arguments

- delim:

  A [list](https://rdrr.io/r/base/list.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of multiple
  species delimitation methods outputs.

- return:

  Which type of output to be returned if there are missing values within
  any of the species partitions used as input for analysis. Default to
  "df".

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`delim_join()` is a helper function to join multiple lists or columns of
species delimitation outputs into a single
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html) while
keeping consistent identifications across multiple methods. Species
delimitation outputs are in general a list or data frame of sample
labels and its species partitions (Species 1, Species 2, etc.). These
partition names may be or not the same across two or more methods.
`delim_join()` standardizes partition names across two or more species
delimitation outputs while keeping its underlying structure intact.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r

# \donttest{
## run GMYC
gmyc_res <- try( gmyc(ape::as.phylo(geophagus_beast), method = "single") )

# create a tibble
gmyc_df <- try( gmyc_tbl(gmyc_res) )

## run bGMYC
bgmyc_res <- try( bgmyc(ape::as.phylo(geophagus_beast),
  mcmc = 11000,
  burnin = 1000,
  thinning = 100,
  t1 = 2,
  t2 = ape::Ntip(ape::as.phylo(geophagus_beast)),
  start = c(1, 0.5, 50)
)
)
#> bGMYC (C engine): 137 tips, 11000 MCMC steps, 100 post-burnin samples
#> 10%
#> 20%
#> 30%
#> 40%
#> 50%
#> 60%
#> 70%
#> 80%
#> 90%
#> 100%
#> Acceptance rates (py / pc / t):
#> 0.5487  0.8376  0.3420
# create a tibble
bgmyc_df <- try( bgmyc_tbl(bgmyc_res, ppcutoff = 0.05) )

## LocMin

# create a distance matrix
mat <- try( ape::dist.dna(geophagus, model = "raw", pairwise.deletion = TRUE) )

# estimate local minima from `mat`
locmin_res <- try( spider::localMinima(mat) )
#> [1] 0.006828358 0.018994392 0.040791871 0.055999413 0.067151612 0.082105695

# create a tibble
locmin_df <- try( locmin_tbl(mat,
  threshold = locmin_res$localMinima[1],
  haps = ape::as.phylo(geophagus_beast)$tip.label
)
)
# join delimitations
all_delims <- try( delim_join(list(gmyc_df, bgmyc_df, locmin_df)) )
#> Checking species delimitation tables...
#> Checking table 1 against table 2...
#> ✔ Labels are the same across tables but they likely are unordered.
#> Checking table 1 against table 2...
#> Checking table 1 against table 3...
#> Checking table 1 against table 3...
#> ✔ Labels are the same across tables but they likely are unordered.
#> Checking table 1 against table 3...
#> ✔ Checking complete! All tables are consistent.
#> Checking table 1 against table 3...
#> Checking table 1 against table 3...

# check
try(all_delims)
#> # A tibble: 137 × 4
#>    labels     gmyc  bgmyc locmin
#>    <chr>      <chr> <chr> <chr> 
#>  1 GU701784.1 sp1   sp1   sp1   
#>  2 GU701785.1 sp1   sp1   sp1   
#>  3 MH780911.1 sp1   sp1   sp1   
#>  4 OR732927.1 sp1   sp1   sp1   
#>  5 JN988869.1 sp1   sp1   sp1   
#>  6 OR732928.1 sp1   sp1   sp1   
#>  7 MZ504448.1 sp2   sp2   sp2   
#>  8 MZ504450.1 sp2   sp2   sp2   
#>  9 MZ504454.1 sp2   sp2   sp2   
#> 10 MZ504457.1 sp3   sp3   sp3   
#> # ℹ 127 more rows

# }
```
