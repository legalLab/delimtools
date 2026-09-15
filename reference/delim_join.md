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
gmyc_res <- try( splits::gmyc(ape::as.phylo(geophagus_beast), method = "single") )
#> node  T   loglik
#> 2 -0.2503616 1000.199 
#> 3 -0.1229369 999.5983 
#> 4 -0.0789159 1000.413 
#> 5 -0.07317712 999.2487 
#> 6 -0.06528104 1000.194 
#> 7 -0.05250486 1000.794 
#> 8 -0.04465915 1000.808 
#> 9 -0.04302288 1001.093 
#> 10 -0.04030643 1001.338 
#> 11 -0.03916037 1001.458 
#> 12 -0.03904058 1002.02 
#> 13 -0.03515353 1002.337 
#> 14 -0.0346084 1002.964 
#> 15 -0.03278097 1003.53 
#> 16 -0.02541094 1004.957 
#> 17 -0.02413816 1006.11 
#> 18 -0.02142377 1006.353 
#> 19 -0.02018164 1008.143 
#> 20 -0.0167524 1010.027 
#> 21 -0.0164289 1010.027 
#> 22 -0.01521815 1009.605 
#> 23 -0.01460825 1009.586 
#> 24 -0.01435513 1009.153 
#> 25 -0.01372697 1008.74 
#> 26 -0.01335444 1007.496 
#> 27 -0.01311278 1006.208 
#> 28 -0.0131017 1006.185 
#> 29 -0.01240316 1006.167 
#> 30 -0.01219801 1004.897 
#> 31 -0.01199715 1005.093 
#> 32 -0.01174803 1006.109 
#> 33 -0.01114228 1006.38 
#> 34 -0.01111022 1004.802 
#> 35 -0.01100491 1004.5 
#> 36 -0.0107574 1004.747 
#> 37 -0.01062164 1004.103 
#> 38 -0.01060558 1003.999 
#> 39 -0.01050048 1003.101 
#> 40 -0.01049683 1002.807 
#> 41 -0.01043136 1002.744 
#> 42 -0.01003061 1003.447 
#> 43 -0.01001803 1002.766 
#> 44 -0.009975316 1002.918 
#> 45 -0.009975263 1002.088 
#> 46 -0.009938944 1001.959 
#> 47 -0.009913875 1001.172 
#> 48 -0.009825785 1001.681 
#> 49 -0.009780681 1002.139 
#> 50 -0.009371663 1001.726 
#> 51 -0.009247986 1001.186 
#> 52 -0.009007852 1001.444 
#> 53 -0.008996279 1001.294 
#> 54 -0.00888486 1000.469 
#> 55 -0.008597392 1000.302 
#> 56 -0.008565803 999.5867 
#> 57 -0.008280494 999.1293 
#> 58 -0.008164649 997.9924 
#> 59 -0.008076452 996.8458 
#> 60 -0.007999869 996.0699 
#> 61 -0.00765457 995.2669 
#> 62 -0.007646285 994.7324 
#> 63 -0.007643749 993.9068 
#> 64 -0.007627436 993.0406 
#> 65 -0.007492421 993.934 
#> 66 -0.007020123 993.1576 
#> 67 -0.006912393 993.2364 
#> 68 -0.006783196 992.5072 
#> 69 -0.006747962 992.281 
#> 70 -0.006264295 992.4479 
#> 71 -0.00601552 992.8324 
#> 72 -0.005795439 993.0989 
#> 73 -0.00573242 993.1665 
#> 74 -0.005509334 992.8808 
#> 75 -0.005488503 992.3843 
#> 76 -0.005425978 992.5391 
#> 77 -0.005411267 992.0382 
#> 78 -0.005306896 991.5123 
#> 79 -0.005306051 990.9922 
#> 80 -0.005164414 991.1308 
#> 81 -0.004907784 991.3595 
#> 82 -0.004797087 990.8825 
#> 83 -0.004746877 990.4211 
#> 84 -0.004662424 990.3632 
#> 85 -0.004520232 991.0171 
#> 86 -0.004264922 990.6133 
#> 87 -0.004251489 990.7873 
#> 88 -0.004157546 990.4221 
#> 89 -0.004112639 990.721 
#> 90 -0.004108289 991.061 
#> 91 -0.003888943 990.6722 
#> 92 -0.003801218 991.2324 
#> 93 -0.003754736 990.9821 
#> 94 -0.003731354 990.7235 
#> 95 -0.003688184 990.4522 
#> 96 -0.003591255 990.6112 
#> 97 -0.003564653 990.68 
#> 98 -0.003442048 990.46 
#> 99 -0.00341143 990.2366 
#> 100 -0.003393153 990.0051 
#> 101 -0.003386863 989.8279 
#> 102 -0.003361018 989.6713 
#> 103 -0.003288484 989.7291 
#> 104 -0.003241535 989.5243 
#> 105 -0.003124931 989.9282 
#> 106 -0.003105419 990.3339 
#> 107 -0.003101937 990.2319 
#> 108 -0.003075062 990.3473 
#> 109 -0.00306445 990.4668 
#> 110 -0.002980459 990.176 
#> 111 -0.002962743 990.4197 
#> 112 -0.002957225 990.2826 
#> 113 -0.002838299 990.1388 
#> 114 -0.002785399 990.0348 
#> 115 -0.002736578 989.9954 
#> 116 -0.002734335 990.0459 
#> 117 -0.002686254 989.8565 
#> 118 -0.002669519 990.2641 
#> 119 -0.002637125 989.947 
#> 120 -0.002617322 989.8852 
#> 121 -0.002542929 989.8291 
#> 122 -0.002478636 989.7958 
#> 123 -0.002473902 989.7829 
#> 124 -0.00206938 989.7749 
#> 125 -0.002015661 989.8818 
#> 126 -0.001766944 990.0066 
#> 127 -0.001725274 990.2031 
#> 128 -0.001652185 990.4162 
#> 129 -0.001598489 990.6556 
#> 130 -0.00149766 990.9172 
#> 131 -0.001491313 991.216 
#> 132 -0.001312548 991.5267 
#> 133 -0.001197979 991.9014 
#> 134 -0.001131569 992.3254 
#> 135 -0.000997143 992.7881 
#> 136 -0.0007525219 993.3164 
#> 
#> Tue Sep 15 12:34:21 2026
#> finish.

# create a tibble
gmyc_df <- try( gmyc_tbl(gmyc_res) )

## run bGMYC
bgmyc_res <- try( bGMYC::bgmyc.singlephy(ape::as.phylo(geophagus_beast),
  mcmc = 11000,
  burnin = 1000,
  thinning = 100,
  t1 = 2,
  t2 = ape::Ntip(ape::as.phylo(geophagus_beast)),
  start = c(1, 0.5, 50)
)
)
#> You are running bGMYC on a single phylogenetic tree.
#> This tree contains  137  tips.
#> The Yule process rate change parameter has a uniform prior ranging from  0  to  2 .
#> The coalescent process rate change parameter has a uniform prior ranging from  0  to  2 .
#> The threshold parameter, which is equal to the number of species, has a uniform prior ranging from  2  to  137 . The upper bound of this prior should not be more than the number of tips in your trees.
#> The MCMC will start with the Yule parameter set to  1 .
#> The MCMC will start with the coalescent parameter set to  0.5 .
#> The MCMC will start with the threshold parameter set to  50 . If this number is greater than the number of tips in your tree, an error will result.
#> Given your settings for mcmc, burnin and thinning, your analysis will result in  100  samples being retained.
#> 10 % 
#> 20 % 
#> 30 % 
#> 40 % 
#> 50 % 
#> 60 % 
#> 80 % 
#> 90 % 
#> 100 % 
#> acceptance rates 
#>  py pc th 
#>  0.5496364 0.5502727 0.2227273 
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
