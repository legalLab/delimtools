[![Build Status](https://travis-ci.org/LegalLab/delimtools.svg?branch=master)](https://travis-ci.org/LegalLab/delimtools)
[![DOI](https://zenodo.org/badge/xxx.svg)](https://zenodo.org/badge/latestdoi/xxx)

# delimtools

## IMPORTANT

This software is under active development, and as such cannot be assumed to be free of bugs or poor functionality. Always inspect results carefully. If you find a problem, please report it with as much detail as possible in [Issues](https://github.com/LegaLab/delimtools/issues). Unfortunately in it's current form, some of the functions (`abgd_tbl()`, `asap_tbl()` and `mptp_tbl()`)  will not work on Windows operating systems, only on Unix (MacOS, Linux). This is because third party binaries (executable programs) are required. We are working to implement importing results from the webservers that are commonly used to run these analyses. There are also difficulties with installing R required package dependancies to run `gmyc_tbl()` and `bgmyc_tbl()` because these are no longer under development and available on CRAN, and must be installed from archived sources. Please refer to [github.com/boopsboops/delimtools-testing](https://github.com/boopsboops/delimtools-testing/blob/main/scripts/install.md) for instructions on how to install.

## Overview

The official GitHub repository for the R package `delimtools` ([Bittencourt et al., 20XX](https://doi.org/10.xxx)).

`delimtools` provides helper functions for the analysis of single-locus species delimitation methods such as GMYC ([Monaghan et al., 2009](https://doi.org/10.1093/sysbio/syp027)), bGMYC ([Reid & Carstens, 2012](https://doi.org/10.1186/1471-2148-12-196)), mPTP ([Kapli et al., 2017](https://doi.org/10.1093/bioinformatics/btx025)) and ASAP ([Puillandre et al., 2020](https://doi.org/10.1111/1755-0998.13281)). These software run multiple different platforms (e.g. R, Unix, webservers), and also do not output their results in a consistent format to allow easy comparison. To address these shortcomings we have developed a suite of functions to standardise and simplify generating single-locus species delimitations.


## Installation

Development version from GitHub:

```r
devtools::install_github("LegalLab/delimtools")
```

Or a specific version via renv:

```r
renv::install("LegalLab/delimtools@v0.1.0")
```

`delimtools` provides 


## Examples

Here, we will demonstrate a single-locus species delimitation analysis on a _Geophagus_ eartheater cichlid dataset ([Ximenes et al., 2021](https://doi.org/10.7717/peerj.12443)) using a variety of methods. For full details please see the GitHub repository accompanying this R package at [github.com/boopsboops/delimtools-testing](https://github.com/boopsboops/delimtools-testing).

![_Geophagus_ sp. "red head Tapajós"](https://github.com/boopsboops/delimtools-testing/blob/79d6257c9ae4b7da1047e5ffa9ef1a04b4139dae/assets/geophagus_redhead_tapajos.jpg)


```r
# load up the data
library("delimtools")
data(geophagus)
```


## Current contributors

Rupert A. Collins, 

* [Pedro S. Bittencourt](https://github.com/pedrosenna)
* [Tomas Hrbek](https://github.com/killidude)
* [Rupert A. Collins](https://github.com/boopsboops)


## Meta

* Please [report here any issues or bugs or suggestions](https://github.com/LegaLab/delimtools/issues).
* License: MIT.
* Get citation information for `delimtools` in R by running `citation(package='delimtools')`.
