## Resubmission

This package was archived on CRAN on 2026-03-18. The two issues previously
reported on the CRAN check page for version 0.2.2 have since been addressed:

* An example for `as_dwc()` (and the related `get_dwc()`/`dwc_terms()`
  examples) called a live GitHub-hosted data source and failed when that
  resource changed. These examples are now wrapped in `\dontrun{}`, since
  they inherently depend on a remote resource outside our control.

* `ggtree` (Imports) is a Bioconductor package, and `Additional_repositories`
  was missing from DESCRIPTION, which could make `ggtree` (and `treeio`,
  used in the vignette and now declared in Suggests) appear unavailable on
  some check flavors. `Additional_repositories` now points at the
  Bioconductor release repository.

The Rd cross-reference NOTE referencing `bGMYC`/`splits` no longer applies:
those packages (previously declared via `Additional_repositories` pointing
at a drat repo and R-Forge) are no longer dependencies -- `bgmyc()`/`gmyc()`
now reimplement the relevant algorithms natively.

While addressing the above, a full function-by-function audit also turned
up and fixed two latent bugs unrelated to the CRAN check report:

* `abgd_tbl()` crashed when parsing ABGD's native (non-SPART) webserver
  results format, due to a self-referencing `nrow(delim)` inside the pipe
  that builds `delim`.
* `asap_tbl()` could error (instead of failing gracefully) while cleaning
  up ASAP's incidental working-directory output files, because
  `tools::file_path_as_absolute()` requires the target file to already
  exist.

## R CMD check results

0 errors | 0 warnings | 1 note

Availability using Additional_repositories specification:
  ggtree, treeio   (Bioconductor release repository)

This NOTE is expected: `ggtree` and `treeio` are Bioconductor packages, and
this check environment cannot query the Bioconductor package index to
confirm availability. Both packages are correctly declared
(`ggtree` in Imports, `treeio` in Suggests) and reachable via the declared
`Additional_repositories`.
