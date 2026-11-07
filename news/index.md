# Changelog

## delimtools 0.4.2

### Bug fixes

- [`abgd_tbl()`](../reference/abgd_tbl.md): fixed a crash
  (`object 'delim' not found`) when parsing ABGD’s native (non-SPART)
  `webserver` results format — a
  `delim <- ... |> mutate(... nrow(delim) ...)` pipe referenced `delim`
  before it existed. Now uses
  [`dplyr::row_number()`](https://dplyr.tidyverse.org/reference/row_number.html).

- [`asap_tbl()`](../reference/asap_tbl.md): the cleanup of ASAP’s
  incidental working-directory output files (`*.res.cvs`,
  `*_distmat.txt`) called
  [`tools::file_path_as_absolute()`](https://rdrr.io/r/tools/fileutils.html)
  before checking whether those files existed, which errors on a
  nonexistent path instead of letting the subsequent
  [`file.exists()`](https://rdrr.io/r/base/files.html) guard handle it;
  it now builds the candidate paths without requiring them to exist
  first.

## delimtools 0.4.1

### New functions

- [`ptp()`](../reference/ptp.md) and
  [`ptp_tbl()`](../reference/ptp_tbl.md) added as aliases of
  [`mptp()`](../reference/mptp.md) /
  [`mptp_tbl()`](../reference/mptp_tbl.md), with `method` defaulting to
  `"single"` (single-rate PTP) instead of
  [`mptp()`](../reference/mptp.md)’s own default of `"multi"`
  (multi-rate mPTP).

### Improvements

- [`abgd_tbl()`](../reference/abgd_tbl.md) and
  [`asap_tbl()`](../reference/asap_tbl.md) now auto-detect and parse the
  SPART matricial text format, in addition to each tool’s own native
  webserver/CLI output, wherever they read a results file (the
  `webserver` argument, and the file the CLI run itself produces).
  Detection and parsing are handled by new internal `.is_spart_file()` /
  `.read_spart()` (`R/spart.R`), following the grammar of
  <https://github.com/iTaxoTools/SpartParser>. A SPART file with more
  than one spartition yields one column per spartition, named
  `{delimname}_1`, `{delimname}_2`, etc.

- [`gmyc()`](../reference/gmyc.md) and
  [`bgmyc()`](../reference/bgmyc.md) now accept a `treeio`-style
  `"treedata"` object (e.g. from
  [`treeio::read.beast()`](https://rdrr.io/pkg/treeio/man/beast-parser.html))
  directly, converting it to `phylo` internally before use.

- [`mptp()`](../reference/mptp.md) /
  [`mptp_mcmc()`](../reference/mptp_mcmc.md) gained input validation
  consistent with
  [`gmyc()`](../reference/gmyc.md)/[`bgmyc()`](../reference/bgmyc.md):
  class, duplicate tip labels, a minimum of 3 taxa, and polytomy
  detection. A polytomous tree previously failed with the misleading
  `"mPTP: tree is neither rooted nor unrooted Newick."`; it now reports
  the actual problem and points to
  [`ape::multi2di()`](https://rdrr.io/pkg/ape/man/multi2di.html).

- [`mptp()`](../reference/mptp.md) /
  [`mptp_mcmc()`](../reference/mptp_mcmc.md) now emit a
  [`message()`](https://rdrr.io/r/base/message.html) when an unrooted
  input tree is rooted automatically (on the tip with the single longest
  terminal branch — not midpoint rooting) and no `outgroup` was
  supplied, matching a notice the older
  [`mptp_tbl()`](../reference/mptp_tbl.md) CLI path already gave.

### Bug fixes

- [`abgd()`](../reference/abgd.md): fixed a segfault on any alignment
  containing a pair of sequences that share no non-gap site (reachable
  with the default `model = "simple"`) — the embedded C code’s
  `fprintf()` / `fclose()` calls operated on a `NULL` file handle left
  over from the original CGI tool; replaced with a clear R-level
  `error()` naming the offending pair.

- [`abgd()`](../reference/abgd.md): removed a `system("mv ...")` call in
  the embedded C code, left over from the original CGI tool and never
  reachable from R, but the kind of call CRAN reviewers flag by hand
  regardless of reachability.

- [`asap()`](../reference/asap.md): fixed sequence pairs with no
  overlapping site being silently reported as distance `0`
  (i.e. identical) for every distance model other than `"simple"` —
  [`ape::dist.dna()`](https://rdrr.io/pkg/ape/man/dist.dna.html)
  correctly returns `NaN` for these, which was then unconditionally
  coerced to `0`. Now raises a clear error naming the offending pairs,
  for every model.

- Internal `asap_exit_properly()` was a no-op under the R wrapper for
  all of ASAP’s “should never happen” internal assertions
  (memory-allocation failures, invalid internal state), silently letting
  execution continue on bad state instead of stopping; it now always
  halts via R’s `error()`.

- [`mptp()`](../reference/mptp.md) /
  [`mptp_mcmc()`](../reference/mptp_mcmc.md): fixed a NULL-pointer crash
  in the embedded mPTP C engine — the `stdout`/`stderr` macros were only
  lazily initialized as a side effect of one unrelated internal
  function, so any `fprintf()` to either stream that ran first (which
  happened on essentially every default run) segfaulted. Both macros now
  resolve through a function that always returns a valid stream.

- [`get_dwc()`](../reference/get_dwc.md) /
  [`as_dwc()`](../reference/as_dwc.md): fixed three compounding bugs —
  the Darwin Core distribution files have no header row (the first term
  was silently dropped by
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)),
  `type = "all"` requested a nonexistent `all_dwc_vertical.csv` (the
  real file is `dwc_vertical.csv`), and the `type` argument was
  mistakenly reused as a column-name selector instead of the actual term
  column.

- [`get_dwc()`](../reference/get_dwc.md),
  [`as_dwc()`](../reference/as_dwc.md),
  [`dwc_terms()`](../reference/dwc_terms.md): their examples read a live
  GitHub-hosted data source and previously failed CRAN’s automated
  checks outright when that resource’s format changed upstream. These
  examples are now wrapped in `\dontrun{}`, since they inherently depend
  on a remote resource outside our control.

- `DESCRIPTION`: added `Additional_repositories` pointing at the
  Bioconductor release repository. `ggtree` (Imports) and `treeio` (now
  declared in Suggests; it was already used, unguarded, in the vignette)
  are Bioconductor packages, and without this field they could be
  reported as unavailable on some CRAN check flavors.

- `.Rbuildignore`: `installs/` (renamed from `install/` some time ago)
  was never added under its new name, so it — along with untracked
  `benchmark/` and `issues/` directories — leaked into the built package
  as non-standard top-level content.

### Internal changes

- `.github/workflows/pkgdown.yaml`: the “Update home page” step was
  pointed at a different package’s files (`vignettes/VCFArrow.md`, a
  `P_vaillantii.jpeg` image) left over from the template it was copied
  from; it now builds the home page from this package’s own vignette
  (`vignettes/delimtools.Rmd`), knitted so its live code actually runs,
  with figures routed through `man/figures/` so pkgdown can serve them.
  The build step force-cleans `docs/` past a one-time pkgdown ownership
  check, and stages/restores `installs/install.md` around it.

- `vignettes/delimtools.Rmd` promoted from a pkgdown-only article
  (previously `vignettes/articles/`) to a real, `R CMD check`-built
  vignette.

- `install.Rmd` moved out of `docs/` (pkgdown’s own build destination,
  which it fully owns and cleans on every build) into `installs/`, as
  `installs/install.Rmd`, alongside a rendered `installs/install.md`.

## delimtools 0.3.5

### Breaking changes

- Removed all remaining direct calls to `bGMYC::`/`splits::` from
  [`bgmyc_tbl()`](../reference/bgmyc_tbl.md),
  [`gmyc_tbl()`](../reference/gmyc_tbl.md), and
  [`delim_join()`](../reference/delim_join.md) — these now call
  delimtools’ own native engines unconditionally.

### Bug fixes

- Fixed an alignment-verification bug:
  [`ape::read.FASTA()`](https://rdrr.io/pkg/ape/man/read.dna.html)
  returns a list-form `DNAbin` (one vector per sequence) rather than a
  matrix, but
  [`abgd()`](../reference/abgd.md)/[`asap()`](../reference/asap.md)’s
  shared input check tested
  [`is.matrix()`](https://rdrr.io/r/base/matrix.html) directly, wrongly
  rejecting validly-aligned list-form input. Now coerces via
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html) and only rejects
  on genuine length mismatches.

- Fixed several `R CMD check` ERRORs/WARNINGs/NOTEs ahead of CRAN
  submission: stale `check_identifiers.Rd` regenerated; undocumented
  `tr`/ `interval` arguments in
  [`confidence_intervals()`](../reference/confidence_intervals.md)
  documented explicitly instead of via a broken
  `@inheritParams splits::gmyc`; examples that called
  `bGMYC::`/`splits::` directly (an unstated-dependency WARNING, and
  neither package is on CRAN) reworked to use delimtools’ own
  [`gmyc()`](../reference/gmyc.md)/[`bgmyc()`](../reference/bgmyc.md),
  including a hand-built legacy `singlebgmyc` example for
  [`bgmyc_tbl()`](../reference/bgmyc_tbl.md) that no longer needs
  `bGMYC` installed; stale Rd cross-references `[gmyc][splits::gmyc]` /
  `[spec.list][splits::spec.list]` repointed to delimtools’ own
  functions.

- `R-CMD-check.yaml` (macOS): installs XQuartz, needed by `ggtree`’s
  `ggiraph`/`gdtools` dependency chain at package-load time.

- `R-CMD-check.yaml`: `ubuntu-latest (devel)` leg marked
  `continue-on-error` — its recurring `bit64`/install-stage failures are
  upstream RSPM/P3M binary-vs-R-devel-ABI mismatches, not a delimtools
  bug.

- `test-coverage.yaml`: fixed `fail_ci_if_error` logic, which previously
  hard-failed on every push regardless of whether a `CODECOV_TOKEN`
  secret was actually configured.

### Internal changes

- Compiled `.o` files no longer end up in the built source tarball (an
  executable `cleanup` script, run automatically by R, removes them).

- `LICENSE` added to `.Rbuildignore`.

- `.Rd` files regenerated for all functions touched by the `bGMYC`/
  `splits` removal; README refreshed for v0.3.x.

## delimtools 0.3.4

### Breaking changes

- Removed the `bGMYC` package dependency: species delimitation and the
  legacy `singlebgmyc`-object path in
  [`bgmyc_tbl()`](../reference/bgmyc_tbl.md) now run on delimtools’ own
  native engine; the option to analyze legacy `bGMYC` output is
  retained.

### Internal changes

- `build-binaries.yaml`: added an XQuartz install step for macOS.

- README updated for v0.3.x.

## delimtools 0.3.3

### Breaking changes

- [`abgd()`](../reference/abgd.md)’s first argument renamed from `file`
  to `x`. Both [`abgd()`](../reference/abgd.md) and
  [`asap()`](../reference/asap.md) now take only a path to an aligned
  FASTA file or an aligned `DNAbin` object — distance matrices are no
  longer accepted directly; distances are always computed internally
  from the sequences (fixes
  [\#28](https://github.com/legalLab/delimtools/issues/28)).

### Internal changes

- Added shared internal input validator `.read_aligned_dna()`
  (`R/asap.R`), used by both [`abgd()`](../reference/abgd.md) and
  [`asap()`](../reference/asap.md).

## delimtools 0.3.2

### Internal changes

- `src/Makevars`: subdirectory build rules reordered (most specific
  first, catch-all last) to prevent a silent-bug risk.

- [`confidence_intervals()`](../reference/confidence_intervals.md):
  `splits::gmyc()` call replaced with
  [`delimtools::gmyc()`](../reference/gmyc.md).

- [`match_ratio()`](../reference/match_ratio.md): namespace-qualified
  function calls.

- Old/unused dependencies removed from `DESCRIPTION`;
  `Imports`/`Suggests` updated.

- `build-binaries.yaml` added, to build platform binaries and generate
  GitHub releases.

## delimtools 0.3.0.9014

### Breaking changes

- [`delim_join()`](../reference/delim_join.md) now returns the
  delimitations in order of input.
  [\#118](https://github.com/legalLab/delimtools/issues/118)d137
- [`delim_consensus()`](../reference/delim_consensus.md) now returns NA
  when there are ties between two first consensus outputs. \#ddc3e74
- [`match_ratio()`](../reference/match_ratio.md) now returns a dataframe
  sorted by match_ratio values by default.
  [\#8](https://github.com/legalLab/delimtools/issues/8)eeac15
- `delim_consensus` now inputs NA values when there are ties.

### Internal

- updated .Rd files for `abgd`,
  [`abgd_tbl()`](../reference/abgd_tbl.md),
  [`asap()`](../reference/asap.md),
  [`asap_tbl()`](../reference/asap_tbl.md)

## delimtools 0.3.0.9013

### Breaking changes

- `mptp_assignments()` removed. Use
  [`mptp_tbl()`](../reference/mptp_tbl.md) directly — it accepts an
  `"mptp_ml"` / `"mptp_mcmc"` object and returns the same information as
  a properly named tibble.

## delimtools 0.3.0.9012

### Breaking changes

- [`asap()`](../reference/asap.md): French parameter names renamed to
  English equivalents — `seuil_pvalue` → `pvalue_threshold`,
  `pond_pente` → `slope_weight`, `pond_score` → `score_weight`.

## delimtools 0.3.0.9011

### Breaking changes

- `run_bgmyc()` renamed to [`bgmyc()`](../reference/bgmyc.md).
- `run_mptp_ml()` renamed to [`mptp()`](../reference/mptp.md).
- `run_mptp_mcmc()` renamed to
  [`mptp_mcmc()`](../reference/mptp_mcmc.md).

### Internal

- All Portuguese text in `asap.R` (comments, roxygen documentation, and
  user-facing messages) translated to English.

## delimtools 0.3.0.9010

### Performance

- `run_bgmyc()` is now ~3.8× faster (136-tip tree, 11 000 steps: 3.94 s
  → 1.03 s). The `C_bgmyc_mcmc` Gibbs loop now caches `log(n_spe[i])`
  and `log(jc[c] * (jc[c] - 1))` per threshold and replaces `pow(x, p)`
  with `exp(p * log_x)` for the two-pass log-likelihood. The cache is
  rebuilt only when a threshold (`t`) proposal is accepted (~11 % of
  steps); the remaining `py` / `pc` updates reuse the cached values
  without any `pow()` call.

## delimtools 0.3.0.9009

### Breaking changes

- `"bgmyc_fit"` objects (returned by `run_bgmyc()`) now store
  `$assignments` as a data frame with columns `labels` and `bgmyc` — the
  same shape as `"mptp_ml"` / `"mptp_mcmc"` objects — instead of a raw
  integer matrix. The raw per-sample cluster indices have been replaced
  by `$probmat`, the posterior co-occurrence probability matrix
  (`n_tips × n_tips`).

- `run_bgmyc()` gains a `ppcutoff` argument (default `0.05`) that
  controls the threshold used to compute the point-estimate
  `$assignments`.

- [`bgmyc_tbl()`](../reference/bgmyc_tbl.md) for `"bgmyc_fit"` objects
  now reads `$probmat` directly, enabling recomputation of the partition
  at any cutoff without re-running the

  2000. 

## delimtools 0.3.0.9008

### New functions

- `run_bgmyc()` — runs a Bayesian implementation of the General Mixed
  Yule-Coalescent (bGMYC) species delimitation model via the embedded C
  GMYC engine. Accepts an ultrametric `ape` `phylo` object. MCMC
  likelihood evaluations are performed in C (`C_gmyc_loglik`) and
  cluster assignments are extracted in C (`C_gmyc_clusters`),
  eliminating the dependency on the external `bGMYC` package. Supports
  configurable MCMC steps, burn-in, thinning, prior bounds, and proposal
  scales. Returns a classed `"bgmyc_fit"` list with MCMC samples,
  per-tip cluster assignments, and acceptance rates. Implements a
  `summary` S3 method.

### Improvements

- [`bgmyc_tbl()`](../reference/bgmyc_tbl.md) now accepts a `"bgmyc_fit"`
  object (returned by `run_bgmyc()`) as its first argument, requiring no
  external package. The co-occurrence probability matrix is built from
  the precomputed `$assignments` matrix and a point-estimate partition
  is derived via union-find at the given `ppcutoff`. The legacy
  `"singlebgmyc"` path (requires `bGMYC`) is unchanged.

## delimtools 0.3.0.9007

### Breaking changes

- `print.mptp_ml` and `print.mptp_mcmc` have been replaced by
  `summary.mptp_ml` and `summary.mptp_mcmc`. Use `summary(result)` to
  display the formatted overview; bare evaluation of an `mptp_ml` /
  `mptp_mcmc` object now falls through to the default list printer.

## delimtools 0.3.0.9006

### Bug fixes

- `run_mptp_mcmc()` now returns meaningful labels in
  `support$node_label`. Previously the column was always empty because
  most Newick files do not label inner nodes, causing `node->label` to
  be `NULL` in the C parser. Inner nodes without a Newick label now
  receive a sequential postorder label (`"Inner_1"`, `"Inner_2"`, …);
  nodes that do carry a label in the file (e.g. bootstrap values) are
  unaffected.

## delimtools 0.3.0.9005

### Breaking changes

- `$assignments` inside `"mptp_ml"` / `"mptp_mcmc"` objects now uses
  column names `labels` and `mptp` (multi-rate) or `ptp` (single-rate),
  matching the [`mptp_tbl()`](../reference/mptp_tbl.md) output format.
  The previous names `taxon` and `species` are gone.

### Internal changes

- `mptp_species()` has been removed. The function was a thin wrapper
  around `$assignments` and is no longer necessary now that column names
  are standardised.

## delimtools 0.3.0.9004

### Improvements

- [`mptp_tbl()`](../reference/mptp_tbl.md) now accepts an `"mptp_ml"` or
  `"mptp_mcmc"` object (returned by `run_mptp_ml()` / `run_mptp_mcmc()`)
  as its first argument, bypassing the CLI path entirely. The
  assignments are extracted and returned as the standard tibble with
  columns `labels` and the species integer index. All other arguments
  are ignored when an object is supplied.

### Internal changes

- The `$species` named list has been removed from `"mptp_ml"` and
  `"mptp_mcmc"` objects. It was redundant with `$assignments` and
  duplicated data in memory. `mptp_species()` now computes the list on
  demand from `$assignments`.

## delimtools 0.3.0.9003

### Bug fixes

- `run_mptp_ml()` and `run_mptp_mcmc()` now correctly report singleton
  species (taxa that form a one-taxon coalescent group). Previously, any
  tip that was itself a coalescent root (i.e. a direct child of a
  speciation node in the backtrack tree) was silently assigned
  `species = 0` and excluded from the result, causing the returned
  cluster count to be lower than the equivalent
  [`mptp_tbl()`](../reference/mptp_tbl.md) run. Fixed in
  `build_assignments_ml()` by checking
  `node->event == EVENT_COALESCENT && !in_coal` at the tip branch before
  inheriting the parent’s species index.

- `run_mptp_ml()` and `run_mptp_mcmc()` now return `n_species = 1` when
  the likelihood ratio test fails (null model preferred). Previously
  `dp_get_stats()` always returned the DP-table species count regardless
  of the LRT outcome, producing a mismatch with the assignments (which
  correctly placed all taxa in one group because no backtrack was
  performed).

## delimtools 0.3.0.9002

### Improvements

- [`abgd_tbl()`](../reference/abgd_tbl.md) now accepts an `"abgd"`
  object (returned by [`abgd()`](../reference/abgd.md)) as its first
  argument, bypassing the CLI path entirely.
  [`best_partition()`](../reference/best_partition.md) is called
  internally and the result is returned as the standard tibble. A `type`
  argument (`"recursive"` / `"initial"`, default `"recursive"`) selects
  which ABGD pass to use. `haps` filtering is respected.

- [`asap_tbl()`](../reference/asap_tbl.md) now accepts an `"asap"`
  object (returned by [`asap()`](../reference/asap.md)) as its first
  argument, bypassing the CLI path entirely.
  [`best_partition()`](../reference/best_partition.md) is called
  internally and the result is returned as the standard tibble. A `rank`
  argument (default `NULL`, i.e. rank 1) selects which ranked partition
  to return. `haps` filtering is respected.

## delimtools 0.3.0.9001

### Breaking changes

- License changed from GPL-3 to AGPL-3, required by the embedded mPTP C
  engine (AGPL-3, Flouri, Lutteropp & Kapli). Tomas Flouri, Sarah
  Lutteropp, and Paschalia Kapli added as contributors in `DESCRIPTION`.

### New functions

- `run_mptp_ml()` — runs the multi-rate Poisson Tree Processes (mPTP)
  species delimitation algorithm (ML mode) via the embedded C engine.
  Accepts a Newick file path or an `ape` `phylo` object; supports rooted
  and unrooted trees, single and multi coalescent rate models, and
  optional outgroup rooting/cropping. Returns a classed `"mptp_ml"` list
  with species assignments, log-likelihoods, LRT result, and edge
  counts. Implements a `print` S3 method.

- `run_mptp_mcmc()` — runs the mPTP MCMC engine to compute posterior
  support for each inner node as a coalescent root. Accepts the same
  inputs as `run_mptp_ml()`; supports multiple independent runs,
  configurable steps, burn-in, and sampling frequency. Returns a classed
  `c("mptp_mcmc", "mptp_ml")` list with all ML fields plus per-node
  support values and MCMC metadata. Implements a `print` S3 method.

- `mptp_assignments()` — extracts the taxon-to-species assignment data
  frame from an `"mptp_ml"` or `"mptp_mcmc"` result.

- `mptp_species()` — extracts the species-to-taxon named list from an
  `"mptp_ml"` or `"mptp_mcmc"` result.

- [`mptp_support()`](../reference/mptp_support.md) — extracts the
  per-node posterior support data frame from an `"mptp_mcmc"` result.

### Internal changes

- Embedded mPTP C engine (v1.2.3, 2023-09-19 snapshot) under
  `src/mptp/`. Includes DP, MCMC, likelihood, tree-parsing (rooted and
  unrooted Newick via Flex/Bison), and utility modules. No external
  library dependencies beyond `-lm`.

- `src/Makevars` refactored to per-directory pattern rules. `abgd/%.o`
  and `asap/%.o` receive `-include asap/r_compat.h`; `mptp/%.o` receives
  `-DHAVE_CONFIG_H -DR_PACKAGE -DYY_FATAL_ERROR(...)`. This replaces the
  previous global `-include asap/r_compat.h` in `PKG_CPPFLAGS`, which
  would cause null-pointer crashes when applied to mptp’s
  `fprintf(stdout/stderr, ...)` calls before `r_compat_null_stream` is
  initialised.

- `src/delimtools_init.c` updated with `rmptp_ml` (9 args) and
  `rmptp_mcmc` (17 args) call-method entries.

## delimtools 0.3.0.9000

### Breaking changes

- License changed from MIT to GPL-3, required by the embedded GMYC C
  engine (GPL-3).

### New functions

- [`abgd()`](../reference/abgd.md) — runs the Automatic Barcode Gap
  Discovery algorithm entirely within R via the embedded C engine
  (`abgdCore.c`). Accepts a FASTA file path or a `DNAbin` object.
  Supports K80, JC69, TN93, and simple (p-distance) models. Returns a
  classed `"abgd"` list with `initial` and `recursive` partition data
  frames.

- [`asap()`](../reference/asap.md) — runs the Assemble Species by
  Automatic Partitions algorithm via the embedded C engine
  (`asap_core.c`). Accepts a `DNAbin` object, a `dist` matrix, or a
  numeric distance matrix. Returns a classed `"asap"` list with
  partitions ranked by ASAP score, the best partition, and the distance
  matrix used. Implements `print` and `summary` S3 methods.

- [`gmyc()`](../reference/gmyc.md) — runs the General Mixed Yule
  Coalescent model via the embedded C log-likelihood engine
  (`gmyc_core.c`) driven by R’s
  [`optim()`](https://rdrr.io/r/stats/optim.html). Accepts an
  ultrametric `phylo` tree. Returns a splits-compatible `"gmyc"` object.
  Implements `print` and `summary` S3 methods.

- [`spec.list()`](../reference/spec.list.md) — S3 generic that extracts
  species lists from delimitation results.
  [`spec.list.gmyc()`](../reference/spec.list.md) method is compatible
  with the `splits` package output.

- [`best_partition()`](../reference/best_partition.md) — S3 generic for
  selecting the best partition from a delimitation result.
  [`best_partition.abgd()`](../reference/best_partition.md) applies the
  plateau criterion (Puillandre et al. 2012);
  [`best_partition.asap()`](../reference/best_partition.md) returns the
  partition at a given rank (default: rank 1).

### Internal changes

- Embedded C engines for ABGD, ASAP, and GMYC under `src/abgd/`,
  `src/asap/`, and `src/gmyc/` respectively. No external binary
  dependencies.

- `src/asap/r_compat.h` and `src/asap/r_compat.c` redirect ASAP’s
  `printf`, `exit`, `rand`, `stderr`, and `stdout` to R-safe
  equivalents; applied globally via `-include asap/r_compat.h` in
  `PKG_CPPFLAGS`.

- ASAP RNG functions (`unirandom`, `exponentialdev`, `poissondev`) now
  defined in `src/asap/asap_wrapper.c` using R’s `unif_rand()` for
  reproducibility with
  [`set.seed()`](https://rdrr.io/r/base/Random.html).

- `src/delimtools_init.c` consolidates `R_registerRoutines` for all
  three engines into a single `R_init_delimtools()`.

- Fixed a pre-existing syntax error (missing comma) in the
  [`delim_tbl()`](../reference/delim_tbl.md) example.

## delimtools 0.2.2

CRAN release: 2026-03-23

- Fixed an issue caused by an update in
  [`dplyr::if_else`](https://dplyr.tidyverse.org/reference/if_else.html)
  conditions;

- Added `try` in examples to avoid any CRAN checking errors;

## delimtools 0.2.1

CRAN release: 2025-09-29

- `Get started` vignette added;

- Fixed `hap_unite` unnesting NA values
  ([\#15](https://github.com/legalLab/delimtools/issues/15));

- `get_delim_cols` now returns a data frame in the same order of the
  phylogenetic tree plot
  ([\#16](https://github.com/legalLab/delimtools/issues/16));

- Fixed `abgd_tbl` not returning a subset of the original file when
  using `haps` and `webserver` options
  ([\#17](https://github.com/legalLab/delimtools/issues/17));

- Fixed `asap_tbl` not returning a subset of the original file when
  using `haps` and `webserver` options
  ([\#18](https://github.com/legalLab/delimtools/issues/18));

- Fixed `get_delim_cols` incompatibility with ggplot2 v.4.0.0
  ([\#20](https://github.com/legalLab/delimtools/issues/20))

## delimtools 0.2.0

CRAN release: 2025-03-31

- Initial CRAN submission.

## delimtools 0.1.0

- Initial release.
