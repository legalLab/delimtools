# A Command-Line Interface for PTP - Poisson Tree Process

`ptp_tbl()` is an alias of [`mptp_tbl`](mptp_tbl.md) with `method`
defaulting to `"single"` (the single-rate PTP model), rather than
`mptp_tbl`'s own default of `"multi"` (the multi-rate mPTP model).

## Usage

``` r
ptp_tbl(
  infile,
  exe = NULL,
  outfolder = NULL,
  method = c("single", "multi"),
  minbrlen = 1e-04,
  webserver = NULL,
  delimname = "ptp"
)
```

## Arguments

- infile:

  Path to tree file in Newick format, or an object of class `"mptp_ml"`
  or `"mptp_mcmc"` returned by [`mptp`](mptp.md) or
  [`mptp_mcmc`](mptp_mcmc.md). When an `mptp_ml` object is supplied the
  remaining arguments are ignored and the assignments are returned
  directly.

- exe:

  Path to an mPTP executable.

- outfolder:

  Path to output folder. Default to NULL. If not specified, a temporary
  location is used.

- method:

  Which algorithm for Maximum Likelihood point-estimate to be used.
  Available options are:

  - single Single-rate PTP model. It assumes that every species evolved
    with the same rate.

  - multi Multi-rate mPTP model. It assumes that all species have
    different evolutionary rates.

- minbrlen:

  Numeric. Branch lengths smaller or equal to the value provided are
  ignored from computations. Default to 0.0001. Use
  [min_brlen](min_brlen.md)for fine tuning.

- webserver:

  A .txt file containing mPTP results obtained from a webserver. Default
  to NULL.

- delimname:

  Character. String to rename the delimitation method in the table.
  Default to 'mptp'.

## Value

See [`mptp_tbl`](mptp_tbl.md).

## See also

[`mptp_tbl`](mptp_tbl.md), [`ptp`](ptp.md)

## Examples

``` r
# \donttest{

# get path to phylogram
path_to_file <- system.file("extdata/geophagus_raxml.nwk", package = "delimtools")

# run PTP in single threshold mode
ptp_df <- try( ptp_tbl(
  infile = path_to_file,
  exe = "/usr/local/bin/mptp",
  minbrlen = 0.0001,
  outfolder = NULL
)
)
#> Error in mptp_tbl(infile = infile, exe = exe, outfolder = outfolder, method = method,  : 
#>   Error: Please provide a valid path to the mPTP executable file.
# check
ptp_df
#> [1] "Error in mptp_tbl(infile = infile, exe = exe, outfolder = outfolder, method = method,  : \n  \033[1m\033[22mError: Please provide a valid path to the mPTP executable file.\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <error/rlang_error>
#> Error in `mptp_tbl()`:
#> ! Error: Please provide a valid path to the mPTP executable file.
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             ├─base::try(...)
#>  34.                             │ └─base::tryCatch(...)
#>  35.                             │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  36.                             │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  37.                             │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  38.                             └─delimtools::ptp_tbl(...)
#>  39.                               └─delimtools::mptp_tbl(...)
# }
```
