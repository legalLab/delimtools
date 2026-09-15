# A Command-Line Interface for ASAP - Assemble Species by Automatic Partitioning

`asap_tbl()` returns species partition hypothesis estimated by ASAP
software (https://bioinfo.mnhn.fr/abi/public/asap/).

## Usage

``` r
asap_tbl(
  infile,
  exe = NULL,
  haps = NULL,
  model = 3,
  outfolder = NULL,
  webserver = NULL,
  delimname = "asap",
  rank = NULL
)
```

## Arguments

- infile:

  Path to a FASTA file, or an object of class `"asap"` returned by
  [`asap()`](asap.md). When an `"asap"` object is supplied, the CLI path
  is skipped and [`best_partition()`](best_partition.md) is called
  directly.

- exe:

  Path to an ASAP executable. Ignored when `infile` is an `"asap"`
  object.

- haps:

  Optional. A vector of haplotypes to keep into the
  [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

- model:

  An integer specifying evolutionary model to be used. Available options
  are:

  - 0: Kimura-2P

  - 1: Jukes-Cantor (default)

  - 2: Tamura-Nei

  - 3: simple distance (p-distance)

- outfolder:

  Path to output folder. Default to NULL. If not specified, a temporary
  location is used.

- webserver:

  A .csv file containing ASAP results obtained from a webserver. Default
  to NULL.

- delimname:

  Character. String to rename the delimitation method in the table.
  Default to 'asap'.

- rank:

  Integer. Which ranked partition to return when `infile` is an `"asap"`
  object (default `NULL` selects rank 1 / best ASAP score). Ignored for
  CLI and webserver paths.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

## Details

`asap_tbl()` relies on [system](https://rdrr.io/r/base/system.html) to
invoke ASAP software through a command-line interface. Hence, you must
have the software available as an executable file on your system in
order to use this function properly. `asap_tbl()` saves all output files
in `outfolder` and imports the first partition file generated to
`Environment`. Alternatively, `asap_tbl()` can parse a .csv file
obtained from webserver such as
(https://bioinfo.mnhn.fr/abi/public/asap/asapweb.html).

## References

Puillandre N., Brouillet S., Achaz G. 2021. ASAP: assemble species by
automatic partitioning. *Molecular Ecology Resources* 21:609–620.
[doi:10.1111/1755-0998.13281](https://doi.org/10.1111/1755-0998.13281)

## Author

Pedro S. Bittencourt, Tomas Hrbek

## Examples

``` r

# \donttest{

#' # get path to fasta file
path_to_file <- system.file("extdata/geophagus.fasta", package = "delimtools")

# run ASAP
asap_df <- try(asap_tbl(infile = path_to_file, exe= "/usr/local/bin/asap", model= 3))
#> Error in asap_tbl(infile = path_to_file, exe = "/usr/local/bin/asap",  : 
#>   Please provide a valid path to the ASAP executable file.

# check
try(asap_df)
#> [1] "Error in asap_tbl(infile = path_to_file, exe = \"/usr/local/bin/asap\",  : \n  \033[1m\033[22mPlease provide a valid path to the ASAP executable file.\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <error/rlang_error>
#> Error in `asap_tbl()`:
#> ! Please provide a valid path to the ASAP executable file.
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
#>  38.                             └─delimtools::asap_tbl(infile = path_to_file, exe = "/usr/local/bin/asap", model = 3)

# }
```
