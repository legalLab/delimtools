# A Command-Line Interface for ABGD - Automatic Barcode Gap Discovery

`abgd_tbl()` returns species partition hypothesis estimated by ABGD
software (https://bioinfo.mnhn.fr/abi/public/abgd/).

## Usage

``` r
abgd_tbl(
  infile,
  exe = NULL,
  haps = NULL,
  slope = 1.5,
  model = 3,
  outfolder = NULL,
  webserver = NULL,
  delimname = "abgd",
  type = c("recursive", "initial")
)
```

## Arguments

- infile:

  Path to a FASTA file, or an object of class `"abgd"` returned by
  [`abgd()`](abgd.md). When an `"abgd"` object is supplied, the CLI path
  is skipped and [`best_partition()`](best_partition.md) is called
  directly.

- exe:

  Path to an ABGD executable. Ignored when `infile` is an `"abgd"`
  object.

- haps:

  Optional. A vector of haplotypes to keep into the
  [`tbl_df`](https://tibble.tidyverse.org/reference/tbl_df-class.html).

- slope:

  Numeric. Relative gap width (slope). Default to 1.5.

- model:

  An integer specifying evolutionary model to be used. Available options
  are:

  - 0: Kimura-2P

  - 1: Jukes-Cantor (default)

  - 2: Tamura-Nei (*not implemented*)

  - 3: simple distance (p-distance)

- outfolder:

  Path to output folder. Default to NULL. If not specified, a temporary
  location is used.

- webserver:

  A .txt file containing ABGD results obtained from a webserver. Default
  to NULL.

- delimname:

  Character. String to rename the delimitation method in the table.
  Default to 'abgd'.

- type:

  Which ABGD pass to use when `infile` is an `"abgd"` object:
  `"recursive"` (default) or `"initial"`. Ignored for CLI and webserver
  paths.

## Value

an object of class
[`tbl_df`](https://tibble.tidyverse.org/reference/tbl_df-class.html)

## Details

`abgd_tbl()` relies on [system](https://rdrr.io/r/base/system.html) to
invoke ABGD software through a command-line interface. Hence, you must
have the software available as an executable file on your system in
order to use this function properly. `abgd_tbl()` saves all output files
in `outfolder` and imports the first recursive partition file generated
to `Environment`. Alternatively, `abgd_tbl()` can parse a .txt file
obtained from a webserver such as
(https://bioinfo.mnhn.fr/abi/public/abgd/abgdweb.html).

## References

Puillandre N., Lambert A., Brouillet S., Achaz G. 2012. ABGD, Automatic
Barcode Gap Discovery for primary species delimitation. *Molecular
Ecology* 21(8):1864-77.
[doi:10.1111/j.1365-294X.2011.05239.x](https://doi.org/10.1111/j.1365-294X.2011.05239.x)

## Author

Pedro S. Bittencourt, Tomas Hrbek

## Examples

``` r
# \donttest{

#' # get path to fasta file
path_to_file <- system.file("extdata/geophagus.fasta", package = "delimtools")

# run ABGD
abgd_df <- try( abgd_tbl(
  infile = path_to_file,
  exe = "/usr/local/bin/abgd",
  model = 3,
  slope = 0.5,
  outfolder = NULL
)
)
#> Error in abgd_tbl(infile = path_to_file, exe = "/usr/local/bin/abgd",  : 
#>   Please provide a valid path to the ABGD executable file.
# check
try(abgd_df)
#> [1] "Error in abgd_tbl(infile = path_to_file, exe = \"/usr/local/bin/abgd\",  : \n  \033[1m\033[22mPlease provide a valid path to the ABGD executable file.\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <error/rlang_error>
#> Error in `abgd_tbl()`:
#> ! Please provide a valid path to the ABGD executable file.
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
#>  38.                             └─delimtools::abgd_tbl(...)
# }
```
