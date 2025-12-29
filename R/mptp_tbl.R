#' Turns a phylogram into a tibble of PTP/mPTP partitions
#' A Command-Line Interface for PTP/mPTP - multiple Poisson Tree Process
#'
#' @description
#' `mptp_tbl()` returns species partition hypothesis estimated by mPTP software
#' <https://github.com/Pas-Kapli/mptp>.
#'
#' @param infile Path to tree file in Newick format. Should be dichotomous and rooted.
#' @param exe Path to an mPTP executable.
#' @param outfolder Path to output folder. Default to NULL. If not specified, a temporary location is used.
#' @param method Which algorithm for Maximum Likelihood point-estimate to be used. Available options are:
#' \itemize{
#' \item single Single-rate PTP model. It assumes that every species evolved with the same rate.
#' \item multi Multi-rate mPTP model. It assumes that all species have different evolutionary rates.
#' }
#' @param minbrlen Numeric. Branch lengths smaller or equal to the value provided
#' are ignored from computations. Default to 0.0001. Use [min_brlen][delimtools:min_brlen] for fine tuning.
#' @param webserver A .txt file containing mPTP results obtained from a webserver. Default to NULL.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'mptp'.
#'
#' @details
#' `mptp_tbl()` relies on [system][base::system] to invoke mPTP software through
#' a command-line interface. Hence, you must have the software available as an executable file on
#' your system in order to use this function properly. `mptp_tbl()` saves all output files in
#' `outfolder` and imports the results generated to `Environment`.
#' If an `outfolder` is not provided by the user, then a temporary location is used.
#' Alternatively, `mptp_tbl()` can parse a file obtained from webserver such as
#' <https://mptp.h-its.org/>. 
#' `mptp_tbl()` turns these results into a tibble that matches the output of all other [xxx_tbl] functions.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df]
#'
#' @author
#' Pedro S. Bittencourt, Tomas Hrbek
#'
#' @source
#' Kapli T., Lutteropp S., Zhang J., Kobert K., Pavlidis P., Stamatakis A., Flouri T. 2017.
#' Multi-rate Poisson tree processes for single-locus species delimitation under
#' maximum likelihood and Markov chain Monte Carlo. Bioinformatics 33(11):1630-1638.
#' DOI: 10.1093/bioinformatics/btx025
#'
#' @examples
#' \donttest{
#'
#' # get path to phylogram
#' path_to_file <- system.file("extdata/geophagus_raxml.nwk", package = "delimtools")
#'
#' # run mPTP in single threshold mode (PTP)
#' ptp_df <- mptp_tbl(
#'   infile = path_to_file,
#'   exe = "/usr/local/bin/mptp",
#'   method = "single",
#'   minbrlen = 0.0001,
#'   delimname = "ptp",
#'   outfolder = NULL
#' )
#'
#' # check
#' ptp_df
#'
#' # run mPTP in multi threshold mode (mPTP)
#'
#' mptp_df <- mptp_tbl(
#'   infile = path_to_file,
#'   exe = "/usr/local/bin/mptp",
#'   method = "multi",
#'   minbrlen = 0.0001,
#'   delimname = "mptp",
#'   outfolder = NULL
#' )
#'
#' # check
#' mptp_df
#' }
#'
#' @export
mptp_tbl <- function(infile, exe = NULL, outfolder = NULL, method = c("multi", "single"), minbrlen = 0.0001, webserver = NULL, delimname = "mptp") {

  # infile checks
  if (file.exists(infile)) {
    lines <- readLines(infile, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    lines <- lines[!grepl("^\\[.*\\]$", lines)]  # drop pure NEXUS comments
    if (length(lines) == 0) {
      cli::cli_abort("Phylogenetic tree file is empty or contains only comments.")
    }
    if (grepl("^#NEXUS", toupper(lines[1])) && grepl("^END;$", toupper(lines[length(lines)]))) {
      tr <- ape::read.nexus(infile)
      # check if tree is unrooted; should never happen, but ...
      if (!ape::is.rooted(tr)) {
        cli::cli_abort("Phylogenetic tree is not rooted.")
      }
      # check if tree is ultrametric
      if (ape::is.ultrametric(tr)) {
        cli::cli_abort("Phylogenetic tree is ultrametric.")
      }
    } else if (grepl("^\\(", lines[1]) && grepl(";$", lines[length(lines)])) {
      tr <- ape::read.tree(infile)
      # check if tree is unrooted; should never happen, but ...
      if (!ape::is.rooted(tr)) {
        cli::cli_abort("Phylogenetic tree is not rooted.")
      }
      # check if tree is ultrametric
      if (ape::is.ultrametric(tr)) {
        cli::cli_abort("Phylogenetic tree is ultrametric.")
      }
    } else {
      cli::cli_abort("Improperly formatted Newick or Nexus tree file.")
    }
  }

  dname <- rlang::sym(delimname)

  split_vec <- function(vec, sep = "") {
    is_sep <- vec == sep
    split(vec[!is_sep], cumsum(is_sep)[!is_sep] + 1)
  }

  if (!is.null(webserver) && file.exists(webserver)) {
    header <- readLines(webserver)[1]

    if (!grepl("mptp", header)) {
      cli::cli_abort("Looks like your file does not contain valid mPTP results ...")
    }

    lines <- readLines(webserver)[-c(1:6)] |> 
      sub(":", "", x = _)

    mptp_ls <- split_vec(lines)

    mptp_ls <- lapply(mptp_ls, function(x) x[-1])

    if (grepl("single", header)) {
      mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], !!dname := as.integer(unlist(x)))))
    }

    if (grepl("multi", header)) {
      mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp.ls[[x]], !!dname := as.integer(unlist(x)))))
    }

    return(mptp_df)
  }

  if (!file.exists(exe)) {
    cli::cli_abort("Please provide a valid path to the mPTP executable file.")
  }

  if (is.null(outfolder)) {
    outfolder <- tempdir()
  }

  if (!dir.exists(outfolder)) {
    cli::cli_abort("Please provide a valid results directory.")
  }

  if (method != "single" && method != "multi") {
    cli::cli_abort(c("Please provide a valid option for {.arg method}.",
      "i" = "Available options are {.val multi} or {.val single}."))
  }

  if (method == "multi") {
    string_mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --minbr {minbrlen}")
    res <- system(command = string_mptp, intern = TRUE)
    writeLines(res)

    lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |> sub(":", "", x = _)

    mptp_ls <- split_vec(lines)

    mptp_ls <- lapply(mptp_ls, function(x) x[-1])

    mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], !!dname := as.integer(unlist(x)))))
  }

  if (method == "single") {
    string_mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --minbr {minbrlen}")
    res <- system(command = string_mptp, intern = TRUE)
    writeLines(res)

    lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |> sub(":", "", x = _)

    mptp_ls <- split_vec(lines)
    
    mptp_ls <- lapply(mptp_ls, function(x) x[-1])
    
    mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], !!dname := as.integer(unlist(x)))))
  }

  minbrlen_tab <- delimtools::min_brlen(tree = infile, verbose = FALSE)

  minbrlen_est <- minbrlen_tab |>
    dplyr::pull(1) |>
    min() |>
    format(scientific = FALSE)

  if (minbrlen_est < format(minbrlen, scientific = FALSE)) {
    cli::cli_alert_info(
      "Warning: there are tip-to-tip distances smaller than the specified minimum branch length ({format(minbrlen, scientific=FALSE)}).
      Consider using `delimtools::min_brlen()` to explore branch lengths in your tree."
    )
  }

  cli::cli_alert_info("mPTP files are located in '{outfolder}'.")

  return(mptp_df)
}
