#' A Command-Line Interface for mPTP - multi-rate Poisson Tree Processes
#'
#' @description
#' \code{mptp()} returns species partition hypothesis estimated by mPTP software
#' (\url{https://github.com/Pas-Kapli/mptp}).
#'
#' @param infile Path to tree file in Newick format.
#' @param exe Path to an mPTP executable.
#' @param outfolder Path to output folder. Default to NULL. If not specified, a temporary location is used.
#' @param method Which algorithm for Maximum Likelihood point-estimate to be used. Available options are:
#' \itemize{
#' \item single Single-rate PTP model. It assumes that every species evolved with the same rate.
#' \item multi Multi-rate mPTP model. It assumes that all species have different evolutionary rates.
#' }
#' @param outgroup A vector of outgroup labels. Default to NULL. If specified,
#' mPTP will crop the specified labels from the tree.
#'
#' @details
#' \code{mptp()} relies on \code{\link[base]{system}} to invoke mPTP software through
#' a command-line interface. Hence, you must have the software available as an executable file on 
#' your system in order to use this function properly. \code{mptp()}
#' saves all output files in \code{outfolder} and imports the results generated to \code{Environment}.
#' If an \code{outfolder} is not provided by the user, then a temporary location is used.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}
#'
#' @author
#' Paschalia Kapli, Sarah Lutteropp, Jiajie Zhang, Kassian Kobert, Pavlos Pavlides, Alexandros Stamatakis, Tomáš Flouri.
#'
#' @importFrom tibble tibble
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom glue glue
#' @importFrom ape read.tree cophenetic.phylo
#' @importFrom delimtools min_brlen

#'
#' @export
mptp <- function(infile, exe = NULL, outfolder = NULL, method = c("multi", "single"), outgroup=NULL, minbrlen = 0.0001){

  if(!file.exists(exe)){

    cli::cli_abort("Error: Please provide a valid path to the mPTP executable file.")
  
  }

  if(is.null(outfolder)){

    outfolder <- tempdir()

  }

  if(!dir.exists(outfolder)){

    cli::cli_abort("Error: Please provide a valid results directory.")

  }

  if(missing(method)){

    cli::cli_abort(c("Please provide a valid option for {.arg method}.",
                     "i" = "Available options are {.val multi} or {.val single}."))

  }

  if(method != "single" && method != "multi"){

    cli::cli_abort(c("Please provide a valid option for {.arg method}.",
                     "i" = "Available options are {.val multi} or {.val single}."))
  }

  if(!is.null(outgroup)){

    if(method == "multi"){

      string_mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --outgroup {paste(outgroup, collapse = ',')} --outgroup_crop --minbr {minbrlen}")
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |>
        sub(":", "", x=_)

      split_vec <- function(vec, sep = "") {
        is_sep <- vec == sep
        split(vec[!is_sep], cumsum(is_sep)[!is_sep]+1)
      }

      mptp_ls <- split_vec(lines)

      mptp_ls <- lapply(mptp_ls, function(x) x[-1])

      mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], mptp = as.numeric(unlist(x)))))

    }

    else if(method == "single"){

      string_mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --outgroup {paste(outgroup, collapse = ',')} --outgroup_crop --minbr {minbrlen}")
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |>
        sub(":", "", x=_)

      split_vec <- function(vec, sep = "") {
        is_sep <- vec == sep
        split(vec[!is_sep], cumsum(is_sep)[!is_sep]+1)
      }

      mptp_ls <- split_vec(lines)

      mptp_ls <- lapply(mptp_ls, function(x) x[-1])

      mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], ptp = as.numeric(unlist(x)))))

    }
  } else if(is.null(outgroup)){

    if(method == "multi"){

      string_mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --minbr {minbrlen}")
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |>
        sub(":", "", x=_)

      split_vec <- function(vec, sep = "") {
        is_sep <- vec == sep
        split(vec[!is_sep], cumsum(is_sep)[!is_sep]+1)
      }

      mptp_ls <- split_vec(lines)

      mptp_ls <- lapply(mptp_ls, function(x) x[-1])

      mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], mptp = as.numeric(unlist(x)))))

    }

    else if(method == "single"){

string_mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --minbr {minbrlen}")
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |>
        sub(":", "", x=_)

      split_vec <- function(vec, sep = "") {
        is_sep <- vec == sep
        split(vec[!is_sep], cumsum(is_sep)[!is_sep]+1)
      }

      mptp_ls <- split_vec(lines)

      mptp_ls <- lapply(mptp_ls, function(x) x[-1])

      mptp_df <- do.call(rbind, lapply(names(mptp_ls), function(x) tibble::tibble(labels = mptp_ls[[x]], ptp = as.numeric(unlist(x)))))

    }
  }

  minbrlen.tab <- min_brlen(tree = infile, print = FALSE)#delimtools::

  minbrlen.est <- minbrlen.tab |> pull(dist) |> min() |> format(scientific=FALSE)

  if(minbrlen.est < format(minbrlen, scientific=FALSE)) {
  
  writeLines("\n")

  cli::cli_alert_info(
    "Warning: there are tip-to-tip distances smaller than the specified minimum branch length ({format(minbrlen, scientific=FALSE)}).
    Consider using `delimtools::min_brlen()` to explore branch lengths in your tree."
    )

  writeLines("\n")

  }

  cli::cli_alert_info("mPTP files are located in '{outfolder}'.")

  writeLines("\n")

  return(mptp_df)

}
