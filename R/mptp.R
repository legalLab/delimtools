#' A Command-Line Interface for mPTP - multi-rate Poisson Tree Processes
#'
#' @description
#' \code{mptp()} returns species partition hypothesis estimated by mPTP software
#' (\url{https://github.com/Pas-Kapli/mptp}).
#'
#' @param infile Path to tree file in Newick format.
#' @param outfolder Path to output folder.
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
#' a command-line interface. Hence, you must have the software installed on your
#' operating system root in order to use this function properly. \code{mptp()}
#' saves all output files in \code{outfolder} and imports the results generated to \code{Environment}.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}
#'
#' @author
#' Paschalia Kapli, Sarah Lutteropp, Jiajie Zhang, Kassian Kobert, Pavlos Pavlides, Alexandros Stamatakis, Tomáš Flouri.
#'
#' @importFrom tibble tibble
#' @importFrom cli cli_abort
#'
#' @export
mptp <- function(infile, outfolder, method = c("multi", "single"), outgroup=NULL){

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

      string_mptp <- paste0("mptp", " --tree_file ", infile, " --output_file ",
                            outfolder, " --ml ", "--", method, " --outgroup ",
                            paste(outgroup, collapse = ","), " --outgroup_crop")
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(paste0(outfolder, ".txt"))[-c(1:8)] |>
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

      string_mptp <- paste0("mptp", " --tree_file ", infile, " --output_file ",
                            outfolder, " --ml ", "--", method, " --outgroup ",
                            paste(outgroup, collapse = ","), " --outgroup_crop")
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(paste0(outfolder, ".txt"))[-c(1:8)] |>
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

      string_mptp <- paste0("mptp", " --tree_file ", infile, " --output_file ",
                            outfolder, " --ml ", "--", method)
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(paste0(outfolder, ".txt"))[-c(1:8)] |>
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

      string_mptp <- paste0("mptp", " --tree_file ", infile, " --output_file ",
                            outfolder, " --ml ", "--", method)
      res <- system(command=string_mptp, intern = TRUE)
      writeLines(res)

      lines <- readLines(paste0(outfolder, ".txt"))[-c(1:8)] |>
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
  return(mptp_df)
}#
