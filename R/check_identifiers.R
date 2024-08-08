#' Checks for Differences Between Identifiers in Metadata and DNA Sequence Files
#'
#' @description
#' \code{check_identifiers()} checks for differences between identifiers in metadata
#' and DNA sequence files.
#'
#' @param data an object of class \code{\link[tibble]{tbl_df}} containing sequence metadata.
#' @param identifier column in \code{data} which contains sequence identifiers.
#' @param dna a \code{\link[ape]{DNAbin}} object.
#'
#' @details
#' \code{check_identifiers()} is a helper function to check for inconsistencies
#' between identifiers in metadata and DNA sequences files, such as absence, mistyping,
#' duplicated entries, or differences in size lengths. If any of these problems are found,
#' warnings will appear in \code{Console} and corrections should be made to prevent
#' unintended consequences later. A list containing erroneous identifiers is returned
#' invisibly.
#'
#' @return
#' A list containing erroneus identifiers between metadata and sequence file.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @import cli
#' @import dplyr
#' @importFrom rlang is_empty
#' @importFrom stringr str_flatten_comma
#' @importFrom vctrs vec_duplicate_detect
#' @importFrom vctrs vec_chop
#'
#' @export
check_identifiers <- function(data, identifier, dna){

  id1 <- dplyr::pull(data, {{ identifier }})
  id2 <- names(dna)

  if(length(id1) != length(id2)){
    cli::cli_abort(c("Identifiers are not of equal length:",
                     "x" = "You've supplied inputs with different size lengths.",
                     "i" = "Your data length: {length(id1)}",
                     "i" = "Your sequence length: {length(id2)}"))
  }

  if(any(duplicated(id1)|duplicated(id2))){
    cli::cli_abort(c("Duplicate identifiers found.",
                     "x" = "You've supplied inputs with duplicated identifier names.",
                     "i" = "Duplicated identifiers in data file:",
                     stringr::str_flatten_comma(id1[vctrs::vec_duplicate_detect(id1)]),
                     "i" = "Duplicated identifiers in sequence file:",
                     stringr::str_flatten_comma(id2[vctrs::vec_duplicate_detect(id2)])))
  }

  diff <- dplyr::symdiff(id1, id2)

  if(rlang::is_empty(diff)){
    cli::cli_alert_success(c("Identifiers are the same across files."))

  } else {

    diff <- vctrs::vec_chop(diff, sizes= c(length(diff)/2, length(diff)/2))
    names(diff) <- c("Identifiers absent or mistyped in data file",
                     "Identifiers absent or mistyped in sequence file")

    cli::cli_warn(c("Identifiers must be identical across files.",
                    "x" = "The identifiers bellow are either absent or mistyped.",
                    "i" = "Identifiers absent or mistyped in data file:",
                    stringr::str_flatten_comma(diff[[1]]),
                    "i" = "Identifiers absent or mistyped in sequence file:",
                    stringr::str_flatten_comma(diff[[2]])))
    invisible(diff)
  }
}
