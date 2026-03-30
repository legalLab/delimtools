#' Generating a Delimitation Hypothesis Table
#'
#' @description
#' `delim_tbl()` returns species partition hypothesis estimated from a prior delimitation hypothesis supplied by the user.
#'
#' @param labels Vector of unique sequence ID labels.
#' @param sppVector Vector of corresponding morphological species delimitation groups.
#' @param delimname Character. User needs to supply string to name the delimitation method in the table. Default to NA.
#'
#' @details
#' `delim_tbl()` uses information in a species name vector to label each unique sample with a number corresponding to this name.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Rupert A. Collins
#'
#' @examples
#'
#' # create a tibble
#' delim_df <- delim_tbl(
#'   labels = geophagus_info$gbAccession,
#'   sppVector = geophagus_info$scientificName
#'   delimname = "SNAPPER"
#' )
#'
#' # check
#' delim_df
#'
#' @export
delim_tbl <- function(labels, sppVector, delimname = NULL) {
  dname <- rlang::sym(delimname)

  if (missing(labels) | missing(sppVector)) {
    cli::cli_abort("Please provide a vector of sample IDs and/or species identifiers.")
  }
  
  if (is.null(delimname)) {
    cli::cli_alert_warning("Delimname is NULL. Consider using a name that reflects you taxonomic hypothesis, such as 'snapper'.")
  }

  if (length(labels) != length(sppVector)) {
    cli::cli_abort("Error: Please ensure that your number of labels is the same as your number of delimitations.")
  }

  delim <- tibble::tibble(labels = as.character(labels), sppVector = as.character(sppVector)) |>
    dplyr::group_by(sppVector) |>
    dplyr::mutate(!!dname := dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::select(-sppVector)

  return(delim)
}
