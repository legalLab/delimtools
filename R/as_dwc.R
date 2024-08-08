#' Rename Columns using Darwin Core Standard Terms
#'
#' @description
#' \code{as_dwc()} rename columns in a \code{\link[tibble]{tbl_df}} using a vector of terms defined by
#' Darwin Core Standard.
#'
#' @param dwc a list of standard terms and definitions created using \code{\link[delimtools]{get_dwc}}
#' @param data a \code{\link[tibble]{tbl_df}}
#' @param terms a vector or list of terms to be used as replacement.
#'
#' @details
#' \code{as_dwc()} will replace current column names by the ones defined in \code{terms}. For each
#' column in \code{data}, Darwin Core equivalent terms must be informed in the same order
#' by the user. If \code{terms} and column names do not match in length or if \code{terms} used
#' are not found in Darwin Core standard, an error will be printed on Console.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @import dplyr
#' @importFrom cli cli_abort
#' @importFrom tidyselect all_of
#' @importFrom stringr str_flatten_comma
#'
#' @export
as_dwc <- function(dwc, data, terms){

  data_cols <- colnames(data)

  if(length(data_cols) != length(terms)) {
    cli::cli_abort(c("Your column names and terms are not of equal length:",
                     "x" = "You've supplied inputs with different size lengths.",
                     "i" = "Your column length: {length(data_cols)}",
                     "i" = "Your terms length: {length(terms)}"))
  }

  if(all(terms %in% dwc$dwc)) {

    new_terms <- names(data) %>% `names<-`(terms)

    data <- dplyr::rename(data, tidyselect::all_of(new_terms))

    return(data)

  } else {

    missing <- setdiff(terms, dwc$dwc)

    cli::cli_abort(c("Some of your terms do not match Darwin Core list of terms.",
                     "x" = "Some terms provided are either non-valid or deprecated.",
                     "i" = "Found { length(missing) } unmatched term{?s}:",
                     stringr::str_flatten_comma(missing)))
  }
}
