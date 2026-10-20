#' Get Darwin Core Terms and Definitions
#'
#' @description
#' `get_dwc()` returns a list of standardized terms and definitions used by the Darwin Core
#' Maintenance Interest Group <https://dwc.tdwg.org/>.
#'
#' @param type Which type of distribution files to download. Available options are:
#' \itemize{
#' \item simple Simple Darwin Core Terms.
#' \item all All Darwin Core Terms.
#' }
#'
#' @details
#' `get_dwc()` reads Darwin Core distribution documents and terms from Github repository
#' <https://github.com/tdwg/dwc> directly into `Environment`. This function will return a list
#' containing the most recent accepted terms as a vector and a [tbl_df][tibble::tbl_df] containing
#' terms, definitions, examples and details about each one of them.
#'
#' @return
#' a list.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins
#' 
#' @examples
#' dwc <- get_dwc(type = "simple") 
#'
#' @export

get_dwc <- function(type){

  # check if `readr` is installed
  rlang::check_installed("readr", reason= "to run `get_dwc` properly.")

  # "simple" maps to simple_dwc_vertical.csv; the full term list is dwc_vertical.csv.
  dist_file <- switch(type,
    simple = "simple_dwc_vertical.csv",
    all = "dwc_vertical.csv",
    stop("`type` must be 'simple' or 'all'.")
  )

  # These distribution files are a bare list of term names with no header
  # row, so col_names must be supplied explicitly -- otherwise readr treats
  # the first term as the header and silently drops it from the data.
  dwc <- readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/tdwg/dwc/master/dist/{dist_file}"),
    col_names = "term",
    show_col_types = FALSE
  )
  terms <- readr::read_csv("https://raw.githubusercontent.com/tdwg/dwc/master/vocabulary/term_versions.csv", show_col_types = FALSE)
  terms <- terms |>
    dplyr::filter(.data$term_localName %in% dwc$term & .data$status == "recommended") |>
    dplyr::distinct(.data$term_localName, .keep_all = TRUE)

  return(list(dwc = dwc$term, terms = terms))
}
