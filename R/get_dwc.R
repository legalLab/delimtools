#' Get Darwin Core Terms and Definitions
#'
#' @description
#' \code{get_dwc()} returns a list of standardized terms and definitions used by the Darwin Core
#' Maintenance Interest Group (\url{https://dwc.tdwg.org/}).
#'
#' @param type Which type of distribution files to download. Available options are:
#' \itemize{
#' \item simple Simple Darwin Core Terms.
#' \item all All Darwin Core Terms.
#' }
#'
#' @details
#' \code{get_dwc()} reads Darwin Core distribution documents and terms from Github repository
#' (\url{https://github.com/tdwg/dwc}) directly into Environment. This function will return a list
#' containing the most recent accepted terms as a vector and a \code{\link[tibble]{tbl_df}} containing
#' terms, definitions, examples and details about each one of them.
#'
#' @return
#' a list.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins
#'
#' @import dplyr
#' @importFrom readr read_csv
#'
#' @export

get_dwc <- function(type){

  dwc <- readr::read_csv(glue::glue("https://raw.githubusercontent.com/tdwg/dwc/master/dist/{type}_dwc_vertical.csv"), show_col_types = FALSE)
  terms <- readr::read_csv("https://raw.githubusercontent.com/tdwg/dwc/master/vocabulary/term_versions.csv", show_col_types = FALSE)
  terms <- terms %>%
    dplyr::filter(term_localName %in% dplyr::pull(dwc, type) & status == "recommended") %>%
    dplyr::distinct(term_localName, .keep_all = TRUE)

  return(list(dwc= dplyr::pull(dwc, type), terms= terms))
}
