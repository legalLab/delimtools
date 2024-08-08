#' Print Darwin Core Terms, Definitions and Examples as Bullet Lists
#'
#' @description
#' \code{dwc_terms()} checks a vector or list of terms and return definitions and examples for
#' each one of them.
#'
#' @param dwc a list of standard terms and definitions created using \code{\link[delimtools]{get_dwc}}
#' @param terms a vector or list of terms to check.
#'
#' @details
#' For each term in a vector or list, \code{dwc_terms} will return a bullet list containing
#' the term, followed by its definition and examples.
#'
#' @return
#' a bullet list.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#' dwc <- get_dwc(type= "simple")
#' dwc_terms(dwc, c("genus", "scientificName"))
#'
#' @import dplyr
#' @importFrom stringr str_glue_data
#' @importFrom cli cli_li
#'
#' @export
dwc_terms <- function(dwc, terms){

  dwc$terms %>%
    dplyr::filter(term_localName %in% {{ terms }}) %>%
    dplyr::arrange(match(term_localName, {{ terms }})) %>%
    stringr::str_glue_data(., "{ term_localName }: { definition } Examples: { examples }", sep= " ") %>%
    cli::cli_li()

}
