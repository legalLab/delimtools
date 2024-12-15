#' Summarise Haplotype Metadata Down to One Row
#'
#' @description
#' \code{collapse_others()} returns a \code{\link[tibble]{tbl_df}} summarising
#' all unique haplotype frequencies, duplicates and selected metadata into a single row.
#'
#' @param data an object of class \code{\link[tibble]{tbl_df}} containing sequence metadata.
#' @param hap_tbl output from \code{link[delimtools]{haplotype_tbl()}}.
#' @param identifier a character vector of unique sequence labels.
#' @param cols a character vector of variables to collapse.
#'
#' @details
#' \code{collapse_others()} is a helper function to summarise metadata along with
#' \code{\link[delimtools]{haplotype_tbl()}}. For any given \code{cols},
#' \code{collapse_others()} flattens its content by unique haplotypes and its duplicates
#' in \code{hap_tbl}.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @import purrr
#' @import dplyr
#' @importFrom stringr str_split
#' @importFrom tidyr unnest
#' @importFrom tidyselect ends_with
#'
#' @export
collapse_others <- function(data, hap_tbl, identifier, cols){

  collapse_tbl <- purrr::map(cols,
                             ~{ hap_tbl %>%
                                 dplyr::mutate(collapsed= stringr::str_split(collapsed, ", ")) %>%
                                 tidyr::unnest(c(collapsed, n_seqs)) %>%
                                 dplyr::mutate(x= dplyr::pull(data, .x)[match(labels, dplyr::pull(data, {{ identifier }}))],
                                               y= dplyr::pull(data, .x)[match(collapsed, dplyr::pull(data, {{ identifier }}))]) %>%
                                 dplyr::mutate("collapsed_{.x}" := dplyr::if_else(is.na(y),
                                                                                  x,
                                                                                  paste(unique(c(x, y)), collapse = ", ")), .by = labels) %>%
                                 dplyr::select(c(labels, n_seqs, tidyselect::ends_with(cols))) %>%
                                 dplyr::distinct(labels, .keep_all = TRUE) %>%
                                 dplyr::full_join(hap_tbl, ., by= c("labels", "n_seqs"))}) %>%
    purrr::reduce(., dplyr::full_join, by= c("labels", "n_seqs", "collapsed"))

  return(collapse_tbl)
}
