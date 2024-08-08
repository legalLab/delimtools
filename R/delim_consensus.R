#' Estimate a Majority-Vote Consensus
#'
#' @description
#' \code{delim_consensus()} estimates a majority-vote consensus over the output of
#' \code{\link[delimtools]{delim_join}} in a row-wise manner.
#'
#' @param delim Output from \code{\link[delimtools]{delim_join}}.
#' @param n_match An integer. Threshold for Majority-Vote calculations. If not specified,
#' returns a warning and the threshold will be defined as \code{ceiling(ncol(delim[, -1])/2)}.
#'
#' @details
#' \code{delim_consensus()} iterates row-by-row, counting the number of matching species
#' partition names across all species delimitations methods in \code{\link[delimtools]{delim_join}} output.
#' If the sum of identical partition names is greater or equal \code{n_match},
#' the consensus column will be filled with its partition name. Otherwise,
#' consensus column will be filled with \code{\link[base]{NA}}.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Pedro S. Bittencourt
#'
#' @import dplyr
#' @importFrom cli cli_abort col_yellow cli_warn
#' @importFrom tidyr unnest
#' @importFrom vctrs vec_count
#'
#' @export
delim_consensus <- function(delim, n_match=NULL){
  if(is.null(n_match)){

    n_match <- ceiling(ncol(delim[, -1])/2)

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} {.arg n_match} was not found. Using {.arg n_match= {.val {n_match}}}  instead.")

    cons_delim <- delim %>%
      dplyr::rowwise() %>%
      dplyr::mutate(consensus= list(vctrs::vec_count(dplyr::c_across(2:ncol(.))))) %>%
      tidyr::unnest(consensus) %>%
      dplyr::mutate(count= dplyr::if_else(count >= {{ n_match }}, key, NA)) %>%
      dplyr::distinct(labels, .keep_all = TRUE) %>%
      dplyr::select(-key) %>%
      dplyr::rename(consensus=count)

  }

  else if(n_match <= 1 || n_match >= ncol(delim[,-1])+1){

    cli::cli_abort(c("{n_match} must be a value between {.val 2} and {.val ncol(delim[, -1]).",
                     "i" = "You've supplied an input file with {.val {ncol(delim)}} column{?s}."))

  }

  else if(n_match >= 1 & n_match <= ncol(delim[, -1])){

    cons_delim <- delim %>%
      dplyr::rowwise() %>%
      dplyr::mutate(consensus= list(vctrs::vec_count(dplyr::c_across(2:ncol(.))))) %>%
      tidyr::unnest(consensus) %>%
      dplyr::mutate(count= dplyr::if_else(count >= {{ n_match }}, key, NA)) %>%
      dplyr::distinct(labels, .keep_all = TRUE) %>%
      dplyr::select(-key) %>%
      dplyr::rename(consensus=count)

  }
  return(cons_delim)
}
