#' Summarise Haplotypes Down to One Row
#'
#' @description
#' \code{haplotype_tbl()} returns a \code{\link[tibble]{tbl_df}} summarising
#' all unique haplotype frequencies and duplicates into a single row.
#'
#' @param dna an object of class \code{\link[ape]{DNAbin}}.
#' @param verbose logical. Returns a warning if any sequence contains non ACTG bases.
#' See \code{link[delimtools]{clean_dna}} for details.
#'
#' @details
#' \code{haplotype_tbl()} uses a combination of \code{link[delimtools]{clean_dna}} and
#' \code{link[delimtools]{hap_collapse}} to summarise haplotypes into a tibble. Each row
#' of the tibble has an unique haplotype, its frequency and all its collapsed duplicates in a
#' flattened string.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt.
#'
#' @import dplyr
#' @import tibble
#' @importFrom delimtools clean_dna hap_collapse
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom stringr str_detect
#'
#' @export
haplotype_tbl <- function(dna, verbose= TRUE){

  if(verbose==TRUE){
    # clean dna
    dat_all_ali <- delimtools::clean_dna(dna, verbose = TRUE)
  } else {
    # clean dna
    dat_all_ali <- delimtools::clean_dna(dna, verbose = FALSE)
  }

  # collapse to haplotypes
  dat_all_haps <- delimtools::hap_collapse(dat_all_ali, collapseSubstrings= TRUE, clean=FALSE)

  # convert to all to character
  dat_haps_char <- lapply(dat_all_haps, function(x) paste(x, collapse=""))
  dat_all_char <- lapply(dat_all_ali, function(x) paste(x, collapse=""))
  dat_collapseds_char <- dat_all_char[which(!names(dat_all_char) %in% names(dat_haps_char))]

  # detect strings for all collapseds of each unique haplotype
  seqs_in <- mapply(FUN=function(x) which(stringr::str_detect(string=dat_haps_char, pattern=x)==TRUE), dat_collapseds_char, SIMPLIFY=TRUE, USE.NAMES=FALSE)
  seqs_in <- mapply(function(x) names(dat_haps_char[x]), seqs_in)
  names(seqs_in) <- names(dat_collapseds_char)

  # turn into dataframe
    pair_list <- tidyr::unnest(tibble::enframe(seqs_in,name="collapsed", value= "labels"), cols = "labels") %>%
      dplyr::distinct(collapsed, .keep_all = TRUE) %>%
      dplyr::group_by(labels) %>%
      dplyr::reframe(n_seqs= n(),
                     collapsed= paste(collapsed, collapse = ", "))

    # join
    haps_df <- tibble::tibble(labels= unlist(names(dat_haps_char))) %>%
      dplyr::full_join(., pair_list, by="labels") %>%
      dplyr::mutate(n_seqs= tidyr::replace_na(n_seqs, 1),
                    n_seqs= dplyr::if_else(!is.na(collapsed), n_seqs+1, n_seqs)) %>%
      dplyr::arrange(desc(n_seqs))

  return(haps_df)
}



