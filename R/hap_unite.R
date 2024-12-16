#' Unite Haplotype Summaries with Species Delimitation Outputs
#' 
#' @description
#' \code{hap_unite()} returns a single \code{\link[tibble]{tbl_df}} combining all
#' results from \code{\link[delimtools]{haplotype_tbl}} or \code{\link[delimtools]{collapse_others}} 
#' with results from \code{\link[delimtools]{delim_join}} or \code{\link[delimtools]{delim_consensus}}.
#' 
#' @param hap_tbl output from \code{\link[delimtools]{haplotype_tbl}} or 
#' \code{\link[delimtools]{collapse_others}}. 
#' @param delim output from \code{\link[delimtools]{delim_join}} or 
#' \code{\link[delimtools]{delim_consensus}}.
#' 
#' @details
#' Many functions in this package relies on the usage of unique haplotypes due to 
#' known issues when using identical or duplicated sequences for species delimitation analysis. 
#' Thus, these outputs will very often refer only to unique haplotypes within a given dataset, 
#' which can be determined by using functions like \code{\link[delimtools]{hap_collapse}}. 
#' Assuming that a duplicated or identical sequence should share the same properties as the first 
#' sequence of the group has, \code{hap_unite()} combines the output of \code{\link[delimtools]{haplotype_tbl}} 
#' with the output of \code{\link[delimtools]{delim_join}}. Alternativelly, one may use 
#' \code{\link[delimtools]{collapse_others}} and \code{\link[delimtools]{delim_consensus}} as well. 
#' This output may be used for downstream analysis or to determine in which cluster a given sequence belongs.
#' 
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#' 
#' @author
#' Pedro S. Bittencourt
#' 
#' @import dplyr
#' @importFrom stringr str_split
#' @importFrom tidyr unnest drop_na
#' 
#' @export
hap_unite <- function(hap_tbl, delim){
  
  delim <- delim |>
    dplyr::mutate(status= "haplotype")
  
  collapsed_df <- hap_tbl |>
    dplyr::select(labels, collapsed) |>
    dplyr::mutate(collapsed= stringr::str_split(collapsed, pattern= ", ")) |>
    tidyr::unnest(cols= c(collapsed)) |>
    dplyr::left_join(delim, by= "labels") |>
    tidyr::drop_na(collapsed) |>
    dplyr::select(-labels) |>
    dplyr::rename(labels= collapsed) |>
    dplyr::mutate(status= "collapsed")
  
  all_haps <- dplyr::left_join(hap_tbl, delim, by= "labels") |>
    dplyr::bind_rows(collapsed_df)
  
  return(all_haps)
}
