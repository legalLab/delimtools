#' Compute Agreement Between Alternative Species Delimitation Partitions
#' 
#' @description
#' \code{match_ratio()} uses the Match Ratio statistic of Ahrens et al. (2014) to
#' compute agreement between all pairs of species delimitation partitions in
#' \code{\link[delimtools]{delim_join}} output.
#' 
#' @param delim Output from \code{\link[delimtools]{delim_join}}.
#' 
#' @details
#' \code{match_ratio()} iterates between all species delimitation partitions in
#' \code{\link[delimtools]{delim_join}} output and returns a \code{\link[tibble]{tbl_df}} 
#' containing the following columns:
#' \itemize{
#'   \item \code{pairs} pairs of species delimitation methods analyzed.
#'   \item \code{delim_1} number of species partitions in method 1.
#'   \item \code{delim_2} number of species partitions in method 2.
#'   \item \code{n_match} number of identical species partitions in methods 1 and 2.
#'   \item \code{match_ratio} match ratio statistic, where 0 indicates no agreement between 
#'   pairs of species delimitation partitions and 1 indicates complete agreement between
#'   them.}
#'   
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#' 
#' @author
#' Pedro S. Bittencourt
#' 
#' @import dplyr
#' @importFrom gtools combinations
#' @importFrom purrr array_tree list_rbind map
#' @importFrom rlang set_names
#' @importFrom vctrs vec_group_id
#' @importFrom stringr str_c str_flatten
#' 
#' @export 
match_ratio <- function(delim){
  n_cols <- colnames(delim[,-1])

  pairs <- n_cols %>%
    gtools::combinations(n= length(.), r= 2, repeats.allowed = FALSE, v=.) %>%
    purrr::array_tree(., margin=1)

  match_ratio <- purrr::map(pairs,
                     ~{ delim %>%
                         dplyr::select(c(labels, {{ .x }} )) %>%
                         dplyr::mutate(order_x= vctrs::vec_group_id(dplyr::pick(2)),
                                       order_y= vctrs::vec_group_id(dplyr::pick(3))) %>%
                         dplyr::mutate(union= dplyr::cur_group_id(), .by= c(2, 3)) %>%
                         dplyr::mutate(n_match= dplyr::if_else(dplyr::pick(2) == dplyr::pick(3), union, 0)) %>%
                         dplyr::summarise(pairs= stringr::str_c(.x[1], .x[2], sep="-"),
                                          delim_1= dplyr::n_distinct(pick(2)),
                                          delim_2= dplyr::n_distinct(pick(3)),
                                          n_match= dplyr::n_distinct(n_match[n_match > 0]),
                                          match_ratio= round(2*n_match/(delim_1 + delim_2), digits = 2))}) %>%
    rlang::set_names(sapply(pairs, stringr::str_flatten, collapse="-")) %>%
    purrr::list_rbind() %>%
    dplyr::group_by(pairs) %>%
    dplyr::arrange(dplyr::desc(pairs), .by_group = TRUE) %>%
    dplyr::ungroup()

  return(match_ratio= match_ratio)

}
