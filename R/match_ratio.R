match_ratio <- function(delim){
  n_cols <- colnames(delim[,-1])

  pairs <- n_cols %>%
    gtools::combinations(n= length(.), r= 2, repeats.allowed = FALSE, v=.) %>%
    array_tree(., margin=1)

  match_ratio <- map(pairs,
                     ~{ delim %>%
                         dplyr::select(c(labels, {{ .x }} )) %>%
                         dplyr::mutate(order_x= vctrs::vec_group_id(pick(2)),
                                       order_y= vctrs::vec_group_id(pick(3))) %>%
                         dplyr::mutate(union= dplyr::cur_group_id(), .by= c(2, 3)) %>%
                         dplyr::mutate(n_match= if_else(pick(2) == pick(3), union, 0)) %>%
                         dplyr::summarise(pairs= stringr::str_c(.x[1], .x[2], sep="-"),
                                          delim_1= dplyr::n_distinct(pick(2)),
                                          delim_2= dplyr::n_distinct(pick(3)),
                                          n_match= dplyr::n_distinct(n_match[n_match > 0]),
                                          match_ratio= round(2*n_match/(delim_1 + delim_2), digits = 2))}) %>%
    rlang::set_names(sapply(pairs, stringr::str_flatten, collapse="-")) %>%
    purrr::list_rbind() %>%
    dplyr::group_by(pairs) %>%
    dplyr::arrange(desc(pairs), .by_group = TRUE) %>%
    dplyr::ungroup()

  return(match_ratio= match_ratio)

}
