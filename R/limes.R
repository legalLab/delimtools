limes <- function(delim) {

  n_cols <- colnames(delim[,-1])

  pairs <- n_cols %>%
    gtools::combinations(n= length(.), r= 2, repeats.allowed = FALSE, v=.) %>%
    array_tree(., margin=1)

  ctax <- map(pairs,
              ~{ delim %>%
                  dplyr::select(c(labels, {{ .x }} )) %>%
                  dplyr::mutate(order_x= vctrs::vec_group_id(pick(2)),
                                order_y= vctrs::vec_group_id(pick(3))) %>%
                  dplyr::mutate(union= dplyr::cur_group_id(), .by= c(2, 3)) %>%
                  dplyr::mutate(n.match= if_else(pick(2) == pick(3), union, 0)) %>%
                  # dplyr::mutate(row_number= dplyr::row_number(),
                  #        seq_x= cumsum(c(1, diff(order_x) > 0)),
                  #        seq_y= cumsum(c(1, diff(order_y) > 0)),
                  #        rank_x= rank(seq_x, ties.method = "min"),
                  #        rank_y= rank(seq_y, ties.method = "min"),
                  #        inters= match(row_number, dplyr::intersect(rank_x, rank_y))) %>%
                  # tidyr::fill(inters, .direction = "down") %>%
                  dplyr::summarise(pairs= paste(.x[1], .x[2], sep="-"),
                                   delim_1= dplyr::n_distinct(pick(2)),
                                   delim_2= dplyr::n_distinct(pick(3)),
                                   matchRatio= round((2*dplyr::n_distinct(n.match[n.match > 0]))/(delim_1 + delim_2), digits = 2))
                                   # ctax_frac= paste0(((dplyr::n_distinct(inters)-1)), "/", (dplyr::n_distinct(union)-1)),
                                   # ctax= round((dplyr::n_distinct(inters)-1)/(dplyr::n_distinct(union)-1), digits = 2))
                }) %>% rlang::set_names(sapply(pairs, stringr::str_flatten, collapse="-")) %>%
    purrr::list_rbind() %>%
    dplyr::group_by(delim_1) %>%
    dplyr::arrange(desc(delim_1), .by_group = TRUE) %>%
    dplyr::ungroup()

  rtax <- delim %>%
    dplyr::mutate(union= dplyr::cur_group_id(), .by = n_cols) %>%
    dplyr::summarise(across(n_cols, ~ round((dplyr::n_distinct(.x)-1)/(dplyr::n_distinct(union)-1), digits = 2)))

  return(list(ctax= ctax, rtax= rtax))

}

