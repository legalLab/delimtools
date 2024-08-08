confidence_intervals <- function(method, args) {

  gmyc_ci <- function(tr, trees)  {

    trees <- c(tr, trees)

    gmyc_res <- collateral::future_map_quietly(trees,
                                               ~{splits::gmyc(.x, method= "single", quiet=TRUE)})

    gmyc_ci <- purrr::map_int(gmyc_res,
                              ~{summary(.x$result) %>%
                                  capture.output() %>%
                                  stringr::str_extract_all(., "\tnumber of ML entities:\t[0-9]+",
                                                           simplify=TRUE) %>%
                                  stringr::str_replace(., "\tnumber of ML entities:\t", "") %>%
                                  as.numeric() %>%
                                  purrr::discard(., is.na)})

    gmyc_sum <- tibble::tibble(method= "gmyc",
                               point_estimate= gmyc_ci[1],
                               CI_95= as.integer(quantile(gmyc_ci[-1],
                                                          probs=c(0.025, 0.975),
                                                          names = FALSE)) %>%
                                 stringr::str_flatten(., collapse = "—"),
                               CI_mean= as.integer(mean(gmyc_ci[-1])),
                               CI_median= as.integer(stats::median(gmyc_ci[-1])))

  }

  bgmyc_ci <- function(tr, trees, mcmc, burnin, thinning, t1, t2, start= c(py, pc, t)) {

    trees <- c(tr, trees)

    bgmyc_res <- collateral::future_map_quietly(trees,
                                                ~{bGMYC::bgmyc.singlephy(.x,
                                                                         mcmc= mcmc,
                                                                         burnin= burnin,
                                                                         thinning= thinning,
                                                                         t1= t1,
                                                                         t2= t2,
                                                                         start= start)})
    bgmyc_ci <- purrr::map_int(bgmyc_res,
                               ~{.x$result %>%
                                   bGMYC::spec.probmat(.) %>%
                                   bGMYC::bgmyc.point(., ppcutoff=0.05) %>%
                                   length()})

    bgmyc_sum <- tibble::tibble(method= "bgmyc",
                                point_estimate= bgmyc_ci[1],
                                CI_95= as.integer(quantile(bgmyc_ci[-1],
                                                           probs=c(0.025, 0.975),
                                                           names = FALSE)) %>%
                                  stringr::str_flatten(., collapse = "—"),
                                CI_mean= as.integer(mean(bgmyc_ci[-1])),
                                CI_median= as.integer(stats::median(bgmyc_ci[-1])))

  }

  ci_res <- purrr::map2(.x= method, .y= args, .f= ~{rlang::exec(.x, !!!.y)}) %>%
    purrr::list_rbind()

  return(ci_res)
}
