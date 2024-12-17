#' Turns Local Minima Results into a Tibble
#'
#' @description
#' \code{locmin_tbl()} processes output from \code{\link[spider]{tclust}} into
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @param distobj A distance object (usually from \code{\link[ape]{dist.dna}}).
#' @param threshold Distance cutoff for clustering. Default of 0.01. See
#' \code{\link[spider]{localMinima}} for details.
#' @param haps Optional. A vector of haplotypes to keep into the \code{\link[tibble]{tbl_df}}.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'locmin'.
#'
#' @details
#' \code{\link[spider]{spider}} package uses \code{\link[spider]{localMinima}} to
#' determine possible thresholds for any distance matrix and \code{\link[spider]{tclust}}
#' to cluster samples within a given \code{threshold} into species partitions.
#' \code{locmin_tbl()} turns these inputs into a tibble which matches
#' the output from \code{\link[delimtools]{gmyc_tbl()}} and \code{\link[delimtools]{bgmyc_tbl()}}.
#'
#' @return
#' An object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Samuel Brown.
#'
#' @import spider
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom rlang sym
#'
#' @export
locmin_tbl <- function(distobj, threshold = 0.01, haps = NULL, delimname = "locmin"){

  dname <- rlang::sym(delimname)

  if(methods::is(distobj, "dist") & is.null(haps)){

    labs <- labels(distobj)
    clu <- spider::tclust(distobj, threshold) %>%
      lapply(., function(x) labs[x])

    locmin_df <- tibble::tibble(labels= unlist(clu),
                                !!dname:= rep(seq_along(clu),
                                            sapply(clu, length)))


  } else if(is(distobj, "dist") & !is.null(haps)){

    labs <- labels(distobj)
    clu <- spider::tclust(distobj, threshold) %>%
      lapply(., function(x) labs[x])

    locmin_df <- tibble::tibble(labels= unlist(clu),
                                !!dname:= rep(seq_along(clu),
                                            sapply(clu, length))) %>%
      dplyr::filter(labels %in% haps)
  } else {

    cli::cli_abort(c("Input data must have class {.cls dist}.",
                     "i" = "You've supplied an input of class {.cls {class(distobj)}}."))
  }
  return(locmin_df)
}
