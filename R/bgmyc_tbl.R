#' Turns bGMYC Results Into a Tibble
#'
#' @description
#' \code{bgmyc_tbl()} processes output from \code{\link[bGMYC]{bgmyc.singlephy}} into an
#' object of class \code{\link[tibble]{tbl_df}}.
#'
#' @param bgmyc_res Output from \code{\link[bGMYC]{bgmyc.singlephy}}.
#' @param ppcutoff  Posterior probability threshold for clustering samples into
#' species partitions. See \code{\link[bGMYC]{bgmyc.point}} for details. Default to 0.05.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'bgmyc'.
#'
#' @details
#' \code{\link[bGMYC]{bGMYC}} package uses \code{\link[bGMYC]{spec.probmat}} to create a
#' matrix of probability of conspecificity and \code{\link[bGMYC]{bgmyc.point}}
#' to split samples into a list which individuals
#' meets the threshold specified by \code{ppcutoff}. \code{bgmyc_tbl()} wraps up these
#' two functions into a single one and turns these inputs into a tibble which matches
#' the output from \code{\link[delimtools]{gmyc_tbl()}} and \code{\link[delimtools]{locmin_tbl()}}
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Noah M. Reid.
#'
#' @import bGMYC
#' @importFrom cli cli_abort
#' @importFrom tibble tibble
#' @importFrom rlang sym
#'
#' @export
bgmyc_tbl <- function(bgmyc_res, ppcutoff = 0.05, delimname = "bgmyc"){

  dname <- rlang::sym(delimname)

  if(methods::is(bgmyc_res, "singlebgmyc")){

    bgmyc_probmat <- bGMYC::spec.probmat(bgmyc_res)

    splist <- bGMYC::bgmyc.point(bgmyc_probmat, ppcutoff)

    bgmyc_tbl <- tibble::tibble(labels= unlist(splist),
                                !!dname:= rep(seq_along(splist),
                                           sapply(splist, length)))
    return(bgmyc_tbl)
  } else {

    cli::cli_abort(c("Input data must have class {.cls singlebgmyc}.",
                     "i" = "You've supplied an input of class {.cls {class(bgmyc_res)}}."))

  }
}
