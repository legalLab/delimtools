#' Turns GMYC Results Into a Tibble
#'
#' @description
#' \code{gmyc_tbl()} processes output from \code{\link[splits]{gmyc}} into an
#' object of class \code{\link[tibble]{tbl_df}}.
#'
#' @param gmyc_res Output from \code{\link[splits]{gmyc}}.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'gmyc'.
#'
#' @details
#' \code{\link[splits]{splits}} package uses \code{\link[splits]{gmyc}} to optimize
#' genetic clusters and \code{\link[splits]{spec.list}} to cluster samples into
#' species partitions. \code{gmyc_tbl()} turns these results into a tibble which matches
#' the output from \code{\link[delimtools]{bgmyc_tbl()}} and \code{\link[delimtools]{locmin_tbl()}}.
#'
#' @return
#' An object of class \code{\link[tibble]{tbl_df}}.
#'
#' @author
#' Thomas Ezard, Tomochika Fujisawa, Tim Barraclough.
#'
#' @import splits
#' @importFrom methods is
#' @importFrom cli cli_abort
#' @importFrom tibble tibble
#' @importFrom rlang sym
#'
#' @export
gmyc_tbl <- function(gmyc_res, delimname = "gmyc"){

  dname <- rlang::sym(delimname)

  if(methods::is(gmyc_res, "gmyc")){

    gmyc_spec <- splits::spec.list(gmyc_res)

    gmyc_tbl <- tibble::tibble(labels= as.character(gmyc_spec$sample_name),
                               !!dname:= gmyc_spec$GMYC_spec)

    return(gmyc_tbl)

  } else {

    cli::cli_abort(c("Input data must have class {.cls gmyc}.",
                     "i" = "You've supplied an input of class {.cls {class(gmyc_res)}}."))
  }
}
