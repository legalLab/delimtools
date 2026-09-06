#' Turns bGMYC Results Into a Tibble
#'
#' @description
#' `bgmyc_tbl()` processes output from [bgmyc()] (class `"bgmyc_fit"`) or
#' [bgmyc.singlephy][bGMYC::bgmyc.singlephy] (class `"singlebgmyc"`) into an
#' object of class [tbl_df][tibble::tbl_df].
#'
#' For `"bgmyc_fit"` objects the partition is derived from the stored
#' posterior co-occurrence matrix (`$probmat`) by grouping tip pairs whose
#' probability of co-occurrence exceeds `ppcutoff` (union-find).  This lets
#' you explore different thresholds without re-running the MCMC.
#'
#' For legacy `"singlebgmyc"` objects the original \pkg{bGMYC} functions
#' [spec.probmat][bGMYC::spec.probmat] and [bgmyc.point][bGMYC::bgmyc.point]
#' are called (requires the \pkg{bGMYC} package to be installed).
#'
#' @param bgmyc_res Output from [bgmyc()] or
#'   [bgmyc.singlephy][bGMYC::bgmyc.singlephy].
#' @param ppcutoff  Posterior co-occurrence probability threshold. Tip pairs
#'   with posterior probability of being conspecific \eqn{\geq} `ppcutoff`
#'   are merged into the same species. Default 0.05.
#' @param delimname Character. Column name for the species index in the
#'   returned tibble. Default `"bgmyc"`.
#'
#' @return An object of class [tbl_df][tibble::tbl_df] with columns
#'   `labels` (tip labels, in tree-tip order) and the species index named by
#'   `delimname`.
#'
#' @author Noah M. Reid (original bGMYC); Pedro S. Bittencourt (delimtools wrapper).
#'
#' @source
#' Reid N.M., Carstens B.C. 2012. Phylogenetic estimation error can decrease
#' the accuracy of species delimitation: a Bayesian implementation of the general
#' mixed Yule-coalescent model. BMC Evolutionary Biology 12 (196).
#'
#' @seealso [bgmyc()], [gmyc_tbl()]
#'
#' @examples
#'
#'\donttest{
#' # bgmyc (no external package required)
#' result <- try( bgmyc(ape::as.phylo(geophagus_beast),
#'   mcmc = 11000, burnin = 1000, thinning = 100,
#'   start = c(1, 0.5, 30), quiet = TRUE
#' ))
#' bgmyc_df <- try( bgmyc_tbl(result, ppcutoff = 0.05) )
#' try(bgmyc_df)
#'
#' # legacy bGMYC object
#' bgmyc_res <- try( bGMYC::bgmyc.singlephy(ape::as.phylo(geophagus_beast),
#'   mcmc = 11000, burnin = 1000, thinning = 100,
#'   t1 = 2, t2 = ape::Ntip(geophagus_beast),
#'   start = c(1, 0.5, 50)
#' ))
#' bgmyc_df2 <- try( bgmyc_tbl(bgmyc_res, ppcutoff = 0.05) )
#' try(bgmyc_df2)
#'}
#'
#' @export
bgmyc_tbl <- function(bgmyc_res, ppcutoff = 0.05, delimname = "bgmyc") {

  dname <- rlang::sym(delimname)

  # ── bgmyc_fit (bgmyc() output, no external package required) ───────────────
  if (inherits(bgmyc_res, "bgmyc_fit")) {
    if (is.null(bgmyc_res$probmat))
      cli::cli_abort("No {.field probmat} found in {.cls bgmyc_fit} object.")

    df <- .bgmyc_point(bgmyc_res$probmat,
                       bgmyc_res$assignments$labels,
                       ppcutoff,
                       colname = delimname)

    return(tibble::as_tibble(df))
  }

  # ── singlebgmyc (legacy bGMYC package output) ───────────────────────────────
  if (inherits(bgmyc_res, "singlebgmyc")) {
    rlang::check_installed("bGMYC", reason = "to process {.cls singlebgmyc} objects.")

    bgmyc_probmat <- bGMYC::spec.probmat(bgmyc_res)
    splist        <- bGMYC::bgmyc.point(bgmyc_probmat, ppcutoff)

    return(tibble::tibble(
      labels  = unlist(splist),
      !!dname := rep(seq_along(splist), lengths(splist))
    ))
  }

  cli::cli_abort(c(
    "Input must have class {.cls bgmyc_fit} or {.cls singlebgmyc}.",
    "i" = "You supplied an object of class {.cls {class(bgmyc_res)}}."
  ))
}
