#' Run PTP species delimitation (Maximum Likelihood)
#'
#' Alias of \code{\link{mptp}} with \code{method} defaulting to
#' \code{"single"} (the single-rate PTP model), rather than \code{mptp}'s own
#' default of \code{"multi"} (the multi-rate mPTP model).
#'
#' @inheritParams mptp
#'
#' @return
#' See \code{\link{mptp}}.
#'
#' @examples
#' \dontrun{
#' result <- ptp("tree.nwk", outgroup = "Outgroup1",
#'                outgroup_crop = TRUE)
#' print(result)
#' head(result$assignments)
#'
#' library(ape)
#' phy <- read.tree("tree.nwk")
#' result <- ptp(phy)
#' }
#'
#' @seealso \code{\link{mptp}}, \code{\link{ptp_tbl}}
#' @export
ptp <- function(tree,
                 method        = c("single", "multi"),
                 outgroup      = NULL,
                 outgroup_crop = FALSE,
                 pvalue        = 0.001,
                 minbr         = 0.0001,
                 seed          = as.integer(Sys.time()),
                 quiet         = FALSE) {

  method <- match.arg(method)

  mptp(
    tree          = tree,
    method        = method,
    outgroup      = outgroup,
    outgroup_crop = outgroup_crop,
    pvalue        = pvalue,
    minbr         = minbr,
    seed          = seed,
    quiet         = quiet
  )
}
