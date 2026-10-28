#' A Command-Line Interface for PTP - Poisson Tree Process
#'
#' @description
#' \code{ptp_tbl()} is an alias of \code{\link{mptp_tbl}} with \code{method}
#' defaulting to \code{"single"} (the single-rate PTP model), rather than
#' \code{mptp_tbl}'s own default of \code{"multi"} (the multi-rate mPTP
#' model).
#'
#' @inheritParams mptp_tbl
#'
#' @return
#' See \code{\link{mptp_tbl}}.
#'
#' @examples
#' \donttest{
#'
#' # get path to phylogram
#' path_to_file <- system.file("extdata/geophagus_raxml.nwk", package = "delimtools")
#'
#' # run PTP in single threshold mode
#' ptp_df <- try( ptp_tbl(
#'   infile = path_to_file,
#'   exe = "/usr/local/bin/mptp",
#'   minbrlen = 0.0001,
#'   outfolder = NULL
#' )
#')
#' # check
#' ptp_df
#' }
#'
#' @seealso \code{\link{mptp_tbl}}, \code{\link{ptp}}
#' @export
ptp_tbl <- function(infile, exe = NULL, outfolder = NULL, method = c("single", "multi"), minbrlen = 0.0001, webserver = NULL, delimname = "ptp") {

  method <- match.arg(method)

  mptp_tbl(
    infile    = infile,
    exe       = exe,
    outfolder = outfolder,
    method    = method,
    minbrlen  = minbrlen,
    webserver = webserver,
    delimname = delimname
  )
}
