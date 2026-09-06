#' Internal: precompute GMYC tree state

#' @keywords internal
.bgmyc_prep <- function(tree) {
  if (!inherits(tree, "phylo"))
    stop("'tree' must be a 'phylo' object.")
  if (!ape::is.ultrametric(tree))
    stop("Tree must be ultrametric.")
  if (!ape::is.binary(tree))
    stop("Tree must be fully binary. Use ape::multi2di() to resolve polytomies.")

  n_tips    <- length(tree$tip.label)
  tip_rows  <- which(tree$edge[, 2L] <= n_tips)
  if (any(tree$edge.length[tip_rows] == 0))
    stop("Tree contains tip branches with zero length.")

  bt         <- -ape::branching.times(tree)
  bt[bt > -1e-6] <- -1e-6
  # Order by APE node number, NOT sorted by time value (required by C_gmyc_setup)
  bt_ordered <- bt[order(as.integer(names(bt)))]

  n_internal <- length(bt)
  n_nodes    <- n_tips + n_internal

  setup <- .Call(C_gmyc_setup,
                 tree$edge, tree$edge.length, tree$tip.label,
                 n_tips, n_nodes, bt_ordered)

  list(setup = setup, n_tips = n_tips, n_internal = n_internal)
}

#' Internal: resolve t2 bound and validate start

#' @keywords internal
.bgmyc_resolve_t2 <- function(t2, n_internal, t1, start) {
  if (is.null(t2)) t2 <- n_internal - 1L
  t2 <- min(as.integer(t2), n_internal - 1L)
  if (start[3L] < t1 || start[3L] > t2)
    stop(sprintf("start[3] = %g is outside [t1 = %d, t2 = %d].",
                 start[3L], t1, t2))
  t2
}

#' Internal: build co-occurrence probability matrix from retained samples

#' @keywords internal
.bgmyc_probmat <- function(par, setup, n_tips) {
  n_samp <- nrow(par)
  if (n_samp == 0L) return(matrix(0.0, n_tips, n_tips))

  comat <- matrix(0.0, nrow = n_tips, ncol = n_tips)
  for (i in seq_len(n_samp)) {
    cl    <- .Call(C_gmyc_clusters, setup, as.integer(par[i, 3L]) - 1L)
    comat <- comat + outer(cl, cl, "==")
  }
  comat / n_samp
}

#' Internal: point-estimate partition from co-occurrence matrix

#' @keywords internal
.bgmyc_point <- function(probmat, tip_labels, ppcutoff, colname = "bgmyc") {
  n      <- nrow(probmat)
  parent <- seq_len(n)

  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      if (probmat[i, j] >= ppcutoff) {
        ri <- i; while (parent[ri] != ri) ri <- parent[ri]
        rj <- j; while (parent[rj] != rj) rj <- parent[rj]
        if (ri != rj) parent[ri] <- rj
      }
    }
  }

  comp  <- vapply(seq_len(n),
                  function(x) { while (parent[x] != x) x <- parent[x]; x },
                  integer(1L))
  sp_id <- as.integer(factor(comp, levels = unique(comp)))

  stats::setNames(
    data.frame(tip_labels, sp_id, stringsAsFactors = FALSE),
    c("labels", colname)
  )
}

#' Public API

#' Run Bayesian GMYC species delimitation
#'
#' Runs a Bayesian implementation of the General Mixed Yule-Coalescent (bGMYC)
#' model using the embedded C GMYC engine.  Likelihood evaluations are performed
#' in C (via \code{C_gmyc_loglik}), eliminating the dependency on the external
#' \pkg{bGMYC} package.
#'
#' The Metropolis-within-Gibbs sampler follows Reid & Carstens (2012): a gamma
#' proposal for \code{py} and \code{pc}, and a Gaussian random walk for the
#' integer threshold \code{t}.  The prior is uniform on
#' \code{[py1, py2] * [pc1, pc2] * \{t1, ..., t2\}}.
#'
#' @param tree An ultrametric, fully binary \code{\link[ape]{phylo}} object.
#' @param mcmc Integer. Total MCMC steps (default 11 000).
#' @param burnin Integer. Steps discarded as burn-in (default 1 000).
#' @param thinning Integer. Thinning interval; every \code{thinning}-th
#'   post-burnin step is retained (default 100).
#' @param py1,py2 Numeric. Prior bounds for the Yule rate-change exponent
#'   (default 0 and 2).
#' @param pc1,pc2 Numeric. Prior bounds for the coalescent rate-change exponent
#'   (default 0 and 2).
#' @param t1,t2 Integer. Prior bounds for the threshold parameter (number of
#'   species). Defaults: \code{t1 = 2}, \code{t2 = NULL} (auto: \code{Nnode - 1}).
#' @param scale Numeric vector of length 3. Proposal scale parameters for
#'   \code{py}, \code{pc}, and \code{t} (default \code{c(20, 10, 5)}).
#' @param start Numeric vector of length 3. Starting values for
#'   \code{py}, \code{pc}, and \code{t} (default \code{c(1, 0.5, 50)}).
#' @param ppcutoff Numeric. Posterior co-occurrence probability threshold used
#'   to compute the point-estimate \code{$assignments}. Tip pairs with
#'   posterior probability of co-occurrence \eqn{\geq} \code{ppcutoff} are
#'   merged into the same species (default 0.05). Can be recomputed later via
#'   \code{\link{bgmyc_tbl}}.
#' @param quiet Logical. Suppress progress output (default \code{FALSE}).
#'
#' @return An object of class \code{"bgmyc_fit"}, a named list with:
#'   \describe{
#'     \item{\code{par}}{Matrix of retained MCMC samples (rows = samples,
#'       columns \code{py}, \code{pc}, \code{t}, \code{loglik}).}
#'     \item{\code{assignments}}{Data frame with columns \code{labels}
#'       (tip labels) and \code{bgmyc} (integer species index), giving the
#'       point-estimate partition at \code{ppcutoff}.}
#'     \item{\code{probmat}}{Numeric matrix (\code{n_tips * n_tips}) of
#'       posterior co-occurrence probabilities. Use with
#'       \code{\link{bgmyc_tbl}} to recompute the partition at any cutoff.}
#'     \item{\code{accept_rates}}{Named numeric vector of per-parameter
#'       acceptance rates (\code{py}, \code{pc}, \code{t}).}
#'     \item{\code{mcmc}, \code{burnin}, \code{thinning}}{Integer scalars.}
#'     \item{\code{tree}}{The input \code{phylo} object.}
#'   }
#'
#' @references
#' Reid N.M., Carstens B.C. 2012. Phylogenetic estimation error can decrease
#' the accuracy of species delimitation: a Bayesian implementation of the
#' general mixed Yule-coalescent model. \emph{BMC Evolutionary Biology} 12, 196.
#'
#' @seealso \code{\link{bgmyc_tbl}}, \code{\link{gmyc}}, \code{\link{mptp_mcmc}}
#' @examples
#' \dontrun{
#' library(ape)
#' phy <- as.phylo(geophagus_beast)
#' result <- bgmyc(phy, mcmc = 11000, burnin = 1000, thinning = 100,
#'                 start = c(1, 0.5, 30))
#' summary(result)
#' head(result$assignments)
#' bgmyc_tbl(result, ppcutoff = 0.95)
#' }
#' @export
bgmyc <- function(tree,
                      mcmc     = 11000L,
                      burnin   = 1000L,
                      thinning = 100L,
                      py1 = 0,   py2 = 2,
                      pc1 = 0,   pc2 = 2,
                      t1  = 2L,  t2  = NULL,
                      scale    = c(20, 10, 5),
                      start    = c(1.0, 0.5, 50.0),
                      ppcutoff = 0.05,
                      quiet    = FALSE) {

  mcmc     <- as.integer(mcmc)
  burnin   <- as.integer(burnin)
  thinning <- as.integer(thinning)
  t1       <- as.integer(t1)

  prep <- .bgmyc_prep(tree)
  t2   <- .bgmyc_resolve_t2(t2, prep$n_internal, t1, start)

  if (!quiet)
    cat(sprintf("bGMYC (C engine): %d tips, %d MCMC steps, %d post-burnin samples\n",
                prep$n_tips, mcmc, floor((mcmc - burnin) / thinning)))

  out <- .Call(C_bgmyc_mcmc,
               prep$setup,
               mcmc, burnin, thinning,
               c(py1, py2, pc1, pc2, as.double(t1), as.double(t2)),
               as.double(scale),
               c(as.double(start[1L]), as.double(start[2L]), as.double(start[3L])),
               as.integer(quiet))

  probmat     <- .bgmyc_probmat(out$par, prep$setup, prep$n_tips)
  assignments <- .bgmyc_point(probmat, tree$tip.label, ppcutoff)

  structure(
    list(
      par          = out$par,
      assignments  = assignments,
      probmat      = probmat,
      accept_rates = setNames(out$accept, c("py", "pc", "t")),
      mcmc         = mcmc,
      burnin       = burnin,
      thinning     = thinning,
      tree         = tree,
      call         = match.call()
    ),
    class = "bgmyc_fit"
  )
}

#' Summary of a Bayesian GMYC (bGMYC) delimitation
#' @description Prints a summary of the MCMC run and species delimitation
#'   results from a \code{bgmyc_fit} object.
#'
#' @param object An object of class \code{bgmyc_fit}, as returned by
#'   \code{bgmyc()} (or equivalent fitting function).
#' @param ... Additional arguments, currently ignored (included for
#'   S3 method consistency).
#'
#' @return Invisibly returns \code{object}.
#'
#' @method summary bgmyc_fit
#' @export

#' @export
summary.bgmyc_fit <- function(object, ...) {
  n_samp  <- nrow(object$par)
  n_sp    <- if (n_samp > 0L) max(object$assignments[[2L]]) else NA_integer_
  cat("Bayesian GMYC (bGMYC) Delimitation\n")
  cat("------------------------------------\n")
  cat(sprintf("  Tips          : %d\n",         nrow(object$assignments)))
  cat(sprintf("  MCMC steps    : %d\n",         object$mcmc))
  cat(sprintf("  Burn-in       : %d\n",         object$burnin))
  cat(sprintf("  Thinning      : %d\n",         object$thinning))
  cat(sprintf("  Retained      : %d samples\n", n_samp))
  cat(sprintf("  Accept (py)   : %.4f\n",       object$accept_rates[["py"]]))
  cat(sprintf("  Accept (pc)   : %.4f\n",       object$accept_rates[["pc"]]))
  cat(sprintf("  Accept (t)    : %.4f\n",       object$accept_rates[["t"]]))
  if (n_samp > 0L) {
    t_vals <- object$par[, "t"]
    cat(sprintf("  N species (t) : mean %.1f  [%d - %d]\n",
                mean(t_vals), as.integer(min(t_vals)), as.integer(max(t_vals))))
    cat(sprintf("  Point est.    : %d species (ppcutoff = %g)\n", n_sp,
                if (!is.null(object$call$ppcutoff)) object$call$ppcutoff else 0.05))
  }
  invisible(object)
}
