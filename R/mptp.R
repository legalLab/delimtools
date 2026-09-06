# ── Internal: resolve tree input (file path or ape phylo) ─────────────────────

#' @keywords internal
.resolve_tree_input <- function(tree, tmpfiles) {
  if (inherits(tree, "phylo")) {
    if (!requireNamespace("ape", quietly = TRUE))
      stop("Package 'ape' is required to pass a phylo object.")
    tmp <- tempfile(fileext = ".nwk")
    tmpfiles$path <- tmp
    ape::write.tree(tree, file = tmp)
    return(tmp)
  }
  if (!is.character(tree) || length(tree) != 1L)
    stop("'tree' must be a file path (character) or a phylo object.")
  normalizePath(tree, mustWork = TRUE)
}

# ── Internal: call the C library ──────────────────────────────────────────────

#' @keywords internal
.mptp_call_ml <- function(tree_file, outfile, outgroup, method,
                           minbr, pvalue, quiet, seed, crop) {
  .Call(rmptp_ml,
        tree_file,
        outfile,
        outgroup,
        as.integer(method),
        as.double(minbr),
        as.double(pvalue),
        as.integer(quiet),
        as.double(seed),
        as.integer(crop))
}

#' @keywords internal
.mptp_call_mcmc <- function(tree_file, outfile, outgroup, method,
                             minbr, pvalue, quiet, seed, crop,
                             mcmc_steps, mcmc_sample, mcmc_burnin,
                             mcmc_runs, mcmc_credible,
                             mcmc_startnull, mcmc_startrandom, mcmc_startml) {
  .Call(rmptp_mcmc,
        tree_file,
        outfile,
        outgroup,
        as.integer(method),
        as.double(minbr),
        as.double(pvalue),
        as.integer(quiet),
        as.double(seed),
        as.integer(crop),
        as.double(mcmc_steps),
        as.integer(mcmc_sample),
        as.double(mcmc_burnin),
        as.integer(mcmc_runs),
        as.double(mcmc_credible),
        as.integer(mcmc_startnull),
        as.integer(mcmc_startrandom),
        as.integer(mcmc_startml))
}

# ── Internal: build structured result from raw .Call() output ─────────────────

#' @keywords internal
.build_mptp_ml <- function(raw, method) {
  asgn   <- raw$assignments
  sp_col <- if (method == "multi") "mptp" else "ptp"

  df <- setNames(
    data.frame(asgn$taxon, asgn$species, stringsAsFactors = FALSE),
    c("labels", sp_col)
  )

  structure(
    list(
      n_species   = raw$n_species,
      method      = method,
      null_logl   = raw$null_logl,
      best_logl   = raw$best_logl,
      pvalue      = raw$pvalue,
      lrt_passed  = isTRUE(raw$lrt_passed),
      edge_count  = raw$edge_count,
      total_edges = raw$total_edges,
      assignments = df
    ),
    class = "mptp_ml"
  )
}

#' @keywords internal
.build_mptp_mcmc <- function(raw, method,
                              mcmc_steps, mcmc_sample, mcmc_burnin,
                              mcmc_runs, seed) {
  ml_part <- .build_mptp_ml(raw, method)

  support <- data.frame(
    node_label = raw$support$node_label,
    support    = raw$support$support,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      n_species   = ml_part$n_species,
      method      = method,
      null_logl   = ml_part$null_logl,
      best_logl   = ml_part$best_logl,
      pvalue      = ml_part$pvalue,
      lrt_passed  = ml_part$lrt_passed,
      edge_count  = ml_part$edge_count,
      total_edges = ml_part$total_edges,
      assignments = ml_part$assignments,
      support     = support,
      mcmc_steps  = as.integer(mcmc_steps),
      mcmc_sample = as.integer(mcmc_sample),
      mcmc_burnin = as.integer(mcmc_burnin),
      mcmc_runs   = as.integer(mcmc_runs),
      seed        = as.integer(raw$seed)
    ),
    class = c("mptp_mcmc", "mptp_ml")
  )
}


# ── Public API ────────────────────────────────────────────────────────────────

#' Run mPTP species delimitation (Maximum Likelihood)
#'
#' Calls the mPTP C engine directly via \code{.Call()} — no external process,
#' no file parsing. Results are returned as a structured R object.
#'
#' @param tree A path to a Newick tree file (character) or an
#'   \code{\link[ape]{read.tree}} \code{phylo} object.  Rooted and unrooted
#'   trees are both accepted.
#' @param method Coalescent rate model: \code{"multi"} (default, recommended)
#'   or \code{"single"}.
#' @param outgroup Character. Name of the outgroup taxon used for rooting an
#'   unrooted tree (optional; if omitted, the longest tip branch is used).
#' @param outgroup_crop Logical. If \code{TRUE}, removes the outgroup after
#'   rooting (default \code{FALSE}).
#' @param pvalue Numeric. Significance threshold for the likelihood ratio test
#'   (default \code{0.001}).
#' @param minbr Numeric. Minimum branch length; edges shorter than this are
#'   excluded from the analysis (default \code{0.0001}).
#' @param seed Integer. Random seed (default: current time).
#' @param quiet Logical. Suppress C-level progress messages (default
#'   \code{FALSE}).
#'
#' @return An object of class \code{"mptp_ml"}, a list with:
#'   \describe{
#'     \item{\code{n_species}}{Integer. Number of delimited species.}
#'     \item{\code{method}}{Character. \code{"multi"} or \code{"single"}.}
#'     \item{\code{null_logl}}{Numeric. Null-model log-likelihood.}
#'     \item{\code{best_logl}}{Numeric. Best log-likelihood under mPTP.}
#'     \item{\code{pvalue}}{Numeric. LRT p-value.}
#'     \item{\code{lrt_passed}}{Logical. Whether the LRT rejected the null.}
#'     \item{\code{edge_count}}{Integer. Edges longer than \code{minbr}.}
#'     \item{\code{total_edges}}{Integer. Total edges in the tree.}
#'     \item{\code{assignments}}{Data frame with columns \code{labels} and
#'       \code{mptp} (integer index; \code{ptp} when \code{method = "single"}).}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- mptp("tree.nwk", outgroup = "Outgroup1",
#'                        outgroup_crop = TRUE)
#' print(result)
#' head(result$assignments)
#'
#' library(ape)
#' phy <- read.tree("tree.nwk")
#' result <- mptp(phy)
#' }
#'
#' @seealso \code{\link{mptp_mcmc}}, \code{\link{mptp_tbl}}
#' @export
mptp <- function(tree,
                        method        = c("multi", "single"),
                        outgroup      = NULL,
                        outgroup_crop = FALSE,
                        pvalue        = 0.001,
                        minbr         = 0.0001,
                        seed          = as.integer(Sys.time()),
                        quiet         = FALSE) {

  method    <- match.arg(method)
  tmpfiles  <- new.env(parent = emptyenv())
  on.exit(if (!is.null(tmpfiles$path)) unlink(tmpfiles$path), add = TRUE)
  tree_file <- .resolve_tree_input(tree, tmpfiles)
  outfile   <- tempfile(pattern = "mptp_ml_")
  method_id <- if (method == "multi") 1L else 0L

  raw <- .mptp_call_ml(
    tree_file = tree_file,
    outfile   = outfile,
    outgroup  = outgroup,
    method    = method_id,
    minbr     = minbr,
    pvalue    = pvalue,
    quiet     = as.integer(quiet),
    seed      = as.double(seed),
    crop      = as.integer(outgroup_crop)
  )

  result      <- .build_mptp_ml(raw, method)
  result$call <- match.call()
  result
}


#' Run mPTP species delimitation (MCMC)
#'
#' Calls the mPTP MCMC engine directly via \code{.Call()}. Returns ML species
#' assignments plus per-node posterior support values.
#'
#' @param tree A path to a Newick tree file (character) or a \code{phylo}
#'   object from \pkg{ape}.  See \code{\link{mptp}} for details.
#' @param method Coalescent rate model: \code{"multi"} (default) or
#'   \code{"single"}.
#' @param mcmc_steps Integer. Total MCMC steps per run (default 1,000,000).
#' @param mcmc_sample Integer. Sampling frequency (default 1,000).
#' @param mcmc_burnin Integer. Steps discarded as burn-in (default 100,000).
#' @param mcmc_runs Integer. Number of independent runs (default 1).
#' @param mcmc_credible Numeric. Credible interval (default 0.95).
#' @param mcmc_startnull Logical. Start from the null (one-species) model.
#' @param mcmc_startrandom Logical. Start from a random delimitation.
#' @param mcmc_startml Logical. Start from the ML delimitation.
#' @param outgroup Character. Outgroup taxon name for rooting (optional).
#' @param outgroup_crop Logical. Remove outgroup after rooting (default
#'   \code{FALSE}).
#' @param pvalue Numeric. LRT significance threshold (default 0.001).
#' @param minbr Numeric. Minimum branch length threshold (default 0.0001).
#' @param seed Integer. Random seed (default: current time).
#' @param quiet Logical. Suppress C-level progress messages (default
#'   \code{FALSE}).
#'
#' @return An object of classes \code{c("mptp_mcmc", "mptp_ml")}, a list with
#'   all fields of \code{"mptp_ml"} plus:
#'   \describe{
#'     \item{\code{support}}{Data frame with columns \code{node_label} and
#'       \code{support} (0–1); posterior probability of each inner node being
#'       a coalescent root.}
#'     \item{\code{mcmc_steps}}{Integer.}
#'     \item{\code{mcmc_sample}}{Integer.}
#'     \item{\code{mcmc_burnin}}{Integer.}
#'     \item{\code{mcmc_runs}}{Integer.}
#'     \item{\code{seed}}{Integer. Actual seed used.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- mptp_mcmc(
#'   "tree.nwk",
#'   mcmc_steps    = 5000000L,
#'   mcmc_burnin   = 500000L,
#'   mcmc_runs     = 3L,
#'   seed          = 42L,
#'   outgroup      = "Outgroup1",
#'   outgroup_crop = TRUE
#' )
#' print(result)
#' head(result$support)
#' }
#'
#' @seealso \code{\link{mptp}}, \code{\link{mptp_tbl}}
#' @export
mptp_mcmc <- function(tree,
                          method           = c("multi", "single"),
                          mcmc_steps       = 1000000L,
                          mcmc_sample      = 1000L,
                          mcmc_burnin      = 100000L,
                          mcmc_runs        = 1L,
                          mcmc_credible    = 0.95,
                          mcmc_startnull   = FALSE,
                          mcmc_startrandom = FALSE,
                          mcmc_startml     = FALSE,
                          outgroup         = NULL,
                          outgroup_crop    = FALSE,
                          pvalue           = 0.001,
                          minbr            = 0.0001,
                          seed             = as.integer(Sys.time()),
                          quiet            = FALSE) {

  method   <- match.arg(method)
  tmpfiles <- new.env(parent = emptyenv())
  on.exit(if (!is.null(tmpfiles$path)) unlink(tmpfiles$path), add = TRUE)
  tree_file <- .resolve_tree_input(tree, tmpfiles)
  outfile   <- tempfile(pattern = "mptp_mcmc_")
  method_id <- if (method == "multi") 1L else 0L

  raw <- .mptp_call_mcmc(
    tree_file        = tree_file,
    outfile          = outfile,
    outgroup         = outgroup,
    method           = method_id,
    minbr            = minbr,
    pvalue           = pvalue,
    quiet            = as.integer(quiet),
    seed             = as.double(seed),
    crop             = as.integer(outgroup_crop),
    mcmc_steps       = mcmc_steps,
    mcmc_sample      = as.integer(mcmc_sample),
    mcmc_burnin      = mcmc_burnin,
    mcmc_runs        = as.integer(mcmc_runs),
    mcmc_credible    = mcmc_credible,
    mcmc_startnull   = as.integer(mcmc_startnull),
    mcmc_startrandom = as.integer(mcmc_startrandom),
    mcmc_startml     = as.integer(mcmc_startml)
  )

  result      <- .build_mptp_mcmc(raw, method,
                                   mcmc_steps, mcmc_sample, mcmc_burnin,
                                   mcmc_runs, seed)
  result$call <- match.call()
  result
}


# ── Accessor helpers ──────────────────────────────────────────────────────────

#' Extract MCMC support values
#'
#' @param x An object of class \code{"mptp_mcmc"}.
#' @return A data frame with columns \code{node_label} and \code{support}.
#' @export
mptp_support <- function(x) {
  if (!inherits(x, "mptp_mcmc"))
    stop("x must be of class 'mptp_mcmc' (mptp_mcmc result).")
  x$support
}


# ── Summary methods ───────────────────────────────────────────────────────────

#' @export
summary.mptp_ml <- function(object, ...) {
  cat("mPTP Maximum Likelihood Delimitation\n")
  cat("--------------------------------------\n")
  cat(sprintf("  Method        : %s coalescent rate\n", object$method))
  cat(sprintf("  N species     : %d\n",      object$n_species))
  cat(sprintf("  Null log-L    : %.6f\n",    object$null_logl))
  cat(sprintf("  Best log-L    : %.6f\n",    object$best_logl))
  cat(sprintf("  LRT p-value   : %.6f\n",    object$pvalue))
  cat(sprintf("  LRT result    : %s\n",
              if (isTRUE(object$lrt_passed)) "passed" else "failed"))
  cat(sprintf("  Edges > minbr : %d / %d\n", object$edge_count, object$total_edges))
  invisible(object)
}

#' @export
summary.mptp_mcmc <- function(object, ...) {
  cat("mPTP MCMC Delimitation\n")
  cat("-----------------------\n")
  cat(sprintf("  Method        : %s coalescent rate\n", object$method))
  cat(sprintf("  MCMC steps    : %d\n",   object$mcmc_steps))
  cat(sprintf("  Burn-in       : %d\n",   object$mcmc_burnin))
  cat(sprintf("  Runs          : %d\n",   object$mcmc_runs))
  cat(sprintf("  Seed          : %d\n",   object$seed))
  cat(sprintf("  N species     : %d\n",   object$n_species))
  cat(sprintf("  Null log-L    : %.6f\n", object$null_logl))
  cat(sprintf("  Best log-L    : %.6f\n", object$best_logl))
  cat(sprintf("  LRT p-value   : %.6f\n", object$pvalue))
  cat(sprintf("  LRT result    : %s\n",
              if (isTRUE(object$lrt_passed)) "passed" else "failed"))
  cat(sprintf("  Edges > minbr : %d / %d\n", object$edge_count, object$total_edges))
  cat(sprintf("  Support nodes : %d inner nodes\n", nrow(object$support)))
  invisible(object)
}
