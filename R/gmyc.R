#' Run the GMYC Species Delimitation Analysis
#'
#' @param tree A \code{"phylo"} object (\pkg{ape}).
#' @param method Analysis method: only \code{"single"} is implemented.
#' @param interval Optimisation interval for the null model scaling exponent
#'   (used only for the pure-R fallback; C engine always searches \code{[0, 5]}).
#' @param quiet Suppress per-threshold progress output (default \code{TRUE}).
#' @param tol Ultrametricity tolerance (default \code{1e-6}).
#' @return A named list of class \code{"gmyc"}, with the same structure as
#'   \code{splits::gmyc()}: \code{method}, \code{likelihood} (per-threshold
#'   vector), \code{parameters}, \code{entity}, \code{cluster},
#'   \code{MRCA}, \code{threshold.time}, and \code{tree}.
#' @references Fujisawa & Barraclough (2013). \emph{Syst. Biol.} 62(5), 707--724.
#' @export
gmyc <- function(tree, method = "single", interval = c(0, 5),
                 quiet = TRUE, tol = 1e-6) {
  if (!inherits(tree, "phylo"))
    stop("'tree' must be an object of class 'phylo' (ape package).")
  if (!ape::is.binary(tree))
    stop("Tree must be fully binary. Use ape::multi2di() to resolve polytomies.")
  if (!ape::is.ultrametric(tree, tol = tol))
    stop("Tree must be ultrametric.")
  if (is.null(tree$edge.length))
    stop("Tree has no branch lengths.")
  n_tips <- length(tree$tip.label)
  if (n_tips < 3) stop("GMYC requires at least 3 taxa.")

  bt <- -ape::branching.times(tree)
  bt[bt > -0.000001] <- -0.000001
  # Order by APE node number so C can index directly: bt_ordered[j] → C node n_tips+j-1
  bt_ordered <- bt[order(as.integer(names(bt)))]

  n_internal <- length(bt)
  n_nodes    <- n_tips + n_internal

  # Precompute null model + per-threshold state in C
  setup <- .Call(C_gmyc_setup,
                 tree$edge, tree$edge.length, tree$tip.label,
                 n_tips, n_nodes, bt_ordered)

  m          <- attr(setup, "n_thresh")
  stthresh_c <- attr(setup, "stthresh_c")   # 0-based first valid threshold
  stthresh_r <- stthresh_c + 1L             # 1-based equivalent for R indexing
  null_ll    <- attr(setup, "null_ll")
  null_p     <- attr(setup, "null_p")
  null_lam   <- attr(setup, "null_lam")
  sb         <- attr(setup, "ev_times")     # sorted event times, oldest first
  thresh_K   <- attr(setup, "thresh_K")     # K per threshold (R 1-based indexing)

  # Result arrays
  likelihood  <- rep(NA_real_,    m)
  cluster_vec <- rep(NA_integer_, m)
  params      <- matrix(NA_real_, nrow = m, ncol = 4L)
  colnames(params) <- c("lambda.div", "lambda.coal", "p.div", "p.coal")

  likelihood[[1L]]  <- null_ll
  cluster_vec[[1L]] <- 1L
  params[1L, 1L]    <- null_lam
  params[1L, 3L]    <- null_p

  # Threshold scan: R's optim() drives Nelder-Mead, C evaluates the loglik.
  # Warm-starting carries each threshold's solution to the next (faster convergence).
  prev_pq <- c(1, 1)
  for (j in stthresh_r:m) {
    T_idx <- j - 1L   # 0-based index for C

    ll_fn <- function(pq) {
      .Call(C_gmyc_loglik, setup, T_idx, as.double(pq))
    }

    opt     <- optim(prev_pq, ll_fn, control = list(fnscale = -1))
    prev_pq <- opt$par

    res_p <- .Call(C_gmyc_loglik_params, setup, T_idx, as.double(opt$par))

    likelihood[[j]]  <- res_p[[1L]]
    params[j, 1L]    <- res_p[[2L]]   # lambda.div  (speciation)
    params[j, 2L]    <- res_p[[3L]]   # lambda.coal (coalescent)
    params[j, 3L]    <- opt$par[[1L]] # p.div  = spe_p
    params[j, 4L]    <- opt$par[[2L]] # p.coal = coa_p
    cluster_vec[[j]] <- thresh_K[[j]]
  }

  # entity vector
  entity_vec <- seq_len(m)
  if (stthresh_r > 2L)
    entity_vec[2L:(stthresh_r - 1L)] <- NA_integer_

  # MRCA list (APE node offsets, used by spec.list)
  int_names <- names(bt)
  int_ape   <- as.integer(int_names)

  parent_of <- vapply(int_names, function(nm) {
    p <- tree$edge[tree$edge[, 2L] == as.integer(nm), 1L]
    if (length(p) == 0L) NA_integer_ else as.integer(p)
  }, integer(1L))

  parent_ht <- vapply(int_names, function(nm) {
    p <- parent_of[[nm]]
    if (is.na(p)) -Inf else bt[[as.character(p)]]
  }, numeric(1L))

  MRCA_list <- vector("list", m)
  for (j in stthresh_r:m) {
    thresh   <- sb[[j]]
    is_mrca  <- (bt >= thresh) & (parent_ht < thresh)
    mrca_ape <- sort(int_ape[is_mrca])
    MRCA_list[[j]] <- if (length(mrca_ape) > 0L) mrca_ape - n_tips else integer(0L)
  }

  result <- list(
    method         = "single",
    likelihood     = likelihood,
    parameters     = params,
    entity         = entity_vec,
    cluster        = cluster_vec,
    MRCA           = MRCA_list,
    threshold.time = sb,
    tree           = tree
  )
  class(result) <- "gmyc"
  result
}

#' @export
print.gmyc <- function(x, ...) {
  best_j <- which.max(x$likelihood)
  LR     <- 2 * (x$likelihood[[best_j]] - x$likelihood[[1L]])
  pval   <- 1 - pchisq(LR, df = 2)
  cat("GMYC species delimitation result\n")
  cat("  Method:    ", x$method, "\n")
  cat("  Threshold: ", format(x$threshold.time[[best_j]], digits = 6), "\n")
  cat("  Clusters:  ", x$cluster[[best_j]], "\n")
  cat("  p-value:   ", format(pval, digits = 4), sig_stars(pval), "\n")
  invisible(x)
}

#' @export
summary.gmyc <- function(object, second.peak = FALSE, ...) {
  if (second.peak) {
    tmp       <- table(cummax(object$likelihood))
    lik.peaks <- names(tmp[tmp > 20])
    peak      <- which(object$likelihood ==
                         lik.peaks[(length(lik.peaks) - 1L)])
  }

  cat("Result of GMYC species delimitation\n")
  cat("\n\tmethod:\t",                    object[["method"]], sep = "")
  cat("\n\tlikelihood of null model:\t",  object$likelihood[[1L]], sep = "")

  if (!second.peak) {
    cat("\n\tmaximum likelihood of GMYC model:\t", max(object$likelihood),
        sep = "")
  } else {
    cat("\n\tmaximum likelihood of GMYC model:\t", object$likelihood[[peak]],
        sep = "")
  }

  LR <- if (!second.peak)
    2 * (max(object$likelihood) - object$likelihood[[1L]])
  else
    2 * (object$likelihood[[peak]] - object$likelihood[[1L]])

  cat("\n\tlikelihood ratio:\t", LR, sep = "")

  pvalue <- 1 - pchisq(LR, df = 2)
  cat("\n\tresult of LR test:\t", pvalue,
      ifelse(pvalue < 0.001, "***",
             ifelse(pvalue < 0.01,  "**",
                    ifelse(pvalue < 0.05, "*", "n.s."))),
      sep = "")

  if (!second.peak) {
    best_j <- which.max(object$likelihood)
    cat("\n\n\tnumber of ML clusters:\t",
        object$cluster[[best_j]], sep = "")
    tmp_c <- object$cluster[object$likelihood >
                              (max(object$likelihood) - 2)]
    cat("\n\tconfidence interval:\t",
        paste(min(tmp_c, na.rm = TRUE), max(tmp_c, na.rm = TRUE), sep = "-"),
        sep = "")
    cat("\n\n\tnumber of ML entities:\t",
        object$entity[[best_j]], sep = "")
    tmp_e <- object$entity[object$likelihood >
                             (max(object$likelihood) - 2)]
    cat("\n\tconfidence interval:\t",
        paste(min(tmp_e, na.rm = TRUE), max(tmp_e, na.rm = TRUE), sep = "-"),
        sep = "")
    cat("\n\n\tthreshold time:\t",
        object$threshold.time[[best_j]], "\n", sep = "")
  } else {
    cat("\n\n\tnumber of ML clusters:\t", object$cluster[[peak]], sep = "")
    cat("\n\tnumber of ML entities:\t", object$entity[[peak]], sep = "")
    cat("\n\tthreshold time:\t", object$threshold.time[[peak]], "\n", sep = "")
  }
  cat("\n")
  invisible(object)
}

#' Get species list from a delimitation result
#'
#' Returns a character vector of tip labels, grouped by inferred species.
#'
#' @param x A fitted delimitation object (e.g. \code{gmyc}).
#' @param ... Additional arguments passed to methods.
#' @return A named character vector mapping tips to species identifiers.
#' @export
spec.list <- function(x, ...) UseMethod("spec.list")

#' @rdname spec.list
#' @param second.peak Logical. If \code{TRUE}, use the second likelihood peak
#'   instead of the global maximum. Default: \code{FALSE}.
#' @export
spec.list.gmyc <- function(x, second.peak = FALSE, ...) {
  tr     <- x$tree
  numtip <- length(tr$tip.label)
  spec   <- tr$tip.label

  if (!second.peak) {
    max.mrca <- x$MRCA[[which.max(x$likelihood)]] + numtip
  } else {
    tmp       <- table(cummax(x$likelihood))
    lik.peaks <- names(tmp[tmp > 20])
    peak      <- which(x$likelihood == lik.peaks[(length(lik.peaks) - 1L)])
    max.mrca  <- x$MRCA[[peak]] + numtip
  }

  nest.tip <- function(nod) {
    tip   <- c()
    child <- tr$edge[tr$edge[, 1L] == nod, 2L]
    for (ch in child) {
      if (ch <= numtip) tip <- c(tip, ch)
      else              tip <- c(tip, nest.tip(ch))
    }
    tip
  }

  res <- NULL
  for (i in seq_along(max.mrca)) {
    tip.name <- tr$tip.label[nest.tip(max.mrca[i])]
    res <- rbind(res, cbind(i, tip.name))
  }

  singletons <- spec[-match(res[, 2L], spec)]
  if (length(singletons) != 0L) {
    numspec <- length(max.mrca) + 1L
    for (s in singletons) {
      res     <- rbind(res, cbind(numspec, s))
      numspec <- numspec + 1L
    }
  }

  res              <- data.frame(res, stringsAsFactors = FALSE)
  colnames(res)    <- c("GMYC_spec", "sample_name")
  res$GMYC_spec    <- as.numeric(as.character(res$GMYC_spec))
  res
}

sig_stars <- function(p) {
  if      (p < 0.001) "***"
  else if (p < 0.01)  "**"
  else if (p < 0.05)  "*"
  else if (p < 0.1)   "."
  else                ""
}
