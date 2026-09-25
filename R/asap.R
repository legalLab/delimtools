# asap.R
# R wrapper for the original ASAP algorithm (Puillandre, Brouillet & Achaz 2021)
# Default parameters mirror those of asap.c main():
#   slope_weight     = 0.1   (line 688, orig: pond_pente)
#   score_weight     = 0.5   (line 689, orig: pond_score)
#   replicates       = 1000  (line 690)
#   pvalue_threshold = 0.001 (line 691, orig: seuil_pvalue)
#   len_seq      = 600   (line 694, overridden if DNAbin is provided)
#   modelo       = Simple_Dist (index 3 in meth[], line 645)


# =============================================================================
# Simple_Dist -- faithful to distancesimple() in oldfns.c
# d(a,b) = v / (L - ncor)   [no Laplace correction, as in the original]
# Executed in C via .Call() for performance equivalent to the original
# =============================================================================

#' @keywords internal
.dist_simple <- function(dnabin) {
  mat_char <- toupper(as.character(as.matrix(dnabin)))
  taxa     <- rownames(mat_char)
  nseq     <- nrow(mat_char)
  L        <- ncol(mat_char)

  # Flat vector of ASCII codes (row-major: sequence a × position i)
  seq_ints <- utf8ToInt(
    paste(apply(mat_char, 1L, paste, collapse = ""), collapse = "")
  )

  # Call C: returns npairs = nseq*(nseq-1)/2 distances
  dist_vec <- .Call(asap_dist_call,
                    as.integer(seq_ints),
                    as.integer(nseq),
                    as.integer(L))

  # Rebuild symmetric matrix (order a>b, a*(a-1)/2+b zero-based)
  d_mat <- matrix(0.0, nseq, nseq, dimnames = list(taxa, taxa))
  k <- 1L
  for (a in seq_len(nseq)) {
    for (b in seq_len(a - 1L)) {
      d_mat[a, b] <- d_mat[b, a] <- dist_vec[k]
      k <- k + 1L
    }
  }
  d_mat
}


# =============================================================================
# Converts symmetric matrix to lower-triangular vector
# order: a=1..n, b=0..(a-1) -> a*(a-1)/2 + b  (zero-based, same as C)
# =============================================================================

#' @keywords internal
.mat_to_lower_vec <- function(mat) {
  n   <- nrow(mat)
  vec <- numeric(n * (n - 1L) / 2L)
  k   <- 1L
  for (a in seq_len(n)) {
    for (b in seq_len(a - 1L)) {
      vec[k] <- mat[a, b]
      k <- k + 1L
    }
  }
  vec
}


# =============================================================================
# .read_aligned_dna -- shared input validation for asap() and abgd()
# Accepts a path to an aligned FASTA file or an ape::DNAbin matrix.
# Distance matrices are deliberately rejected.
# =============================================================================

#' @keywords internal
.read_aligned_dna <- function(x, arg = "x") {
  if (!requireNamespace("ape", quietly = TRUE))
    stop("Package 'ape' is required.")

  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    if (!file.exists(x))
      stop("File not found: '", x, "'")
    x <- tryCatch(
      ape::read.dna(x, format = "fasta"),
      error = function(e)
        stop("'", arg, "' could not be read as a FASTA file: ",
             conditionMessage(e), call. = FALSE)
    )
  } else if (!inherits(x, "DNAbin")) {
    stop("'", arg, "' must be a path to an aligned FASTA file or a ",
         "DNAbin object; distance matrices are not accepted.", call. = FALSE)
  }

  if (!is.matrix(x))
    stop("Sequences in '", arg, "' are not aligned (differing lengths).",
         call. = FALSE)
  if (is.null(rownames(x)))
    rownames(x) <- paste0("seq", seq_len(nrow(x)))
  x
}


# =============================================================================
# asap() -- main function
# =============================================================================

#' ASAP -- Assemble Species by Automatic Partitions
#'
#' Delimits species from aligned sequences using the
#' algorithm of Puillandre, Brouillet & Achaz (2021). The original C code
#' (asap_core.c, asap_common.c) is executed without modifications; only the
#' Python/graphical dependencies are replaced.
#'
#' @param x Path to an aligned FASTA file, or an aligned \code{DNAbin} (ape)
#'   object. Distance matrices are not accepted; distances are calculated
#'   internally from the sequences.
#' @param model Distance model.
#'   \code{"simple"} (default) uses Simple_Dist, identical to the ASAP
#'   original default. Any model from \code{ape::dist.dna()} is also accepted.
#' @param len_seq Sequence length for the coalescent simulations. Inferred
#'   from the alignment length when \code{NULL} (default).
#' @param replicates Coalescent replicates. Default: \code{1000}.
#' @param pvalue_threshold P-value threshold. Default: \code{0.001}.
#' @param slope_weight Slope window weight. Default: \code{0.1}.
#' @param score_weight Weight of the p-value rank in the ASAP-score. Default: \code{0.5}.
#' @param pairwise.deletion Passed to \code{ape::dist.dna()}. Default: \code{TRUE}.
#'
#' @return A list of class \code{"asap"} with:
#' \describe{
#'   \item{\code{partitions}}{data.frame sorted by ascending ASAP-score.}
#'   \item{\code{best}}{List with the best partition (rank 1).}
#'   \item{\code{dist_matrix}}{Distance matrix used.}
#'   \item{\code{taxa}}{Sequence names.}
#' }
#'
#' @author
#' Pedro S. Bittencourt
#' 
#' @section AI Disclaimer: 
#' 
#' This function was written with assistance of AI coding agent (Claude Code Sonnet 4.6).
#' Correctness was validated by comparing against original software, on a suite 
#' of real sequencing datasets, and manual code review. All validation and output
#' 
#' @section Source Code:
#' N. Puillandre,  A. Lambert,  S. Brouillet,  G. Achaz (ASAP C engine)
#'
#' @references
#' Puillandre N, Brouillet S, Achaz G (2021). ASAP: assemble species by
#' automatic partitions. \emph{Molecular Ecology Resources}, 21(2),
#' 609-620. \doi{10.1111/1755-0998.13281}
#'
#' @examples
#' \dontrun{
#' library(ape)
#' seqs   <- read.dna("barcode.fasta", format = "fasta")
#' result <- asap(seqs)
#' result <- asap("barcode.fasta")   # aligned FASTA path also accepted
#' print(result)
#' bp <- best_partition(result)
#' bp$n_groups   # number of species
#' bp$partition  # assignment for each sequence
#' }
#'
#' @export
asap <- function(x,
                 model             = "simple",
                 len_seq           = NULL,
                 replicates        = 1000L,
                 pvalue_threshold  = 0.001,
                 slope_weight      = 0.1,
                 score_weight      = 0.5,
                 pairwise.deletion = TRUE) {

  # --- 1. Read/validate sequences and compute distance matrix ---
  x <- .read_aligned_dna(x)
  if (is.null(len_seq)) len_seq <- ncol(x)
  if (model == "simple") {
    mat <- .dist_simple(x)
  } else {
    mat <- as.matrix(
      ape::dist.dna(x, model = model, pairwise.deletion = pairwise.deletion)
    )
  }

  mat[!is.finite(mat)] <- 0
  n <- nrow(mat)
  if (n < 3L) stop("ASAP requires at least 3 sequences.")

  taxa <- rownames(mat)
  if (is.null(taxa)) taxa <- paste0("seq", seq_len(n))

  if (is.null(len_seq) || len_seq <= 0L) len_seq <- 600L

  # --- 2. Lower-triangular vector ---
  dist_vec <- .mat_to_lower_vec(mat)

  # --- 3. Call the original C core ---
  raw <- .Call(asap_run_call,
               as.double(dist_vec),
               as.integer(n),
               as.integer(len_seq),
               as.integer(replicates),
               as.double(pvalue_threshold),
               as.double(slope_weight),
               as.double(score_weight))

  if (is.null(raw) || length(raw[[1L]]) == 0L) {
    warning("ASAP found no partitions.")
    return(structure(
      list(partitions = data.frame(), best = NULL,
           dist_matrix = mat, taxa = taxa),
      class = "asap"
    ))
  }

  # --- 4. Build data.frame (already sorted by rank_general in C) ---
  parts <- data.frame(
    rank       = as.integer(raw$rank),
    nbgroups   = as.integer(raw$nbgroups),
    nbspecRec  = as.integer(raw$nbspecRec),
    dist       = raw$dist,
    d_jump     = raw$d_jump,
    proba      = raw$proba,
    intra      = raw$intra,
    inter      = raw$inter,
    slope      = raw$slope,
    asap_score = raw$asap_score,
    stringsAsFactors = FALSE
  )

  # --- 5. Best partition (rank 1) ---
  best <- .build_best(parts, mat, taxa)

  structure(
    list(partitions  = parts,
         best        = best,
         dist_matrix = mat,
         taxa        = taxa),
    class = "asap"
  )
}


# =============================================================================
# .build_best
# =============================================================================

#' @keywords internal
.build_best <- function(parts, mat, taxa) {
  if (nrow(parts) == 0L) return(NULL)
  row <- parts[1L, ]
  partition <- .build_partition(mat, row$d_jump)
  names(partition) <- taxa
  list(
    n_groups   = row$nbgroups,
    asap_score = row$asap_score,
    proba      = row$proba,
    dist       = row$dist,
    d_jump     = row$d_jump,
    intra      = row$intra,
    inter      = row$inter,
    slope      = row$slope,
    partition  = partition
  )
}


# =============================================================================
# .build_partition -- Union-Find with threshold = d_jump
# =============================================================================

#' @keywords internal
.build_partition <- function(mat, threshold) {
  n      <- nrow(mat)
  parent <- seq_len(n)

  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }

  for (i in seq_len(n)) {
    for (j in seq_len(i - 1L)) {
      if (mat[i, j] < threshold) {
        ri <- find(i); rj <- find(j)
        if (ri != rj) parent[ri] <- rj
      }
    }
  }

  roots   <- vapply(seq_len(n), find, integer(1L))
  uid     <- unique(roots)
  mapping <- setNames(seq_along(uid), uid)
  unname(mapping[as.character(roots)])
}


# =============================================================================
# S3 methods
# =============================================================================

#' @export
print.asap <- function(x, n_best = 10L, ...) {
  cat("=== ASAP -- Assemble Species by Automatic Partitions ===\n\n")
  cat(sprintf("Sequences  : %d\n", nrow(x$dist_matrix)))
  cat(sprintf("Partitions : %d (showing top %d by ASAP-score)\n\n",
              nrow(x$partitions), min(n_best, nrow(x$partitions))))

  if (nrow(x$partitions) == 0L) {
    cat("No partitions found.\n")
    return(invisible(x))
  }

  cat(sprintf("%-5s  %-8s  %-11s  %-10s  %-10s  %-10s  %s\n",
              "Rank", "Groups", "Dist.", "p-value",
              "pi_intra", "pi_inter", "ASAP-score"))
  cat(strrep("-", 74), "\n")

  nb <- min(nrow(x$partitions), as.integer(n_best))
  for (k in seq_len(nb)) {
    r  <- x$partitions[k, ]
    mk <- if (k == 1L) " *" else ""
    cat(sprintf("%-5d  %-8d  %-11.6f  %-10.4e  %-10.6f  %-10.6f  %.1f%s\n",
                r$rank, r$nbgroups, r$dist, r$proba,
                r$intra, r$inter, r$asap_score, mk))
  }

  cat("\n* = best partition (lowest ASAP-score)\n")
  if (!is.null(x$best))
    cat(sprintf("\nBest: %d groups  |  ASAP-score = %.1f  |  p = %.4e\n",
                x$best$n_groups, x$best$asap_score, x$best$proba))
  invisible(x)
}

#' @export
summary.asap <- function(object, ...) print(object, ...)


# =============================================================================
# best_partition
# =============================================================================

#' @rdname best_partition
#' @param rank Integer. Rank of the partition to return (1 = best ASAP score).
#'   If \code{NULL} (default), returns rank 1.
#' @export
best_partition.asap <- function(x, rank = NULL, ...) {
  if (!inherits(x, "asap"))
    stop("'x' must be an object returned by asap().")

  parts <- x$partitions
  if (nrow(parts) == 0L) stop("No partitions available.")

  if (!is.null(rank)) {
    rank <- as.integer(rank)
    if (rank < 1L || rank > nrow(parts))
      stop(sprintf("'rank' must be between 1 and %d.", nrow(parts)))
    row <- parts[rank, ]
  } else {
    row <- parts[1L, ]
  }

  partition <- .build_partition(x$dist_matrix, row$d_jump)
  names(partition) <- x$taxa

  list(
    n_groups   = row$nbgroups,
    asap_score = row$asap_score,
    proba      = row$proba,
    dist       = row$dist,
    d_jump     = row$d_jump,
    intra      = row$intra,
    inter      = row$inter,
    slope      = row$slope,
    partition  = partition
  )
}
