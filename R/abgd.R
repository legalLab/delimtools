# abgd.R -- R interface for the original ABGD by G. Achaz
# All calculations are performed by the original C code (abgdCore.c + main_abgd.c).
# This file organizes inputs/outputs and exposes the API to the user.


# =============================================================================
# MAIN FUNCTION: abgd()
# =============================================================================

#' Automatic Barcode Gap Discovery (ABGD)
#'
#' Delimits species using the original algorithm of Puillandre et al. (2012).
#' All computation (distances, gap detection, partitioning and recursion) is
#' performed by the original C code of G. Achaz via \code{.Call()}.
#'
#' @param file Path to an aligned FASTA file, a Phylip distance matrix, or a
#'   \code{DNAbin} object (ape). If a \code{DNAbin} object is provided,
#'   sequences are exported to a temporary file and processed by the original
#'   C code without intermediate conversion.
#' @param model Distance model:
#'   \describe{
#'     \item{\code{"simple"}}{p-distance with Laplace correction --
#'       equivalent to \code{-d 3} in the original ABGD.
#'       \strong{Use this to replicate \code{abgd -a -d 3}.}}
#'     \item{\code{"JC69"}}{Jukes-Cantor (original default, \code{-d 1}).}
#'     \item{\code{"K80"}}{Kimura 2-parameter (\code{-d 0}).}
#'     \item{\code{"TN93"}}{Tamura-Nei (\code{-d 2}, *not implemented*).}
#'   }
#' @param prior_min Minimum prior for intraspecific divergence (\code{-p}).
#'   Default: \code{0.001}.
#' @param prior_max Maximum prior for intraspecific divergence (\code{-P}).
#'   Default: \code{0.1}.
#' @param prior_steps Number of steps in \eqn{[p,P]} (\code{-n}).
#'   Default: \code{10}.
#' @param min_slope_increase Minimum slope increase factor (\code{-X}).
#'   Default: \code{1.5}.
#' @param ts_tv Transition/transversion ratio for K80 (\code{-t}).
#'   Default: \code{2.0}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{initial}}{A data.frame with one row per prior (initial
#'     partition): \code{prior}, \code{n_groups}, \code{partition}.}
#'   \item{\code{recursive}}{A data.frame with one row per prior (recursive
#'     partition): \code{prior}, \code{n_groups}, \code{partition}.}
#'   \item{\code{dist_matrix}}{Numeric distance matrix (nseq x nseq).}
#'   \item{\code{taxa}}{Character vector of sequence names.}
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
#' verification was made by the authors.
#' 
#' @section Source Code:
#' N. Puillandre,  A. Lambert,  S. Brouillet,  G. Achaz (ABGD C engine)
#'
#' @references 
#' Puillandre N, Lambert A, Brouillet S, Achaz G (2012). ABGD, Automatic
#' Barcode Gap Discovery for primary species delimitation.
#' \emph{Molecular Ecology}, 21(8), 1864--1877.
#' \doi{10.1111/j.1365-294X.2011.05239.x}
#'
#' @examples
#' \dontrun{
#' # Equivalent to: abgd -a -d 3 geophagus.fasta
#' result <- abgd("geophagus.fasta", model = "simple")
#'
#' result$initial[, c("prior", "n_groups")]
#' result$recursive[, c("prior", "n_groups")]
#' result$recursive$partition[[1]]   # partition for the first prior
#'
#' # Input from a DNAbin object
#' library(ape)
#' seqs   <- read.dna("geophagus.fasta", format = "fasta")
#' result <- abgd(seqs, model = "simple")
#' }
#'
#' @export
abgd <- function(file,
                 model              = "simple",
                 prior_min          = 0.001,
                 prior_max          = 0.1,
                 prior_steps        = 10L,
                 min_slope_increase = 1.5,
                 ts_tv              = 2.0) {

  # Accepts DNAbin -- writes to a temporary file and uses the original C code
  if (inherits(file, "DNAbin")) {
    if (!requireNamespace("ape", quietly = TRUE))
      stop("Package 'ape' is required for DNAbin input.")
    tmp <- tempfile(fileext = ".fasta")
    ape::write.dna(file, tmp, format = "fasta", colsep = "")
    on.exit(unlink(tmp), add = TRUE)
    file <- tmp
  }

  if (!file.exists(file))
    stop("File not found: '", file, "'")

  method_int <- switch(model,
    "K80"    = 0L,
    "JC69"   = 1L,
    "TN93"   = 2L,
    "simple" = 3L,
    stop("model must be 'K80', 'JC69', 'TN93' or 'simple'.")
  )

  raw <- .Call(abgd_run_call,
               normalizePath(file),
               as.double(prior_min),
               as.double(prior_max),
               as.integer(prior_steps),
               as.integer(method_int),
               as.double(min_slope_increase),
               as.double(ts_tv))

  taxa   <- raw$names
  np     <- length(raw$priors)

  # Add sequence names to partitions
  parts_init <- lapply(raw$initial,  function(p) { names(p) <- taxa; p })
  parts_rec  <- lapply(raw$recursive, function(p) { names(p) <- taxa; p })

  df_init <- data.frame(
    prior    = raw$priors,
    n_groups = raw$n_init,
    stringsAsFactors = FALSE
  )
  df_init$partition <- parts_init

  df_rec <- data.frame(
    prior    = raw$priors,
    n_groups = raw$n_rec,
    stringsAsFactors = FALSE
  )
  df_rec$partition <- parts_rec

  # Distance matrix with rownames/colnames
  dm <- raw$dist_matrix
  rownames(dm) <- colnames(dm) <- taxa

  structure(
    list(
      initial     = df_init,
      recursive   = df_rec,
      dist_matrix = dm,
      taxa        = taxa
    ),
    class = "abgd"
  )
}


# =============================================================================
# BEST PARTITION -- S3 generic + abgd method
# =============================================================================

#' Select the Best Partition from a Delimitation Result
#'
#' Generic function to extract the most informative partition from an ABGD or
#' ASAP result object.
#'
#' @param x A delimitation result object (\code{"abgd"} or \code{"asap"}).
#' @param ... Additional arguments passed to methods.
#' @return A named list describing the selected partition. See method
#'   documentation for details.
#' @export
best_partition <- function(x, ...) UseMethod("best_partition")

#' @rdname best_partition
#' @param pass Which pass to use: \code{"recursive"} (default) or
#'   \code{"initial"}.
#' @examples
#' \dontrun{
#' result <- abgd("geophagus.fasta", model = "simple")
#' best   <- best_partition(result)
#' best$n_groups
#' }
#' @export
best_partition.abgd <- function(x, pass = c("recursive", "initial"), ...) {
  pass <- match.arg(pass)
  df   <- if (pass == "recursive") x$recursive else x$initial

  df_inf <- df[df$n_groups > 1L, ]
  if (nrow(df_inf) == 0L)
    stop("No partition with more than 1 group was found.")

  rle_res <- rle(df_inf$n_groups)
  ends    <- cumsum(rle_res$lengths)
  starts  <- ends - rle_res$lengths + 1L

  plateau_df <- data.frame(
    n_groups       = rle_res$values,
    plateau_length = rle_res$lengths,
    prior_min      = df_inf$prior[starts],
    prior_max      = df_inf$prior[ends],
    stringsAsFactors = FALSE
  )
  plateau_df <- plateau_df[order(-plateau_df$plateau_length,
                                  plateau_df$prior_min), ]

  best_run   <- which.max(rle_res$lengths)
  best_start <- starts[best_run]

  list(
    n_groups       = rle_res$values[best_run],
    prior_range    = c(df_inf$prior[starts[best_run]],
                       df_inf$prior[ends[best_run]]),
    plateau_length = rle_res$lengths[best_run],
    partition      = df_inf$partition[[best_start]],
    summary        = plateau_df
  )
}
