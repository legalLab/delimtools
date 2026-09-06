#' Helper Functions for Species Delimitation Analysis
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggtree %<+%
#' @importFrom methods is
#' @importFrom rlang := .data
#' @importFrom ape branching.times is.ultrametric is.binary dist.dna read.dna
#' @importFrom stats optim pchisq setNames rpois
#' @useDynLib delimtools, .registration = TRUE
## usethis namespace: end
NULL

## C symbols created by useDynLib(delimtools, .registration=TRUE)
## Suppress R CMD check NOTE "no visible binding for global variable"
utils::globalVariables(c(
  "abgd_run_call",
  "asap_run_call",
  "asap_dist_call",
  "C_gmyc",
  "C_gmyc_setup",
  "C_gmyc_loglik",
  "C_gmyc_loglik_params",
  "C_gmyc_clusters",
  "C_gmyc_threshold_data",
  "rmptp_ml",
  "rmptp_mcmc"
))

#' @description
#' \bold{delimtools} contains helpers functions to process, analyse, and plot the output
#' of single locus species delimitation methods.
#' The main purpose of this package is to facilitate some work routines for the average
#' R user, from the organization of data to plotting trees and associated data as
#' easily as possible.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins, Tomas Hrbek.
