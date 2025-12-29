#' Turns an ultrametric phylogenetic tree into a tibble of bGMYC partitions
#' A Command-Line Interface for bGMYC - Bayesian Generalized Mixed Yule Coalescent
#'
#' @description
#' `bgmyc_tbl()` takes a Nexus format ultrametric phylogenetic tree, 
#' generates partitions using [bgmyc.singlephy][bGMYC::bgmyc.singlephy], and 
#' processes the output into an object of class [tbl_df][tibble::tbl_df].
#'
#' @param infile An ultrametric phylogenetic tree (Nexus tree file from BEAST2).
#' @param ppcutoff Posterior probability threshold for clustering samples into
#' species partitions. See [bgmyc.point][bGMYC::bgmyc.point] for details. Default to 0.05.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'bgmyc'.
#'
#' @details
#' `bgmyc_tbl()` function takes an ultrametric phylogeny in a Nexus tree file 
#' format, and converts it to a [phylo][ape::as.phylo] object. 
#' The object is then fed to the `bGMYC` package which uses 
#' [spec.probmat][bGMYC::spec.probmat] to create a matrix of probability of 
#' conspecificity and [bgmyc.point][bGMYC::bgmyc.point] to split samples into a 
#' list with individuals meeting the threshold specified by `ppcutoff`. 
#' `bgmyc_tbl()` then wraps up these two functions into a single one and turns 
#' these inputs in a tibble that matches the output of all other [xxx_tbl] functions.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Pedro S. Bittencourt, Tomas Hrbek
#'
#' @source
#' Reid N.M., Carstens B.C. 2012. Phylogenetic estimation error can decrease 
#' the accuracy of species delimitation: a Bayesian implementation of the general 
#' mixed Yule-coalescent model. BMC Evolutionary Biology 12 (196).
#' DOI: 10.1186/1471-2148-12-196
#' 
#' @examples
#' 
#'\donttest{
#' # get path to ultrametric tree
#' path_to_file <- system.file("extdata/geophagus_beast.nex", package = "delimtools")
#' 
#' # run bGMYC and create a tibble
#' bgmyc_df <- bgmyc_tbl(infile = path_to_file)
#'
#' # check
#' bgmyc_df
#'}
#'
#' @export
bgmyc_tbl <- function(infile, mcmc = 11000, burnin = 1000, thinning = 100, ppcutoff = 0.05, delimname = "bgmyc") {
  
  # infile checks
  if (file.exists(infile)) {
    lines <- readLines(infile, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    lines <- lines[!grepl("^\\[.*\\]$", lines)]  # drop pure NEXUS comments
    if (length(lines) == 0) {
      cli::cli_abort("Phylogenetic tree file is empty or contains only comments.")
    }
    if (grepl("^#NEXUS", toupper(lines[1])) && grepl("^END;$", toupper(lines[length(lines)]))) {
      tr <- ape::read.nexus(infile)
      # check if tree is unrooted; should never happen, but ...
      if (!ape::is.rooted(tr)) {
        cli::cli_abort("Phylogenetic tree is not rooted.")
      }
      # check if tree is ultrametric
      if (!ape::is.ultrametric(tr)) {
        cli::cli_abort("Phylogenetic tree is not ultrametric.")
      }
    } else if (grepl("^\\(", lines[1]) && grepl(";$", lines[length(lines)])) {
      tr <- ape::read.tree(infile)
      # check if tree is unrooted; should never happen, but ...
      if (!ape::is.rooted(tr)) {
        cli::cli_abort("Phylogenetic tree is not rooted.")
      }
      # check if tree is ultrametric
      if (!ape::is.ultrametric(tr)) {
        cli::cli_abort("Phylogenetic tree is not ultrametric.")
      }
    } else {
      cli::cli_abort("Improperly formatted Newick or Nexus tree file.")
    }
  }
  
  # check if `bGMYC` is installed
  rlang::check_installed(pkg= "bGMYC", reason= "to run `bgmyc_tbl` properly.")
  
  dname <- rlang::sym(delimname)
  
  splist <- bGMYC::bgmyc.singlephy(tr, mcmc = mcmc,
                                  burnin = burnin,
                                  thinning = thinning,
                                  t1 = 2,
                                  t2 = ape::Ntip(tr),
                                  start = c(1, 0.5, 50)) |>
    bGMYC::spec.probmat() |>
    bGMYC::bgmyc.point(ppcutoff)

    bgmyc_tbl <- tibble::tibble(
      labels = unlist(splist),
      !!dname := rep(
        seq_along(splist),
        sapply(splist, length)
      )
    )
    
    return(bgmyc_tbl)
}
