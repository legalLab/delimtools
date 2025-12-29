#' Turns an ultrametric phylogenetic tree into a tibble of GMYC partitions
#' A Command-Line Interface for GMYC - Generalized Mixed Yule Coalescent
#'
#' @description
#' `gmyc_tbl()` processes a Nexus format ultrametric phylogenetic tree, 
#' generates partitions using [gmyc][splits::gmyc] and [spec.list][splits::spec.list], 
#' and processes the output into an object of class [tbl_df][tibble::tbl_df].
#'
#' @param infile An ultrametric phylogenetic tree (Nexus tree file from BEAST2).
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'gmyc'.
#'
#' @details
#' `gmyc_tbl()` function takes an ultrametric phylogeny in a Nexus tree file 
#' format, and converts it to a [phylo][ape::as.phylo] object. 
#' The object is then fed to the `splits` package which uses [gmyc][splits::gmyc] 
#' to optimize genetic clusters and [spec.list][splits::spec.list] to cluster 
#' samples into species partitions. 
#' `gmyc_tbl()` turns these results into a tibble that matches the output of all other [xxx_tbl] functions.
#'
#' @return
#' An object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Pedro S. Bittencourt, Tomas Hrbek
#' 
#' @source
#' Pons J., Barraclough T. G., Gomez-Zurita J., Cardoso A., Duran D. P., Hazell S., 
#' Kamoun S., Sumlin W. D., Vogler A. P. 2006. Sequence-based species delimitation for 
#' the DNA taxonomy of undescribed insects. Systematic Biology. 55:595-609.
#' DOI: 10.1080/10635150600852011
#' 
#' Monaghan M. T., Wild R., Elliot M., Fujisawa T., Balke M., Inward D. J. G., Lees D. C., Ranaivosolo R.,
#' Eggleton P., Barraclough T. G., Vogler A. P. 2009. Accelerated species inventory 
#' on Madagascar using coalescent-based models of species delineation. 
#' Systematic Biology. 58:298-311.
#' DOI: 10.1093/sysbio/syp027
#' 
#' Fujisawa T., Barraclough T. G. 2013. Delimiting species using single-locus data 
#' and the generalized mixed Yule coalescent approach: A revised method and evaluation 
#' on simulated data sets. Systematic Biology. 62(5):707–724.
#' DOI: 10.1093/sysbio/syt033
#'
#' @examples
#' 
#' \donttest{
#' # get path to ultrametric tree
#' path_to_file <- system.file("extdata/geophagus_beast.nex", package = "delimtools")
#' 
#' # run GMYC and create a tibble
#' gmyc_df <- gmyc_tbl(infile = path_to_file)
#' 
#' # check
#' gmyc_df
#'}
#'
#' @export
gmyc_tbl <- function(infile, delimname = "gmyc"){
  
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
 
  # check if `splits` is installed
  rlang::check_installed("splits", reason = "to run `gmyc_tbl` properly.")

  dname <- rlang::sym(delimname)
  
  gmyc_spec <- splits::gmyc(tr, method = "single") |>
    splits::spec.list()

  gmyc_tbl <- tibble::tibble(labels = as.character(gmyc_spec$sample_name),
                             !!dname := as.integer(gmyc_spec$GMYC_spec))
  
  return(gmyc_tbl)
}
