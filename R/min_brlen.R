#' A function to report the smallest tip-to-tip distances in a phylogenetic tree
#'
#' @description
#' `min_brlen()` returns a table of smallest tip-to-tip distances in a phylogenetic tree.
#'
#' @param tree A path to tree file in Newick format, or a phylogenetic tree object of class [phylo][ape::phylo].
#' @param n Number of distances to report (default = 5).
#' @param verbose Logical of whether to print the result to screen (default = TRUE).
#'
#' @details
#' `min_brlen()` tabulates the smallest tip-to-tip distances in a phylogenetic tree
#' using [cophenetic.phylo][ape::cophenetic.phylo] and prints a table to screen.
#' This is useful when excluding identical or near-identical haplotypes
#' using the '--minbr' parameter in mPTP.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df]
#'
#' @author
#' Rupert A. Collins, Tomas Hrbek
#'
#' @examples
#' # estimate minimum branch length from Newick or Nexus tree
#' # also accepts trees in 'phylo' or 'tidytree' formats
#' min_brlen(ape::as.phylo(geophagus_raxml), n = 5)
#' min_brlen(geophagus_raxml, n = 5)
#' min_brlen("geophagus_raxml.nwk", n = 5)
#' min_brlen(ape::as.phylo(geophagus_beast), n = 5)
#' min_brlen(geophagus_beast, n = 5)
#' min_brlen("geophagus_beast.nex", n = 5)
#'
#' @export
min_brlen <- function(infile, n = 5, verbose = TRUE) {
  
  # checks
  if (methods::is(infile, "phylo")) {
    tr <- infile
  } else if (methods::is(infile, "treedata")) {
    tr <- ape::as.phylo(infile)
  } else if (file.exists(infile)) {
    lines <- readLines(infile, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    lines <- lines[!grepl("^\\[.*\\]$", lines)]  # drop pure NEXUS comments
    if (length(lines) == 0) {
      cli::cli_abort("Phylogenetic tree file is empty or contains only comments.")
    }
    if (grepl("^#NEXUS", toupper(lines[1])) && grepl("^END;$", toupper(lines[length(lines)]))) {
      tr <- ape::read.nexus(infile)
    } else if (grepl("^\\(", lines[1]) && grepl(";$", lines[length(lines)])) {
      tr <- ape::read.tree(infile)
    } else {
      cli::cli_abort("Infile is improperly formatted Newick or Nexus tree file.")
    }
  } else {
    cli::cli_abort("Please provide a phylogenetic tree object or a path to a Newick/Nexus tree file that can be read by `ape`.")
  }

  # fun
  tr_dist <- ape::cophenetic.phylo(tr)

  pairs_tab <- tr_dist |>
    tibble::as_tibble(rownames = "tip1") |>
    tidyr::pivot_longer(-1, names_to = "tip2", values_to = "dist") |>
    dplyr::filter(tip1 != tip2)

  pairs_tab_cut <- pairs_tab |>
    dplyr::arrange(dist) |>
    dplyr::count(dist) |>
    dplyr::slice_head(n = n)

  # print
  if(verbose == TRUE) {
    cli::cli_alert_info("Printing {n} smallest tip-to-tip distances in a tree with {length(tr$tip.label)} tips ...")

    pairs_tab_cut |>
      knitr::kable(align = "lr") |>
      print()
  }

  return(invisible(pairs_tab_cut))
}
