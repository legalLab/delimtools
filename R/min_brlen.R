#' A function to report the smallest tip-to-tip distances in a phylogenetic tree
#'
#' @description
#' \code{min_brlen()} returns a table of smallest tip-to-tip distances in a phylogenetic tree.
#'
#' @param tree A path to tree file in Newick format, or a phylogenetic tree object of class "phylo".
#' @param n Number of distances to report (default = 5).
#' @param print Logical of whether to print the result to screen (default = TRUE).
#'
#' @details
#' \code{min_brlen()} tabulates the smallest tip-to-tip distances in a phylogenetic tree
#' using \code{ape::cophenetic.phylo()} and prints a table to screen.
#' This is useful when excluding identical or near-identical haplotypes
#' using the '--minbr' parameter in mPTP.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}
#'
#' @author
#' Rupert A. Collins
#'
#' @importFrom tibble as_tibble
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom ape read.tree cophenetic.phylo
#' @importFrom knitr kable
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter arrange slice_head count
#'
#'
#' @export

min_brlen <- function(tree, n = 5, print = TRUE) {

  # checks

  if(class(tree) == "phylo") {
    
    tr <- tree

  } else if(file.exists(tree)) {

    tr <- ape::read.tree(tree)

  } else {

    writeLines("\n")

    cli::cli_abort("Error. Please provide a phylogenetic tree object or a path to a Newick file that can be read by `ape`.")

    writeLines("\n")

  }

  # fun

  tr.dist <- ape::cophenetic.phylo(tr)
  
  pairs.tab <- tr.dist |> 
    tibble::as_tibble(rownames="tip1") |> 
    tidyr::pivot_longer(-1,names_to="tip2",values_to="dist") |> 
    dplyr::filter(tip1!=tip2)
  
  pairs.tab.cut <- pairs.tab |> 
    dplyr::arrange(dist) |> 
    dplyr::count(dist) |> 
    dplyr::slice_head(n = n) 
  
  # print

  if(isTRUE(print)) {
    
      writeLines("\n")

      cli::cli_alert_info("Printing {n} smallest tip-to-tip distances in a tree with {length(tr$tip.label)} tips ...")

      pairs.tab.cut |> knitr::kable(align="lr") |> print()

  }

  invisible(pairs.tab.cut)

}
