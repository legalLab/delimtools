#' Plot Phylogenetic Trees With Species Delimitation Partitions
#'
#' @description
#' \code{delim_autoplot2()} returns a phylogenetic tree plotted using
#' \code{\link[ggtree]{ggtree}} alongside with a customized tile plot using
#' \code{\link[ggplot2]{geom_tile}} and \code{\link[patchwork]{wrap_plots}}.
#'
#' @param delim Output from \code{\link[delimtools]{delim_join}}.
#' @param tr A \code{\link[tidytree]{treedata-class}} object. Both phylogram and
#' ultrametric trees are supported.
#' @param consensus Logical. Should the majority-vote consensus to be estimated?
#' @param n_match An Integer. If \code{consensus = TRUE}, threshold for majority-vote
#' calculations. See \code{\link[delimtools]{delim_consensus}} for details.
#' @param delim_order A character vector of species delimitation names ordered by user.
#' Default to NULL.
#' @param tbl_labs A \code{\link[tibble]{tbl_df}} of customized labels for tree plotting. The
#' first column must match tip labels of the \code{tr} object, while the second column
#' should have customized labels, and third column must have species names for each tip of the tree.
#' @param species column name in \code{tbl_labs} which contains species names for each tip of the tree.
#' @param widths A numeric vector containing the relative widths of the tree and
#' species delimitation bars. See \code{\link[patchwork]{wrap_plots}} for details.
#' Defaults to \code{c(0.5, 0.2)}.
#' @param hexpand Numeric. Expand xlim of tree by a ratio of x axis range. Useful if
#' tiplabels become truncated when plotting. Default to \code{0.1}.
#'
#' @details
#' \code{delim_autoplot2()} is a wrapper for tree plotting with associated data implemented
#' using \code{\link[ggtree]{ggtree}} and \code{\link[patchwork]{wrap_plots}}. If
#' \code{consensus = TRUE}, a consensus bar will be plotted next to the species delimitation plot,
#' summarizing partitions across samples. If no consensus is reached, an "X" will be plotted instead.
#' This function is a modified version of \code{\link[delimtools]{delim_autoplot}} which plots 
#' species partitions using a black and grey color scheme.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#' 
#' @import dplyr
#' @import ggtree
#' @import ggplot2
#' @importFrom ape is.ultrametric
#' @importFrom cli cli_abort cli_warn
#' @importFrom delimtools delim_brewer
#' @importFrom forcats fct_inorder fct_rev
#' @importFrom ggfun xrange yrange
#' @importFrom methods is
#' @importFrom patchwork wrap_plots
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer 
#' @importFrom tidyselect all_of
#' 
#' @export
#' 
delim_autoplot2 <- function(delim, tr, consensus=TRUE, n_match= NULL,
                            delim_order= NULL, tbl_labs, species,
                            hexpand= 0.1, widths= c(0.5, 0.2)){
  
  if(!is(tr, "treedata")){
    
    cli::cli_abort(c("Tree file must be from class {.cls treedata}.",
                     "i"= "You've supplied a tree file of class {.cls {class(tr)}}",
                     "i"= "You may convert your tree file by using {.fun tidytree::as.treedata}"))
    
  }
  
  if(is.null(tbl_labs)) {
    
    cli::cli_abort("Argument {.arg tbl_labs} not provided. Please provide one or use {.fn delim_autoplot} instead.")
    
  }
  
  if(is.null(species)) {
    
    cli::cli_abort("Argument {.arg species} not provided. Please provide one or use {.fn delim_autoplot} instead.")
    
  }
  
  if(ape::is.ultrametric(tr@phylo)) {
    
    p <- ggtree::ggtree(tr, ladderize = TRUE, color= "grey50", size=1) %<+% tbl_labs
    
    pp <- p+
      ggtree::hexpand(ratio= hexpand)+
      ggtree::geom_tiplab(aes(label= get(colnames(tbl_labs)[2])), size=3.5)+
      ggtree::geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.95))
    
  } else {
    
    p <- ggtree::ggtree(tr, ladderize = TRUE, color= "grey50", size=1) %<+% tbl_labs
    
    pp <- p+
      ggtree::hexpand(ratio= hexpand)+
      ggtree::geom_tiplab(aes(label= get(colnames(tbl_labs)[2])), size=3.5, align = TRUE)+
      ggtree::geom_point2(aes(subset=!isTip & support >= 75))
  }
  
  if(is.null(delim_order)){
    delim_order <- colnames(delim)[-1]
    
    cli::cli_warn("Argument {.arg delim_order} not provided. Using default order from {.arg delim}.")
    
  }
  
  # load rle function
  rle_method <- function(x) {
    # Compute the run-length encoding
    rle_result <- rle(x)
    
    # Create a logical vector initialized to FALSE
    separate_runs_logical <- logical(length(x))
    
    # Identify unique values
    unique_values <- unique(x)
    
    # Determine if each value appears in separate runs
    runs_count <- sapply(unique_values, function(val) {
      length(which(rle_result$values == val))
    })
    
    # Create a logical vector indicating if a value appears in separate runs
    is_separate_run <- sapply(x, function(val) {
      runs_count[val] > 1
    })
    # Print the logical vector
    return(unname(is_separate_run))
  }
  
  if(consensus == TRUE) {
    
    # reorder and turn into long format
    delim_long <- delim %>%
      dplyr::mutate(species= dplyr::pull(tbl_labs, {{ species }})[match(labels, dplyr::pull(tbl_labs, 1))]) %>%
      dplyr::arrange(match(labels, ggtree::get_taxa_name(pp))) %>%
      delimtools::delim_consensus(., n_match= n_match) %>%
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) %>%
      tidyr::pivot_longer(cols= c(-labels, -species),
                          names_to = "method",
                          values_to = "spp",
                          cols_vary = "fastest") %>%
      dplyr::mutate(method= as.factor(method) %>% forcats::fct_inorder(),
                    spp= as.factor(spp) %>% forcats::fct_inorder(),
                    label=labels,
                    labels= as.factor(labels) %>% forcats::fct_inorder() %>% forcats::fct_rev()) %>%
      dplyr::group_by(spp) %>%
      dplyr::mutate(interaction= interaction(method, species, spp)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(method) %>% 
      dplyr::arrange(labels, .by_group=TRUE) %>% 
      dplyr::mutate(is_monophyletic= !rle_method(as.character(spp))) %>%
      dplyr::ungroup()
    
    # delim plot
    delim_tile_bw <- ggplot2::ggplot(delim_long, aes(x= method, y= labels, fill= is_monophyletic, color= is_monophyletic, group= interaction))+
      ggplot2::geom_line(lineend= "round", linejoin= "round", linewidth= 5, show.legend = FALSE)+
      ggplot2::geom_point(data= subset(delim_long, !is.na(spp)), shape= 21, size= 4.2, show.legend = FALSE)+
      ggplot2::geom_point(data= subset(delim_long, is.na(spp)), shape=4, color= "black", show.legend = FALSE)+
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add= c(0.5,0.5)))+
      ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add= c(0.5,0.5)))+
      ggplot2::scale_fill_manual(values= c("grey80", "black"), na.value= "transparent")+
      ggplot2::scale_colour_manual(values= c("grey80", "black"), na.value= "transparent")+
      ggplot2::theme(axis.title = element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(color= "black", size= 10),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
    
    # rescale tree plot
    pp2 <- pp+
      ggplot2::coord_cartesian(xlim= ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile_bw), expand = FALSE)+
      ggtree::geom_treescale(color="grey50", linesize=1, fontsize=3, y= ggfun::yrange(delim_tile_bw)[1])
    
  } else {
    
    delim_long <- delim %>%
      dplyr::mutate(species= pull(tbl_labs, {{ species }})[match(labels, pull(tbl_labs, 1))]) %>%
      dplyr::arrange(match(labels, ggtree::get_taxa_name(pp))) %>%
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) %>%
      tidyr::pivot_longer(cols= c(-labels, -species),
                          names_to = "method",
                          values_to = "spp",
                          cols_vary = "fastest") %>%
      dplyr::mutate(method= as.factor(method) %>% forcats::fct_inorder(),
                    spp= as.factor(spp) %>% forcats::fct_inorder(),
                    label=labels,
                    labels= as.factor(labels) %>% forcats::fct_inorder() %>% forcats::fct_rev()) %>%
      dplyr::group_by(spp) %>%
      dplyr::mutate(interaction= interaction(method,spp)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(method) %>% 
      dplyr::arrange(labels, .by_group=TRUE) %>% 
      dplyr::mutate(is_monophyletic= !rle_method(as.character(spp))) %>%
      dplyr::ungroup()
    
    # delim plot
    delim_tile_bw <- ggplot2::ggplot(delim_long, aes(x= method, y= labels, fill= is_monophyletic, color= is_monophyletic, group= interaction))+
      ggplot2::geom_line(lineend= "round", linejoin= "round", linewidth= 5, show.legend = FALSE)+
      ggplot2::geom_point(shape= 21, size= 4.2, show.legend = FALSE)+
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add= c(0.5,0.5)))+
      ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add= c(0.5,0.5)))+
      ggplot2::scale_fill_manual(values= c("grey80", "black"), na.value= "transparent")+
      ggplot2::scale_colour_manual(values= c("grey80", "black"), na.value= "transparent")+
      ggplot2::theme(axis.title = element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(color= "black", size= 10),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
    
    # rescale tree plot
    pp2 <- pp+
      ggplot2::coord_cartesian(xlim= ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile_bw), expand = FALSE)+
      ggtree::geom_treescale(color="grey50", linesize=1, fontsize=3, y= ggfun::yrange(delim_tile_bw)[1])
    
  }
  
  x <- patchwork::wrap_plots(pp2, delim_tile_bw, widths = widths)
  
  return(x)
}
