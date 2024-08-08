#' Plot Phylogenetic Trees With Species Delimitation Partitions
#'
#' @description
#' \code{delim_autoplot()} returns a phylogenetic tree plotted using
#' \code{\link[ggtree]{ggtree}} alongside with a customized heatmap using
#' \code{\link[ggplot2]{geom_tile}} and \code{\link[ggtreeExtra]{geom_fruit}}.
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
#' should have customized labels.
#' @param col_vec A color vector for species delimitation partitions.
#' @param widths A numeric vector containing the relative widths of the tree and
#' species delimitation bars. See \code{\link[patchwork]{wrap_plots}} for details.
#' Defaults to \code{c(0.5, 0.2)}.
#' @param hexpand Numeric. Expand xlim of tree by a ratio of x axis range. Useful if
#' tiplabels become truncated when plotting. Default to \code{0.1}.
#'
#' @details
#' \code{delim_autoplot()} is a wrapper for tree plotting with associated data implemented
#' using \code{\link[ggtree]{ggtree}} and \code{\link[patchwork]{wrap_plots}}. If
#' \code{consensus = TRUE}, a consensus bar will be plotted next to the species delimitation plot,
#' summarizing partitions across samples. If no consensus is reached, an "X" will be plotted instead.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.

delim_autoplot2 <- function(delim, tr, consensus=TRUE, n_match= NULL,
                            delim_order= NULL, tbl_labs= NULL, col_vec= NULL,
                            hexpand= 0.1, widths= c(0.5, 0.2)){
  if(!is(tr, "treedata")){

    cli::cli_abort(c("Tree file must be from class {.cls treedata}.",
                     "i"= "You've supplied a tree file of class {.cls {class(tr)}}",
                     "i"= "You may convert your tree file by using {.fun tidytree::as.treedata}"))

  }

  if(is.null(tbl_labs)) {

    tbl_labs <- tibble::tibble(label= tidytree::tip.label(tr),
                               labs= label)

    cli::cli_warn("Argument {.arg tbl_labs} not provided. Using tiplabels instead.")

  }

  if(ape::is.ultrametric(tr@phylo)) {

    p <- ggtree(tr, ladderize = TRUE, color= "grey50", size=1) %<+% tbl_labs

    pp <- p+
      hexpand(ratio= hexpand)+
      geom_tiplab(aes(label= get(colnames(ftab)[2])), size=3.5)+
      geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.95))

  } else {

    p <- ggtree(tr, ladderize = TRUE, color= "grey50", size=1) %<+% tbl_labs

    pp <- p+
      hexpand(ratio= hexpand)+
      geom_tiplab(aes(label= get(colnames(ftab)[2])), size=3.5, align = TRUE)+
      geom_point2(aes(subset=!isTip & support >= 75))
  }

  if(is.null(delim_order)){
    delim_order <- colnames(delim)[-1]

    cli::cli_warn("Argument {.arg delim_order} not provided. Using default order from {.arg delim}.")

  }

  if(is.null(col_vec)){
    col_vec <- delimtools::delim_brewer(delim, "Set1", 9)

    cli::cli_warn("Argument {.arg col_vec} not provided.
                  Customizing one using {.fn delimtools::delim_brewer}.")

  }

  if(consensus == TRUE) {

    # reorder and turn into long format
    delim_long <- delim %>%
      dplyr::arrange(match(labels, ggtree::get_taxa_name(pp))) %>%
      delimtools::delim_consensus(., n_match= n_match) %>%
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) %>%
      tidyr::pivot_longer(cols= -labels,
                          names_to = "method",
                          values_to = "spp",
                          cols_vary = "fastest") %>%
      dplyr::mutate(method= as.factor(method) %>% forcats::fct_inorder(),
                    spp= as.factor(spp) %>% forcats::fct_inorder(),
                    label=labels,
                    labels= as.factor(labels) %>% forcats::fct_inorder() %>% forcats::fct_rev())

    delim_tile <- ggplot(delim_long, aes(x= method, y= labels, color= spp, fill= spp))+
      geom_tile(data= subset(delim_long, !is.na(spp)), width= 0.5,show.legend = FALSE)+
      geom_point(data= subset(delim_long, is.na(spp)), shape=4, color= "black", show.legend = FALSE)+
      scale_y_discrete(expand = expansion(add= c(0,0)))+
      scale_x_discrete(position = "top", expand = expansion(add= c(0,0.5)))+
      ggplot2::scale_fill_manual(values= col_vec, na.value= "transparent")+
      ggplot2::scale_color_manual(values= col_vec, na.value= "transparent")+
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(color= "black", size= 10),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    pp2 <- pp +
      coord_cartesian(xlim= ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile), expand = FALSE)+
      geom_treescale(color="grey50", linesize=1, fontsize=3, y= ggfun::yrange(delim_tile)[1])

  } else {
    delim_long <- delim %>%
      dplyr::arrange(match(labels, ggtree::get_taxa_name(pp))) %>%
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) %>%
      tidyr::pivot_longer(cols= -labels,
                          names_to = "method",
                          values_to = "spp",
                          cols_vary = "fastest") %>%
      dplyr::mutate(method= as.factor(method) %>% forcats::fct_inorder(),
                    spp= as.factor(spp) %>% forcats::fct_inorder(),
                    label=labels,
                    labels= as.factor(labels) %>% forcats::fct_inorder() %>% forcats::fct_rev())

    delim_tile <- ggplot(delim_long, aes(x= method, y= labels, color= spp, fill= spp))+
      geom_tile(data= subset(delim_long, !is.na(spp)), width= 0.5,show.legend = FALSE)+
      scale_y_discrete(expand = expansion(add= c(0,0)))+
      scale_x_discrete(position = "top", expand = expansion(add= c(0,0.5)))+
      ggplot2::scale_fill_manual(values= col_vec, na.value= "transparent")+
      ggplot2::scale_color_manual(values= col_vec, na.value= "transparent")+
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(color= "black", size= 10),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    pp2 <- pp +
      coord_cartesian(xlim= ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile), expand = FALSE)+
      geom_treescale(color="grey50", linesize=1, fontsize=3, y= ggfun::yrange(delim_tile)[1])

  }

  x <- patchwork::wrap_plots(pp2, delim_tile, widths = widths)

  return(x)
}






