#' Plot Phylogenetic Trees With Species Delimitation Partitions
#'
#' @description
#' \code{delim_autoplot()} returns a phylogenetic tree plotted using
#' \code{\link[ggtree]{ggtree}} alongside with a customized heatmap using
#' \code{\link[ggplot2]{geom_tile}} and \code{\link[ggtreeExtra]{geom_fruit}}.
#'
#' @param delim Output from \code{\link[delimtools]{delim_join}}.
#' @param tr A \code{\link[tidytree]{treedata-class}} object.
#' @param consensus Logical. Should the majority-vote consensus to be estimated?
#' @param n_match An Integer. If \code{consensus = TRUE}, threshold for majority-vote
#' calculations. See \code{\link[delimtools]{delim_consensus}} for details.
#' @param delim_order A character vector of species delimitation names ordered by user. Default to NULL.
#' @param tbl_labs A \code{\link[tibble]{tbl_df}} of customized labels for tree plotting. The
#' first column must match tip labels of the \code{tr} object.
#' @param col_vec A color vector for species delimitation partitions.
#' @param width Numeric. Width of species delimitations bars.
#' @param pwidth Numeric. The width of the \code{\link[ggplot2]{geom_tile}}. Default to 0.2,
#' meaning 0.2 times of x range of tree.
#' @param offset Numeric. Distance between \code{tr} and \code{\link[ggplot2]{geom_tile}}.
#' Default to 0.03, meaning 0.03 times of x range of tree.
#' See \code{\link[ggtreeExtra]{geom_fruit}} for details.
#' @param consensus_offset Numeric. Distance of the consensus \code{\link[ggplot2]{geom_tile}} to the
#' species delimitation plot. Default to 0.03, meaning 0.03 times of x range of tree.
#' @param ... Aditional parameters for \code{\link[ggtreeExtra]{geom_fruit}} (to be implemented).
#'
#' @details
#' \code{delim_autoplot()} is a wrapper for tree plotting with associated data implemented
#' using \code{\link[ggtree]{ggtree}} and \code{\link[ggtreeExtra]{geom_fruit}}. If
#' \code{consensus = TRUE}, a consensus bar will be plotted next to the species delimitation plot,
#' summarising partitions across samples. If no consensus is reached, an "X" will be plotted instead. Currently,
#' this functions works only with phylogenetic trees obtained using BEAST 2 software.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @import ggtree
#' @import ggplot2
#' @import ggtreeExtra
#' @import dplyr
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_inorder
#' @importFrom ggfun xrange
#'
#'@export
delim_autoplot <- function(delim, tr, consensus=TRUE, n_match= NULL,
                           delim_order= NULL, tbl_labs, col_vec= NULL,
                           width= 1, pwidth= 0.3, offset= 0.3, consensus_offset= 0.03){

  if(is(tr, "treedata") & consensus == TRUE){

    # reorder and turn into long format
    delim_long <- delim %>%
      dplyr::arrange(match(labels, tr@phylo$tip.label)) %>%
      delim_consensus(., n_match) %>%
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) %>%
      tidyr::pivot_longer(cols= -labels,
                          names_to = "method",
                          values_to = "spp",
                          cols_vary = "slowest") %>%
        dplyr::mutate(method= as.factor(method) %>% forcats::fct_inorder(),
                      spp= as.factor(spp) %>% forcats::fct_inorder())

    # tree plot
    p <- ggtree::ggtree(tr, ladderize = TRUE, color= "grey50", size=1) %<+% tbl_labs

    if(ggfun::xrange(p)[2] <= 1) {
      width <- max(p$data$x)/10 * width
      offset <- max(p$data$x) + (max(p$data$x) * offset)
      pwidth <- max(p$data$x) + (max(p$data$x) * pwidth)
      consensus_offset <- offset/10 * consensus_offset
    }

    pp <- p+
      ggtree::geom_treescale(color="grey50", linesize=1, fontsize=3)+
      ggtree::geom_tiplab(aes(label=labs), size=3.5)+
      ggtree::geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.95))+
      ggtreeExtra::geom_fruit(data= (delim_long %>% dplyr::filter(method != "consensus")),
                              geom= geom_tile,
                              mapping = aes(x= method, y= labels, color= spp, fill= spp),
                              show.legend=FALSE,
                              width= width,
                              height= 1,
                              pwidth = pwidth,
                              offset= offset,
                              axis.params = list(axis="x",
                                                 text.size=2,
                                                 line.color= "transparent"))+
      ggtreeExtra::geom_fruit(data= (delim_long %>% dplyr::filter(method == "consensus")),
                              geom= geom_tile,
                              mapping = aes(x= method, y= labels, color= spp, fill= spp),
                              show.legend=FALSE,
                              width= width,
                              height= 1,
                              pwidth = 0.2,
                              offset= consensus_offset,
                              axis.params = list(axis="x",
                                                 text.size=2,
                                                 line.color= "transparent"))
    if(!is_null(col_vec)){
      pp <- pp +
        ggplot2::scale_fill_manual(values= col_vec, na.value= "transparent")+
        ggplot2::scale_color_manual(values= col_vec, na.value= "transparent")

        }

    if(sum(is.na(delim_long$spp)) > 0){
    pp <- pp + ggtreeExtra::geom_fruit(data= subset(delim_long, is.na(spp)),
                                       geom= geom_point,
                                       mapping= aes(x= method, y= labels),
                                       shape=4,
                                       color="black",
                                       offset = 0)
    }

  } else if(is(tr, "treedata") & consensus == FALSE){

    # reorder and turn into long format
    delim_long <- delim %>%
      dplyr::arrange(match(labels, tr@phylo$tip.label)) %>%
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) %>%
      tidyr::pivot_longer(cols= -labels,
                          names_to = "method",
                          values_to = "spp",
                          cols_vary = "slowest") %>%
      dplyr::mutate(method= as.factor(method) %>% forcats::fct_inorder(),
                    spp= as.factor(spp) %>% forcats::fct_inorder())

    # tree plot
    p <- ggtree::ggtree(tr, ladderize = TRUE, color= "grey50", size=1) %<+% tbl_labs

    if(ggfun::xrange(p)[2] <= 1) {
      width <- max(p$data$x)/10 * width
      offset <- max(p$data$x) + (max(p$data$x) * offset)
      pwidth <- max(p$data$x) + (max(p$data$x) * pwidth)
      consensus_offset <- offset/10 * consensus_offset
    }

    pp <- p+
      geom_treescale(color="grey50", linesize=1, fontsize=3)+
      geom_tiplab(aes(label=labs), size=3.5)+
      geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.95))+
      ggtreeExtra::geom_fruit(data= delim_long,
                              geom= geom_tile,
                              mapping = aes(x= method, y= labels, color= spp, fill= spp),
                              show.legend=FALSE,
                              width= width,
                              height= 1,
                              pwidth = pwidth,
                              offset= offset,
                              axis.params = list(axis="x",
                                                 text.size=2,
                                                 line.color= "transparent"))+
      ggplot2::scale_fill_manual(values= col_vec, na.value= "transparent")+
      ggplot2::scale_color_manual(values= col_vec, na.value= "transparent")
    }
  return(pp)
}
